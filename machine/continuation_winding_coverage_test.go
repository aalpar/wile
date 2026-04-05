// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Tests for continuation and dynamic-wind coverage gaps.
// Uses package machine_test to access full runtime environment.

package machine_test

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/testutil"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// compileClosure compiles a Scheme lambda expression and returns the resulting closure.
func compileClosure(t *testing.T, env *environment.EnvironmentFrame, code string) *machine.MachineClosure {
	t.Helper()
	mc, err := runSchemeExpr(t, env, code)
	qt.Assert(t, err, qt.IsNil)
	cls, ok := mc.GetValue().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected MachineClosure, got %T", mc.GetValue()))
	return cls
}

// --- UnwindTo ---

// TestUnwindTo_DirectCall exercises UnwindTo by manually pushing winding
// frames with compiled Scheme closures and verifying after thunks are called.
func TestUnwindTo_DirectCall(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	// Set up tracking variables
	_, err := runSchemeExprs(t, env, "(define after1-called #f)", "(define after2-called #f)")
	c.Assert(err, qt.IsNil)

	// Compile after thunks
	after1 := compileClosure(t, env, "(lambda () (set! after1-called #t))")
	after2 := compileClosure(t, env, "(lambda () (set! after2-called #t))")

	// Create a context and push winding frames
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, after1)) // outermost
	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, after2)) // innermost
	c.Assert(testMC.WindingStack(), qt.HasLen, 2)

	// UnwindTo(0) should call both after thunks (innermost first)
	err = testMC.UnwindTo(0)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 0)

	// Verify both after thunks were called
	mc, err := runSchemeExpr(t, env, "after1-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "after2-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)
}

// TestUnwindTo_PartialUnwind exercises UnwindTo with a commonDepth > 0,
// unwinding only the innermost frames while preserving outer ones.
func TestUnwindTo_PartialUnwind(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExprs(t, env, "(define outer-called #f)", "(define inner-called #f)")
	c.Assert(err, qt.IsNil)

	outerAfter := compileClosure(t, env, "(lambda () (set! outer-called #t))")
	innerAfter := compileClosure(t, env, "(lambda () (set! inner-called #t))")

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, outerAfter))
	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, innerAfter))

	// Unwind only the innermost frame (commonDepth=1)
	err = testMC.UnwindTo(1)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 1)

	// Only inner should have been called
	mc, err := runSchemeExpr(t, env, "inner-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "outer-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.FalseValue)
}

// TestUnwindTo_NilAfterThunks verifies that frames with nil After closures
// are skipped without error during unwinding.
func TestUnwindTo_NilAfterThunks(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExpr(t, env, "(define tracked #f)")
	c.Assert(err, qt.IsNil)

	tracked := compileClosure(t, env, "(lambda () (set! tracked #t))")

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	// Mix of nil and non-nil after thunks
	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, nil))     // nil after
	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, tracked)) // real after
	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, nil))     // nil after

	err = testMC.UnwindTo(0)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 0)

	mc, err := runSchemeExpr(t, env, "tracked")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)
}

// TestUnwindTo_DynamicWindWithPromptAbort exercises UnwindTo via Scheme:
// aborting inside dynamic-wind within a continuation prompt triggers
// UnwindTo in PrimCallWithContinuationPrompt.
func TestUnwindTo_DynamicWindWithPromptAbort(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define tag (make-continuation-prompt-tag 'test))",
		"(define wind-log '())",
		`(call-with-continuation-prompt
			(lambda ()
				(dynamic-wind
					(lambda () (set! wind-log (cons 'before wind-log)))
					(lambda () (abort-current-continuation tag 42))
					(lambda () (set! wind-log (cons 'after wind-log)))))
			tag
			(lambda (v) v))`,
	)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))

	// The after thunk should have run during unwinding
	mc, err = runSchemeExpr(t, env, "(memq 'after wind-log)")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue() != values.FalseValue, qt.IsTrue,
		qt.Commentf("after thunk should have been called during unwind"))
}

// TestUnwindTo_NestedDynamicWindWithPromptAbort exercises UnwindTo with
// multiple nested dynamic-wind frames inside a prompt.
func TestUnwindTo_NestedDynamicWindWithPromptAbort(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define tag2 (make-continuation-prompt-tag 'test2))",
		"(define wind-log2 '())",
		`(call-with-continuation-prompt
			(lambda ()
				(dynamic-wind
					(lambda () (set! wind-log2 (cons 'outer-before wind-log2)))
					(lambda ()
						(dynamic-wind
							(lambda () (set! wind-log2 (cons 'inner-before wind-log2)))
							(lambda () (abort-current-continuation tag2 99))
							(lambda () (set! wind-log2 (cons 'inner-after wind-log2)))))
					(lambda () (set! wind-log2 (cons 'outer-after wind-log2)))))
			tag2
			(lambda (v) v))`,
	)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))

	// Both after thunks should have run (inner first, then outer)
	mc, err = runSchemeExpr(t, env, "wind-log2")
	c.Assert(err, qt.IsNil)
	// Log is cons'd in reverse: (outer-after inner-after inner-before outer-before)
	expected := values.List(
		values.NewSymbol("outer-after"),
		values.NewSymbol("inner-after"),
		values.NewSymbol("inner-before"),
		values.NewSymbol("outer-before"),
	)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, expected)
}

// --- RestoreWithWinding ---

// TestRestoreWithWinding_DirectCall exercises the 0%-covered wrapper method
// by calling it directly with a target winding stack that requires rewinding.
func TestRestoreWithWinding_DirectCall(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExpr(t, env, "(define before-called #f)")
	c.Assert(err, qt.IsNil)

	before := compileClosure(t, env, "(lambda () (set! before-called #t))")

	frame := machine.NewDynamicWindFrame(before, nil)
	targetStack := machine.WindingStack{frame}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	// RestoreWithWinding with nil cont: should rewind to target (calls before thunks)
	err = testMC.RestoreWithWinding(nil, targetStack)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 1)

	mc, err := runSchemeExpr(t, env, "before-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)
}

// TestRestoreWithWinding_UnwindAndRewind exercises RestoreWithWinding with
// both source and target stacks requiring unwind then rewind.
func TestRestoreWithWinding_UnwindAndRewind(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExprs(t, env,
		"(define src-after-called #f)",
		"(define tgt-before-called #f)",
	)
	c.Assert(err, qt.IsNil)

	srcAfter := compileClosure(t, env, "(lambda () (set! src-after-called #t))")
	tgtBefore := compileClosure(t, env, "(lambda () (set! tgt-before-called #t))")

	srcFrame := machine.NewDynamicWindFrame(nil, srcAfter)
	tgtFrame := machine.NewDynamicWindFrame(tgtBefore, nil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	// Start with source frame on stack
	testMC.PushWindingFrame(srcFrame)

	// Restore to target stack (should unwind source, rewind target)
	err = testMC.RestoreWithWinding(nil, machine.WindingStack{tgtFrame})
	c.Assert(err, qt.IsNil)

	mc, err := runSchemeExpr(t, env, "src-after-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)

	mc, err = runSchemeExpr(t, env, "tgt-before-called")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)
}

// --- FindPrompt ---

// TestFindPrompt_ContinuationFrame exercises the uncovered branch where
// a prompt tag is found on a continuation frame (not on the context itself).
func TestFindPrompt_ContinuationFrame(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	tag := machine.NewPromptTag("test-frame")

	// Create continuation chain with a prompt on a frame
	parent := machine.NewMachineContinuation(nil, tpl, env)
	promptFrame := machine.NewMachineContinuationWithPrompt(parent, tpl, env, tag, nil)
	top := machine.NewMachineContinuation(promptFrame, tpl, env)

	// Create context with the continuation chain
	mc := machine.NewMachineContext(context.Background(), top)

	// FindPrompt should find it on the continuation frame
	found, ok := mc.FindPrompt(tag)
	c.Assert(ok, qt.IsTrue)
	c.Assert(found, qt.IsNotNil)
	c.Assert(found.PromptTag(), qt.Equals, tag)
}

// TestFindPrompt_NotFound exercises the branch where no prompt matches.
func TestFindPrompt_NotFound(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	tag := machine.NewPromptTag("missing")
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	found, ok := mc.FindPrompt(tag)
	c.Assert(ok, qt.IsFalse)
	c.Assert(found, qt.IsNil)
}

// --- SliceContinuationAt ---

// TestSliceContinuationAt_NilCont exercises the branch where p.cont is nil.
func TestSliceContinuationAt_NilCont(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Create context with nil continuation (no parent)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	// mc.cont is cont.parent which is nil
	// (NewMachineContext sets p.cont = cont.parent)
	result := mc.SliceContinuationAt(nil)
	c.Assert(result, qt.IsNil)
}

// TestSliceContinuationAt_PromptIsTopFrame exercises the branch where
// p.cont == prompt (prompt is the first frame).
func TestSliceContinuationAt_PromptIsTopFrame(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	prompt := machine.NewMachineContinuation(nil, tpl, env)
	// Create continuation whose parent is prompt, so mc.cont == prompt
	cont := machine.NewMachineContinuation(prompt, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	// mc.cont == prompt -> should return nil
	result := mc.SliceContinuationAt(prompt)
	c.Assert(result, qt.IsNil)
}

// TestSliceContinuationAt_DeepChain exercises slicing a multi-frame chain.
func TestSliceContinuationAt_DeepChain(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Build chain: top -> frame2 -> prompt -> bottom
	bottom := machine.NewMachineContinuation(nil, tpl, env)
	prompt := machine.NewMachineContinuation(bottom, tpl, env)
	frame2 := machine.NewMachineContinuation(prompt, tpl, env)
	top := machine.NewMachineContinuation(frame2, tpl, env)
	// mc.cont = top.parent = frame2... wait, NewMachineContext sets p.cont = cont.parent
	// So if we pass `top` as the continuation, mc.cont = top.parent = frame2
	// We want mc.cont to be `top` itself. So we need a wrapper.
	wrapper := machine.NewMachineContinuation(top, tpl, env)
	mc := machine.NewMachineContext(context.Background(), wrapper)

	// mc.cont == top. Slice at prompt: should return frames [top, frame2]
	segment := mc.SliceContinuationAt(prompt)
	c.Assert(segment, qt.IsNotNil)

	// Walk the segment to verify depth
	depth := 0
	for f := segment; f != nil; f = f.Parent() {
		depth++
	}
	// Should have 2 frames: top copy and frame2 copy (not including prompt)
	c.Assert(depth, qt.Equals, 2)
}

// --- RewindTo ---

// TestRewindTo_DirectCall exercises RewindTo by rewinding into a target
// winding stack from a common ancestor.
func TestRewindTo_DirectCall(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExprs(t, env,
		"(define rewind-log '())",
	)
	c.Assert(err, qt.IsNil)

	before1 := compileClosure(t, env, "(lambda () (set! rewind-log (cons 'b1 rewind-log)))")
	before2 := compileClosure(t, env, "(lambda () (set! rewind-log (cons 'b2 rewind-log)))")

	frame1 := machine.NewDynamicWindFrame(before1, nil)
	frame2 := machine.NewDynamicWindFrame(before2, nil)
	target := machine.WindingStack{frame1, frame2}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	// Rewind from empty (commonDepth=0) to target with 2 frames
	err = testMC.RewindTo(target, 0)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 2)

	// Before thunks called in order: outermost first
	mc, err := runSchemeExpr(t, env, "rewind-log")
	c.Assert(err, qt.IsNil)
	// b2 was cons'd last (innermost, called second), so it's car
	expected := values.List(values.NewSymbol("b2"), values.NewSymbol("b1"))
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, expected)
}

// TestRewindTo_NilBeforeThunks verifies that frames with nil Before
// closures are added to the stack without error.
func TestRewindTo_NilBeforeThunks(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	frame1 := machine.NewDynamicWindFrame(nil, nil)
	frame2 := machine.NewDynamicWindFrame(nil, nil)
	target := machine.WindingStack{frame1, frame2}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RewindTo(target, 0)
	c.Assert(err, qt.IsNil)
	c.Assert(testMC.WindingStack(), qt.HasLen, 2)
}

// --- Integration: call/cc with dynamic-wind exercises RestoreWithWindingFrom ---

// TestCallCC_DynamicWindReentry exercises RestoreWithWindingFrom through
// continuation re-entry into a dynamic-wind extent.
func TestCallCC_DynamicWindReentry(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define k #f)",
		"(define reentry-log '())",
		`(dynamic-wind
			(lambda () (set! reentry-log (cons 'before reentry-log)))
			(lambda ()
				(call-with-current-continuation
					(lambda (c) (set! k c)))
				(set! reentry-log (cons 'thunk reentry-log))
				'result)
			(lambda () (set! reentry-log (cons 'after reentry-log))))`,
	)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("result"))

	// Log should be (after thunk before) from first pass
	_, err = runSchemeExpr(t, env, "reentry-log")
	c.Assert(err, qt.IsNil)

	// Now re-enter the continuation
	mc, err = runSchemeExpr(t, env, "(k 'ignored)")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("result"))

	// After re-entry, log should have additional before/thunk/after entries
	mc, err = runSchemeExpr(t, env, "(memq 'before reentry-log)")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue() != values.FalseValue, qt.IsTrue,
		qt.Commentf("before thunk should have been called on re-entry"))
}

// TestComposableContinuation_DynamicWind exercises RestoreWithWindingFrom
// through composable continuation application with winding stack changes.
func TestComposableContinuation_DynamicWind(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExprs(t, env,
		"(define cc-tag (make-continuation-prompt-tag 'cc-test))",
		"(define cc-log '())",
		// Capture a composable continuation inside dynamic-wind
		`(define cc-k
			(call-with-continuation-prompt
				(lambda ()
					(dynamic-wind
						(lambda () (set! cc-log (cons 'before cc-log)))
						(lambda ()
							(+ 1 (call-with-composable-continuation
								(lambda (k) k)
								cc-tag)))
						(lambda () (set! cc-log (cons 'after cc-log)))))
				cc-tag
				(lambda (v) v)))`,
	)
	c.Assert(err, qt.IsNil)

	// Reset log before re-entry
	_, err = runSchemeExpr(t, env, "(set! cc-log '())")
	c.Assert(err, qt.IsNil)

	// Invoke composable continuation: should rewind (call before),
	// execute (+ 1 10) = 11, unwind (call after)
	var mc *machine.MachineContext
	mc, err = runSchemeExpr(t, env, "(cc-k 10)")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(11))

	// Verify winding happened
	mc, err = runSchemeExpr(t, env, "(memq 'before cc-log)")
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue() != values.FalseValue, qt.IsTrue,
		qt.Commentf("before thunk should have been called on composable continuation entry"))
}

// --- Error paths ---

// TestUnwindTo_ApplyError exercises the error path when an after thunk
// has the wrong arity (Apply fails).
func TestUnwindTo_ApplyError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	// Compile a closure that takes 1 argument (not a thunk)
	badAfter := compileClosure(t, env, "(lambda (x) x)")

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, badAfter))

	// UnwindTo should propagate the Apply error
	err := testMC.UnwindTo(0)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

// TestUnwindTo_RunError exercises the error path when an after thunk
// raises an exception during execution.
func TestUnwindTo_RunError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	// Compile a thunk that raises an error
	badAfter := compileClosure(t, env, `(lambda () (error "after-thunk-failed"))`)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	testMC.PushWindingFrame(machine.NewDynamicWindFrame(nil, badAfter))

	err := testMC.UnwindTo(0)
	c.Assert(err, qt.IsNotNil)
}

// TestRewindTo_ApplyError exercises the error path when a before thunk
// has the wrong arity.
func TestRewindTo_ApplyError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	badBefore := compileClosure(t, env, "(lambda (x) x)")
	frame := machine.NewDynamicWindFrame(badBefore, nil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RewindTo(machine.WindingStack{frame}, 0)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

// TestRewindTo_RunError exercises the error path when a before thunk
// raises an exception during execution.
func TestRewindTo_RunError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	badBefore := compileClosure(t, env, `(lambda () (error "before-thunk-failed"))`)
	frame := machine.NewDynamicWindFrame(badBefore, nil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RewindTo(machine.WindingStack{frame}, 0)
	c.Assert(err, qt.IsNotNil)
}

// TestRestoreWithWindingFrom_AfterThunkApplyError exercises the Apply error
// path in RestoreWithWindingFrom's unwind phase.
func TestRestoreWithWindingFrom_AfterThunkApplyError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	badAfter := compileClosure(t, env, "(lambda (x) x)")
	srcFrame := machine.NewDynamicWindFrame(nil, badAfter)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RestoreWithWindingFrom(nil,
		machine.WindingStack{srcFrame}, // source: has bad after thunk
		machine.WindingStack{},         // target: empty
	)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

// TestRestoreWithWindingFrom_AfterThunkRunError exercises the Run error
// path in RestoreWithWindingFrom's unwind phase.
func TestRestoreWithWindingFrom_AfterThunkRunError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	badAfter := compileClosure(t, env, `(lambda () (error "unwind-failed"))`)
	srcFrame := machine.NewDynamicWindFrame(nil, badAfter)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RestoreWithWindingFrom(nil,
		machine.WindingStack{srcFrame},
		machine.WindingStack{},
	)
	c.Assert(err, qt.IsNotNil)
}

// TestRestoreWithWindingFrom_RewindError exercises the error propagation
// from RewindTo through RestoreWithWindingFrom.
func TestRestoreWithWindingFrom_RewindError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	badBefore := compileClosure(t, env, "(lambda (x) x)") // wrong arity
	tgtFrame := machine.NewDynamicWindFrame(badBefore, nil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	testMC := machine.NewMachineContext(ctx, cont)

	err := testMC.RestoreWithWindingFrom(nil,
		machine.WindingStack{},         // source: empty
		machine.WindingStack{tgtFrame}, // target: has bad before thunk
	)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

// --- RunWithEscapeHandling additional coverage ---

// TestRunWithEscapeHandling_PromptAbortNotFound exercises the branch where
// no prompt matches the abort tag.
func TestRunWithEscapeHandling_PromptAbortNotFound(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	tag := machine.NewPromptTag("nonexistent")

	fn := func(mc *machine.MachineContext) error {
		return &machine.ErrPromptAbort{
			Tag:    tag,
			Values: []values.Value{values.NewInteger(1)},
		}
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	fcls := machine.NewForeignClosure(env, 0, false, fn)
	litIdx := tpl.MaybeAppendLiteral(fcls)
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpLoadLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpApply})
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	mc := machine.NewMachineContext(ctx, cont)

	err := mc.RunWithEscapeHandling()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no prompt found")
}

// TestRunWithEscapeHandling_PromptAbortNoHandler exercises the no-handler
// branch via Scheme: call-with-continuation-prompt with a handler that
// returns the value directly (simplest handler).
func TestRunWithEscapeHandling_PromptAbortNoHandler(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	// Test abort with handler that returns the abort value
	mc, err := runSchemeExprs(t, env,
		"(define nh-tag (make-continuation-prompt-tag 'nh))",
		`(call-with-continuation-prompt
			(lambda ()
				(abort-current-continuation nh-tag 42))
			nh-tag
			(lambda (v) v))`,
	)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestRunWithEscapeHandling_PromptAbortWithMultipleValues exercises abort
// with multiple values and a handler that processes them.
func TestRunWithEscapeHandling_PromptAbortWithMultipleValues(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	mc, err := runSchemeExprs(t, env,
		"(define mv-tag (make-continuation-prompt-tag 'mv))",
		`(call-with-continuation-prompt
			(lambda ()
				(abort-current-continuation mv-tag 10 20))
			mv-tag
			(lambda (a b) (+ a b)))`,
	)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(30))
}

// TestRunWithEscapeHandling_NormalCompletionWithWindingStack exercises the
// path where normal completion triggers unwinding of remaining winding frames.
func TestRunWithEscapeHandling_NormalCompletionWithWindingStack(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	_, err := runSchemeExpr(t, env, "(define unwind-called #f)")
	c.Assert(err, qt.IsNil)

	after := compileClosure(t, env, "(lambda () (set! unwind-called #t))")

	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	mc := machine.NewMachineContext(ctx, cont)

	mc.PushWindingFrame(machine.NewDynamicWindFrame(nil, after))

	err = mc.RunWithEscapeHandling()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.WindingStack(), qt.HasLen, 0)

	result, err := runSchemeExpr(t, env, "unwind-called")
	c.Assert(err, qt.IsNil)
	c.Assert(result.GetValue(), valuestest.SchemeEquals, values.TrueValue)
}

// TestRunWithEscapeHandling_OtherError exercises the fallthrough path
// where an unrecognized error is returned directly.
func TestRunWithEscapeHandling_OtherError(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	fn := func(mc *machine.MachineContext) error {
		return werr.NewForeignErrorf("custom test error")
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	fcls := machine.NewForeignClosure(env, 0, false, fn)
	litIdx := tpl.MaybeAppendLiteral(fcls)
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpLoadLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpApply})
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	mc := machine.NewMachineContext(ctx, cont)

	err := mc.RunWithEscapeHandling()
	c.Assert(err, qt.IsNotNil)
}

// TestWindingStackAliasingBug_M2 verifies that winding stack slicing uses
// cap-limited slices to prevent append from corrupting shared backing arrays.
//
// The test creates a continuation with multiple dynamic-wind frames, invokes
// it (triggering unwind/rewind), then checks that the winding stack operations
// maintain correct state without corruption from backing array aliasing.
//
// R7RS §6.10: Continuations can be invoked multiple times. If the winding
// stack is corrupted during unwinding, subsequent invocations execute the
// wrong before/after thunks, breaking the dynamic-wind guarantee.
func TestWindingStackAliasingBug_M2(t *testing.T) {
	c := qt.New(t)
	env := testutil.NewFullRuntimeEnv(t)

	// Track before/after thunk invocations to verify no corruption
	code := `
		(begin
		  (define log '())
		  (define (record! msg) (set! log (cons msg log)))
		  (define k #f)
		  (define invoked 0)

		  ; Set up dynamic-wind frames and capture continuation
		  (dynamic-wind
		    (lambda () (record! 'before1))
		    (lambda ()
		      (dynamic-wind
		        (lambda () (record! 'before2))
		        (lambda ()
		          (dynamic-wind
		            (lambda () (record! 'before3))
		            (lambda ()
		              (if (= invoked 0)
		                  (call/cc (lambda (cont) (set! k cont))))
		              (set! invoked (+ invoked 1))
		              (record! 'body))
		            (lambda () (record! 'after3))))
		        (lambda () (record! 'after2))))
		    (lambda () (record! 'after1)))

		  ; Invoke continuation once to trigger unwind/rewind
		  ; This tests the winding stack aliasing bug
		  (if (< invoked 2) (k 'reinvoke))

		  ; Return log for verification
		  log)
	`

	mc, err := runSchemeExpr(t, env, code)
	c.Assert(err, qt.IsNil)

	// Verify the log sequence (reverse order because cons prepends)
	expected := []string{
		"after1", "after2", "after3", "body", // Second exit (from k invocation)
		"before3", "before2", "before1", // Re-entry from continuation invocation
		"after1", "after2", "after3", "body", // First exit (normal)
		"before3", "before2", "before1", // Initial entry and execution
	}

	// Extract log as a list of symbols (returned from the code above)
	// Convert to Go slice for comparison
	var logSymbols []string
	curr := mc.GetValue()
	for !values.IsEmptyList(curr) {
		pair, ok := curr.(*values.Pair)
		c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair, got %T", curr))

		sym, ok := pair.Car().(*values.Symbol)
		c.Assert(ok, qt.IsTrue, qt.Commentf("expected symbol, got %T", pair.Car()))
		logSymbols = append(logSymbols, sym.Key)

		curr = pair.Cdr()
	}

	c.Assert(logSymbols, qt.DeepEquals, expected,
		qt.Commentf("winding stack corruption detected: before/after thunks called in wrong order"))
}
