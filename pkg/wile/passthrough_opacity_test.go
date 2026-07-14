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

package wile_test

// Tier-2 passthrough / quasiquote opacity, end to end.
//
// A cond-expand or include body, and a quasiquote template, reach the validator as
// raw syntax it never looks inside. Every analysis built on the sub-expression walk
// therefore concluded "nothing in there" — not conservatively, just blindly. Three
// consumers were wrong as a result, and two of them silently:
//
//   escape/mutable marking -> a let-bound lambda mutated through a quasiquote was
//     INLINED with its stale pre-set! body (returned 7, not 99). Silent.
//   frame-reuse arming     -> a closure captured inside a cond-expand did not
//     disqualify in-place frame reuse, so codegen armed OpSelfTailCall and rebound
//     the parameter frame under a live closure. Silent, and it corrupts across
//     TYPES, not just values.
//   StableInUnit           -> a legal same-unit set! concealed in either was
//     REJECTED under the default immutable top level. Loud.
//
// Every case below was verified RED against the unfixed tree.
//
// Reproduction condition: the bodies are (begin …)-wrapped where the finding is
// about a single top-level unit, matching how file execution feeds a program in.

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

func evalPassthrough(t *testing.T, src string) (wile.Value, error) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng.EvalMultiple(ctx, src)
}

// TestPassthrough_QuasiquoteSetBangDefeatsInlining is the SILENT miscompile, and the
// most serious of the group: the set! runs, f really does hold the new lambda, and
// the program still returns the old one — because the inliner had already
// substituted f's body, never having seen the set! hidden in the quasiquote.
//
// The review's CORRECTION on this finding matters: it is NOT confined to the
// inline-threshold path. The broken invariant is escape/mutable marking, so a
// non-inlinable body miscompiles too; the inliner is just where it shows first.
func TestPassthrough_QuasiquoteSetBangDefeatsInlining(t *testing.T) {
	v, err := evalPassthrough(t, `
		(let ((f (lambda () 7)))
		  (define ignored `+"`"+`(,(set! f (lambda () 99))))
		  (f))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "99",
		qt.Commentf("the set! inside the quasiquote must defeat inlining of f. "+
			"Returning 7 means the inliner substituted f's stale pre-set! body."))
}

// TestPassthrough_CondExpandHiddenClosureDoesNotCorruptFrame is the OTHER silent
// one. Each iteration captures a closure over the loop parameter i, but the capture
// is hidden inside a cond-expand, so the escape analysis never saw it and codegen
// armed in-place frame reuse. The closures then all read the same, rebound slot.
//
// The corruption is not merely "stale values": before the fix this returned four
// #<machine-closure>, because the reused slot no longer held an integer at all. A
// test asserting only "the numbers are stale" would have been satisfied by a
// wrong-type read too.
func TestPassthrough_CondExpandHiddenClosureDoesNotCorruptFrame(t *testing.T) {
	v, err := evalPassthrough(t, `
		(begin
		  (define slot (make-vector 1 '()))
		  (define (save! f) (vector-set! slot 0 (cons f (vector-ref slot 0))))
		  (define (loop i)
		    (if (= i 0)
		        'done
		        (begin
		          (cond-expand (else (save! (lambda () i))))
		          (loop (- i 1)))))
		  (loop 4)
		  (map (lambda (f) (f)) (vector-ref slot 0)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(1 2 3 4)",
		qt.Commentf("each closure must observe its OWN iteration's i. Before the fix this "+
			"was (#<machine-closure> …): the frame was rebound in place under the live "+
			"closures and the slot that held i held something else entirely."))
}

// TestPassthrough_VisibleClosureCapture is the control. The same loop with the
// capture in plain sight was always correct; if this ever fails, the bug is in the
// escape analysis generally, not in the opacity hole.
func TestPassthrough_VisibleClosureCapture(t *testing.T) {
	v, err := evalPassthrough(t, `
		(begin
		  (define slot (make-vector 1 '()))
		  (define (save! f) (vector-set! slot 0 (cons f (vector-ref slot 0))))
		  (define (loop i)
		    (if (= i 0)
		        'done
		        (begin
		          (save! (lambda () i))
		          (loop (- i 1)))))
		  (loop 4)
		  (map (lambda (f) (f)) (vector-ref slot 0)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(1 2 3 4)")
}

// TestPassthrough_CondExpandSetBangIsLegal is the LOUD symptom: a same-unit set! is
// legal, and concealing it in a cond-expand must not make StableInUnit freeze the
// binding and reject it.
func TestPassthrough_CondExpandSetBangIsLegal(t *testing.T) {
	v, err := evalPassthrough(t, `
		(begin
		  (define x 1)
		  (cond-expand (else (set! x 2)))
		  x)
	`)
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("a legal same-unit set! concealed in a cond-expand must not be "+
			"rejected as a mutation of an immutable top-level binding"))
	qt.Assert(t, v.SchemeString(), qt.Equals, "2")
}

// TestPassthrough_QuasiquoteSetBangIsLegal is the same, one form over.
func TestPassthrough_QuasiquoteSetBangIsLegal(t *testing.T) {
	v, err := evalPassthrough(t, `
		(begin
		  (define x 1)
		  (define ignored `+"`"+`(,(set! x 2)))
		  x)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "2")
}

// TestPassthrough_NoSelfTailCallForConcealedCapture is the bytecode assertion
// REVIEW.md §7 asks for by name: "no bytecode assertion that SelfTailCall is NOT
// emitted for a passthrough-wrapped call/cc."
//
// It pins the fix at the level where it is decided, not merely where it is observed.
// The runtime differential above could be satisfied by some unrelated change that
// happens to mask the corruption; this cannot. A visible call/cc suppresses
// SelfTailCall, and after the fix a concealed one must too.
func TestPassthrough_NoSelfTailCallForConcealedCapture(t *testing.T) {
	disasmOfLoop := func(t *testing.T, body string) string {
		t.Helper()
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		_, err = eng.EvalMultiple(ctx, `
			(begin
			  (define sink '())
			  (define (loop i acc)
			    (if (= i 0)
			        acc
			        (loop (- i 1) (cons `+body+` acc)))))
		`)
		qt.Assert(t, err, qt.IsNil)

		v, ok := eng.Get("loop")
		qt.Assert(t, ok, qt.IsTrue)
		closure, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue)
		return machine.DisassembleString(closure.Template())
	}

	// Baseline: a call/cc in plain sight is a known capture, so the compiler must
	// not arm in-place frame reuse. If this ever emits SelfTailCall the guard is
	// broken outright and the concealed case below proves nothing.
	visible := disasmOfLoop(t, `(call/cc (lambda (k) i))`)
	qt.Assert(t, strings.Contains(visible, "SelfTailCall"), qt.IsFalse,
		qt.Commentf("a VISIBLE call/cc must already suppress OpSelfTailCall; "+
			"if it does not, this test's premise is void.\n%s", visible))

	// The finding: the same call/cc behind a cond-expand must suppress it too.
	// Before the fix the body validated to an opaque literal the analysis read as
	// childless, so the capture vanished and SelfTailCall was armed.
	concealed := disasmOfLoop(t, `(cond-expand (else (call/cc (lambda (k) i))))`)
	qt.Assert(t, strings.Contains(concealed, "SelfTailCall"), qt.IsFalse,
		qt.Commentf("a call/cc CONCEALED in a cond-expand must suppress OpSelfTailCall "+
			"exactly as a visible one does. Emitting it rebinds the parameter frame in "+
			"place while a captured continuation still aliases it.\n%s", concealed))

	// And a loop with no capture at all must STILL get the optimization — otherwise
	// the fix is indistinguishable from "never arm SelfTailCall", which would pass
	// both assertions above while silently deleting the optimization.
	plain := disasmOfLoop(t, `i`)
	qt.Assert(t, strings.Contains(plain, "SelfTailCall"), qt.IsTrue,
		qt.Commentf("a capture-free self-tail loop must still emit OpSelfTailCall; "+
			"the fix must narrow the arming, not abolish it.\n%s", plain))
}
