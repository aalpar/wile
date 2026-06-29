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

package core

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimApply implements the apply primitive.
// Applies a procedure to a list of arguments.
func PrimApply(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "apply")
	if err != nil {
		return err
	}
	proc := mc.Arg(0)
	restVal := mc.Arg(1)

	// R7RS: (apply proc arg1 ... args) combines arg1 ... with the final list args
	// restVal is a list containing (arg1 ... args) where args is the final list
	restTuple, ok := restVal.(values.Tuple)
	if !ok || values.IsEmptyList(restVal) {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "apply: expected at least one argument list")
	}

	// Collect all elements from rest except the last one, which is the final args list
	var prefixArgs values.Vector
	var finalList values.Value
	for {
		car := restTuple.Car()
		cdr := restTuple.Cdr()
		if values.IsEmptyList(cdr) {
			// This is the last element - it's the final args list
			finalList = car
			break
		}
		prefixArgs = append(prefixArgs, car)
		restTuple, ok = cdr.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "apply: improper rest argument list")
		}
	}

	// Now append elements from finalList to prefixArgs
	if !values.IsEmptyList(finalList) {
		finalTuple, ok := finalList.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "apply: final argument must be a list but got %T", finalList)
		}
		err := helpers.ForEachList(mc.Context(), finalTuple, "apply", func(_ context.Context, _ int, _ bool, elem values.Value) error {
			prefixArgs = append(prefixArgs, elem)
			return nil
		})
		if err != nil {
			return err
		}
	}

	// Run proc in place rather than in a sub-context. ApplyCallable reconfigures
	// the VM (mc.reconfigured / template repoint), and the foreign-call
	// dispatcher continues execution into proc instead of restoring. proc
	// returns through the continuation already on mc.cont — the frame saved for
	// this (apply ...) call (non-tail) or the caller's caller (tail) — so
	// (apply f args) in tail position is a proper tail call, and proc's result
	// (including multiple values) flows back naturally. This avoids a per-call
	// sub-context plus a pooled eval stack acquisition on every apply.
	//
	// Unlike PrimCallCC, apply needs no mc.Parent() gate: it generates no
	// continuation abort of its own. Any control effect inside proc (call/cc,
	// raise, an invoked continuation) is handled by proc's own machinery and
	// propagates through mc exactly as if proc had been called directly.
	_, err = mc.ApplyCallable(proc, prefixArgs...)
	if err != nil {
		return err
	}
	return nil
}

// PrimCallCC implements the call/cc primitive.
// Captures current continuation and passes to procedure.
//
// R7RS §6.10: call-with-current-continuation packages the current continuation
// as an "escape procedure" and passes it as an argument to proc.
//
// Curry-Howard: call/cc as Peirce's law (Griffin 1990).
//
//	call/cc : ((A → B) → A) → A
//
//	where f : (A → B) → A  is the user callback, and the escape
//	continuation k : A → B has return type B (invoking k never returns
//	to f — it reinstalls the captured continuation at the prompt).
//
//	Adding call/cc to a language = adding the law of excluded middle.
//	This means certain program transformations (e.g., CPS conversion
//	optimizations that assume intuitionistic control flow) are unsound
//	in the presence of call/cc.
//
//	Invariant: invoking k must NOT return to f. The CapturedContinuation
//	  returns an ErrResumeContinuation carrying the captured segment unrun;
//	  the nearest DefaultPromptTag driver reinstalls it onto the live chain
//	  (boundary == nil: replace the whole chain). Resuming the captured
//	  continuation instead of returning to f is what gives k the B return type.
//	Constrains: ErrResumeContinuation handling (must propagate through
//	  foreign calls), RunResumable (the top-level resume/abort driver).
//	Constrained by: CESK model (K must be capturable data, not Go
//	  stack), WindingStack (captured by value for dynamic-wind thunks),
//	  threadID (cross-thread invocation rejected).
//
// See BIBLIOGRAPHY.md "call/cc as Peirce's Law".
//
// Implementation follows the Racket model: capture a composable continuation
// (via SliceContinuationAt) and return it as a CapturedContinuation value.
// Invoking that value does not run the segment — it returns an
// ErrResumeContinuation that the driver reinstalls onto the live chain (the
// resume trampoline; see docs/continuations/resume-trampoline.md). The
// equivalence below is the semantic model, not the literal runtime path:
//
//	(call/cc f) ≡
//	  (call-with-composable-continuation
//	    (lambda (k)
//	      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
//	    default-prompt-tag)
//
// Two execution modes:
//
// Inline mode (mc.Parent() != nil): The lambda runs directly in the current VM context.
// This preserves the full continuation chain, ensuring continuations captured inside the
// lambda include the complete call stack back to the top level. This is critical for
// cooperative coroutines and other patterns that capture/invoke multiple continuations.
//
// Sub-context mode (mc.Parent() == nil): Falls back to running the lambda in an isolated
// sub-context. Used when call/cc is invoked inside another foreign function's sub-context
// (e.g., inside apply or dynamic-wind) where there's no saved continuation to return to.
func PrimCallCC(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call/cc")
	if err != nil {
		return err
	}
	proc := mc.Arg(0)

	mcls, err := helpers.RequireType[machine.Closure](proc, werr.ErrNotAProcedure, "call/cc")
	if err != nil {
		return err
	}

	// Capture the continuation delimited at the nearest DefaultPromptTag (D5).
	// FindPrompt(DefaultPromptTag) returns (nil, true) at the top-level context
	// boundary — SliceContinuationAt(nil) then captures the whole chain — or a chain
	// FRAME for a call/cc captured inside a call-with-continuation-prompt reusing the
	// default tag; slicing at that frame captures only the delimited segment (NOT the
	// code after the prompt, which would otherwise include the re-invocation site and
	// loop forever — TestApplyWindingStackInheritance). The matching invocation-side
	// reinstatement (RunResumable's ErrResumeContinuation arm) grafts this segment onto
	// the nearest DefaultPromptTag at the (k v) site, so an inner prompt receives the
	// segment's result while the top-level case replaces the whole chain (escape-past).
	capturePrompt, _ := mc.FindPrompt(machine.DefaultPromptTag)
	segment := mc.SliceContinuationAt(capturePrompt)
	windingStack := mc.WindingStack().Copy()
	comp := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())
	// Snapshot the reachable parameter/handler marks so a continuation captured inside
	// a sub-context (e.g. a call-with-values producer) restores the outer environment
	// on resume — call/cc isolates marks on resume, so without this the outer
	// handler/parameter values would be lost. Composable continuations
	// (call-with-composable-continuation) and timer interrupts deliberately do NOT
	// snapshot: they compose with the invoker's continuation (no isolatedMarks), so
	// they should see the invoker's marks, not a captured snapshot.
	mc.SnapshotReachableMarksInto(comp)

	capt := machine.NewCapturedContinuation(comp, mc.ThreadID(), mc.BarrierValid())

	if mc.Parent() != nil {
		// Inline mode: apply the lambda directly in the current VM context.
		// When the lambda returns normally, RestoreContinuation pops mc.cont,
		// resuming from the caller of call/cc. When the lambda invokes the
		// continuation, the escape propagates through the VM to RunWithEscapeHandling.
		_, err := mc.ApplyCallable(mcls, capt)
		if err != nil {
			return err
		}
		// No PC compensation needed: applyForeign does not post-increment pc.
		return nil
	}

	// Sub-context mode (the call/cc-er is itself rootless — e.g. inside another
	// foreign sub-context or a thread root): run the lambda under its OWN
	// DefaultPromptTag driver. Sub-context mode IS the implicit
	// call-with-continuation-prompt for this captured continuation, so it must be a
	// DefaultPromptTag owner: RunWithEscapeHandling installs DefaultPromptTag and
	// resolves the call/cc resume trampoline (ErrResumeContinuation) and any reified
	// boundary on this sub's chain, then delivers the result. (RunWithinBoundary would
	// re-raise the resume signal, which has no enclosing driver here.)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(mcls, capt)
	if err != nil {
		return err
	}
	err = sub.RunWithEscapeHandling()
	if err != nil {
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}

// PrimValues implements the values primitive.
// Returns multiple values as specified by R7RS. With no arguments returns no values.
// With one or more arguments, returns all arguments as multiple values.
func PrimValues(mc machine.CallContext) error {
	restVal := mc.Arg(0)

	// restVal is a list of all arguments (variadic)
	if values.IsEmptyList(restVal) {
		// (values) with no arguments returns no values
		mc.SetValues()
		return nil
	}

	// Collect all values from the list
	var vals []values.Value
	current := restVal
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "values: improper argument list")
		}
		vals = append(vals, tuple.Car())
		current = tuple.Cdr()
	}

	mc.SetValues(vals...)
	return nil
}

// PrimCallWithValues implements the call-with-values primitive.
// Calls producer, passes results to consumer.
func PrimCallWithValues(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-values")
	if err != nil {
		return err
	}
	producer := mc.Arg(0)
	consumer := mc.Arg(1)

	producerCls, err := helpers.RequireType[machine.Closure](producer, werr.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	consumerCls, err := helpers.RequireType[machine.Closure](consumer, werr.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	// Reify call-with-values: push a consumer apply-frame onto mc.cont and inline-apply
	// the producer on the live chain (no producer sub-context). On producer's normal
	// return the frame applies consumer to the produced value(s) exactly once; the
	// consumer call stays in tail position relative to call-with-values (R7RS §3.5),
	// returning through mc.cont as before.
	//
	// Because the consumer is now a continuation-CHAIN frame, a continuation captured
	// inside producer spans the consumer frame and the rest of the program — re-entry
	// replays them, fixing the producer-sub-context truncation — while a full call/cc
	// invoked inside producer aborts to DefaultPromptTag, discarding the chain-resident
	// consumer frame (escape-past preserved: the consumer does NOT run).
	_, err = mc.RunBodyUnderConsumer(producerCls, consumerCls)
	return err
}
