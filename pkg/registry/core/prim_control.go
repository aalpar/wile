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
	"errors"

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
//	to f — it aborts to the prompt).
//
//	Adding call/cc to a language = adding the law of excluded middle.
//	This means certain program transformations (e.g., CPS conversion
//	optimizations that assume intuitionistic control flow) are unsound
//	in the presence of call/cc.
//
//	Invariant: the escape closure must abort to DefaultPromptTag after
//	  applying the captured continuation. Without the abort, control
//	  would return to f after k returns, violating the B return type.
//	Constrains: ErrPromptAbort handling (must propagate through foreign
//	  calls), RunWithEscapeHandling (top-level abort catcher).
//	Constrained by: CESK model (K must be capturable data, not Go
//	  stack), WindingStack (captured by value for dynamic-wind thunks),
//	  threadID (cross-thread invocation rejected).
//
// See BIBLIOGRAPHY.md "call/cc as Peirce's Law".
//
// Implementation follows the Racket model: capture a composable continuation
// (via SliceContinuationAt), build an escape closure that applies it then aborts.
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

	// Capture a composable continuation via SliceContinuationAt(nil).
	// FindPrompt(DefaultPromptTag) returns (nil, true) for the context-level
	// prompt, so SliceContinuationAt(nil) deep-copies the entire chain.
	segment := mc.SliceContinuationAt(nil)
	windingStack := mc.WindingStack().Copy()
	comp := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())

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

	// Sub-context mode: run the lambda in an isolated context.
	// The CapturedContinuation's apply emits ErrPromptAbort to DefaultPromptTag.
	// This propagates up through Run() and is caught by RunWithEscapeHandling
	// at the top level. In contexts without RunWithEscapeHandling (e.g., threads
	// that call Run() directly), we catch the abort here and extract the value —
	// sub-context mode acts as the implicit call-with-continuation-prompt.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(mcls, capt)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var abortErr *machine.ErrPromptAbort
		if errors.As(err, &abortErr) && abortErr.Tag == machine.DefaultPromptTag {
			// The escape closure ran the composable continuation to completion and
			// aborted to DefaultPromptTag with the result. Extract all values here
			// so callers that don't use RunWithEscapeHandling (e.g., threads) work.
			if len(abortErr.Values) > 0 {
				mc.SetValues(abortErr.Values...)
			} else {
				mc.SetValue(values.Void)
			}
			return nil
		}
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

	// Call producer with no arguments
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(producerCls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
	}

	// Get all values returned by producer
	producedValues := sub.GetValues()

	// Apply the consumer in place on mc (not in a sub-context) so the consumer
	// call is in tail position relative to call-with-values: it returns through
	// mc.cont, mirroring PrimApply. Running it in a sub-context with sub2.Run()
	// nested one Go stack frame per call, so a tail loop through call-with-values
	// (directly, or via the let-values/let*-values macros) overflowed the host
	// goroutine stack instead of running in O(1) frames. R7RS §3.5 requires the
	// consumer call to be a tail call.
	//
	// Lifetime: ApplyCallable binds producedValues into the consumer's frame
	// synchronously (Apply -> bindArgs), so the deferred ReleaseSubContext(sub)
	// on return cannot corrupt them. The consumer's own result (including
	// multiple values) flows back through mc naturally, exactly as in apply.
	_, err = mc.ApplyCallable(consumerCls, producedValues...)
	if err != nil {
		return err
	}
	return nil
}
