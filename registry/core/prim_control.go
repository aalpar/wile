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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimApply implements the apply primitive.
// Applies a procedure to a list of arguments.
func PrimApply(mc *machine.MachineContext) error {
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
		v, err := finalTuple.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
			prefixArgs = append(prefixArgs, elem)
			return nil
		})
		if err != nil {
			return err
		}
		if !values.IsEmptyList(v) {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "apply: final argument is an improper list")
		}
	}

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err := sub.ApplyCallable(proc, prefixArgs...)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
	}
	mc.SetValues(sub.GetValues()...)
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
func PrimCallCC(mc *machine.MachineContext) error {
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
	cc := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())

	capt := machine.NewCapturedContinuation(cc, mc.ThreadID(), mc.BarrierValid())

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

// PrimDynamicWind implements the (dynamic-wind) primitive.
// Calls a thunk with before and after handlers that execute on entry and exit.
//
// R7RS §6.10: dynamic-wind calls thunk without arguments, returning the result(s).
// Before is called whenever execution enters the dynamic extent of the call to thunk,
// and after is called whenever it exits.
func PrimDynamicWind(mc *machine.MachineContext) error {
	before := mc.Arg(0)
	thunk := mc.Arg(1)
	after := mc.Arg(2)

	beforeCls, ok := before.(machine.Closure)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "dynamic-wind: before must be a procedure, got %T", before)
	}

	thunkCls, ok := thunk.(machine.Closure)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "dynamic-wind: thunk must be a procedure, got %T", thunk)
	}

	afterCls, ok := after.(machine.Closure)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "dynamic-wind: after must be a procedure, got %T", after)
	}

	// Create a new winding frame
	frame := machine.NewDynamicWindFrame(beforeCls, afterCls)

	// 1. Call before thunk (in current dynamic extent)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err := sub.ApplyCallable(beforeCls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		return err
	}

	// 2. Push frame onto winding stack (we're now in this dynamic extent)
	mc.PushWindingFrame(frame)

	// 3. Create escape continuation for call/cc inside the thunk.
	// This points to "after dynamic-wind returns" so that continuations captured
	// inside the thunk can properly continue the outer computation.
	// The offset of 1 means "next instruction after this foreign function call".
	escapeCont := machine.NewMachineContinuationFromMachineContext(mc, 1)

	// 4. Call main thunk (with new winding context and escape continuation)
	sub2 := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub2)
	sub2.SetEscapeCont(escapeCont) // Allow call/cc to find continuation
	_, err = sub2.ApplyCallable(thunkCls)
	if err != nil {
		mc.PopWindingFrame() // Clean up on Apply error
		return err
	}
	thunkErr := sub2.Run()
	thunkResult := sub2.GetValues()

	// 5. Pop frame from winding stack (exiting this dynamic extent)
	mc.PopWindingFrame()

	// 6. Call after thunk (back to original dynamic extent)
	sub3 := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub3)
	_, err = sub3.ApplyCallable(afterCls)
	if err != nil {
		return err
	}
	err = sub3.Run()
	if err != nil {
		return err
	}

	// 7. Handle thunk's result/error
	if thunkErr != nil {
		return thunkErr
	}

	mc.SetValues(thunkResult...)
	return nil
}

// PrimValues implements the values primitive.
// Returns multiple values as specified by R7RS. With no arguments returns no values.
// With one or more arguments, returns all arguments as multiple values.
func PrimValues(mc *machine.MachineContext) error {
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
func PrimCallWithValues(mc *machine.MachineContext) error {
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

	// Call consumer with all produced values as arguments
	sub2 := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub2)
	_, err = sub2.ApplyCallable(consumerCls, producedValues...)
	if err != nil {
		return err
	}
	err = sub2.Run()
	if err != nil {
		return err
	}

	// Return what consumer returned
	mc.SetValues(sub2.GetValues()...)
	return nil
}
