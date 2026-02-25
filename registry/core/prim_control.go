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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
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
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "apply: expected at least one argument list")
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
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: improper rest argument list")
		}
	}

	// Now append elements from finalList to prefixArgs
	if !values.IsEmptyList(finalList) {
		finalTuple, ok := finalList.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: final argument must be a list but got %T", finalList)
		}
		v, err := finalTuple.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
			prefixArgs = append(prefixArgs, elem)
			return nil
		})
		if err != nil {
			return err
		}
		if !values.IsEmptyList(v) {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: final argument is an improper list")
		}
	}

	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
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

	mcls, err := helpers.RequireType[machine.Closure](proc, values.ErrNotAProcedure, "call/cc")
	if err != nil {
		return err
	}

	// Capture a composable continuation via SliceContinuationAt(nil).
	// FindPrompt(DefaultPromptTag) returns (nil, true) for the context-level
	// prompt, so SliceContinuationAt(nil) deep-copies the entire chain.
	segment := mc.SliceContinuationAt(nil)
	windingStack := mc.WindingStack().Copy()
	cc := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())

	contClosure := newComposeAbortEscapeClosure(mc.EnvironmentFrame().TopLevel(), cc, mc.ThreadID(), mc.BarrierValid())

	if mc.Parent() != nil {
		// Inline mode: apply the lambda directly in the current VM context.
		// When the lambda returns normally, RestoreContinuation pops mc.cont,
		// resuming from the caller of call/cc. When the lambda invokes the
		// continuation, the escape propagates through the VM to RunWithEscapeHandling.
		_, err := mc.ApplyCallable(mcls, contClosure)
		if err != nil {
			return err
		}
		// No PC compensation needed: applyForeign does not post-increment pc
		// (unlike OperationForeignFunctionCall which did mc.pc++ after each call).
		return nil
	}

	// Sub-context mode: run the lambda in an isolated context.
	// The escape closure emits ErrPromptAbort to DefaultPromptTag. This
	// propagates up through Run() and is caught by RunWithEscapeHandling
	// at the top level. In contexts without RunWithEscapeHandling (e.g., threads
	// that call Run() directly), we catch the abort here and extract the value —
	// sub-context mode acts as the implicit call-with-continuation-prompt.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
	_, err = sub.ApplyCallable(mcls, contClosure)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var abortErr *machine.ErrPromptAbort
		if errors.As(err, &abortErr) && abortErr.Tag == machine.DefaultPromptTag {
			// The escape closure ran the composable continuation to completion and
			// aborted to DefaultPromptTag with the result. Extract the value here
			// so callers that don't use RunWithEscapeHandling (e.g., threads) work.
			if len(abortErr.Values) > 0 {
				mc.SetValue(abortErr.Values[0])
			} else {
				mc.SetValue(values.Void)
			}
			return nil
		}
		return err
	}

	mc.SetValue(sub.GetValue())
	return nil
}

// newComposeAbortEscapeClosure creates an escape closure that applies the composable
// continuation in a sub-context, then aborts to DefaultPromptTag with the result.
//
// This implements the Racket model: (lambda (v) (abort-current-continuation default-prompt-tag (k v)))
// where k is the composable continuation captured at the call/cc site.
//
// capturingThreadID records which thread captured the continuation; invoking from a
// different thread returns ErrCrossThreadContinuation per SRFI-18 semantics.
//
// capturingBarrierValid is mc.BarrierValid() at capture time (nil = outside any barrier).
// On invocation, the closure compares capture-time and invocation-time barrier pointers:
// inequality means the continuation would cross a with-continuation-barrier boundary.
func newComposeAbortEscapeClosure(
	env *environment.EnvironmentFrame,
	cc *machine.ComposableContinuation,
	capturingThreadID uint64,
	capturingBarrierValid *machine.BarrierToken,
) *machine.ForeignClosure {
	fn := func(innerMC *machine.MachineContext) error {
		// Reject cross-thread continuation invocation
		if innerMC.ThreadID() != capturingThreadID {
			return values.WrapForeignErrorf(values.ErrCrossThreadContinuation,
				"call/cc: continuation captured in thread %d, invoked from thread %d",
				capturingThreadID, innerMC.ThreadID())
		}
		// Reject barrier crossing: pointer inequality means different barrier contexts.
		// nil != non-nil: captured outside, invoked inside (or vice versa).
		// ptr-A != ptr-B: captured inside barrier A, invoked inside barrier B.
		if capturingBarrierValid != innerMC.BarrierValid() {
			return values.WrapForeignErrorf(values.ErrContinuationBarrier,
				"call/cc: continuation cannot cross continuation barrier")
		}
		// Get the value passed to the continuation (from the closure's argument)
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

		// Apply the composable continuation in a sub-context.
		// ApplyCallable does DeepCopy → Graft → RestoreWithWindingFrom → Restore → SetValue.
		sub := innerMC.NewSubContext()
		defer machine.ReleaseSubContext(sub)
		sub.SetWindingStack(innerMC.WindingStack())
		_, err := sub.ApplyCallable(cc, val)
		if err != nil {
			return err
		}
		// Run the restored frames to completion.
		// When frames finish (cont == nil → errHalt → Run returns nil).
		err = sub.Run()
		if err != nil {
			return err
		}

		// Abort to DefaultPromptTag with the result.
		// RunWithEscapeHandling catches this via the nil-prompt path.
		return &machine.ErrPromptAbort{
			Tag:    machine.DefaultPromptTag,
			Values: []values.Value{sub.GetValue()},
		}
	}
	return machine.NewForeignClosure(env, 1, false, fn)
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
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: before must be a procedure, got %T", before)
	}

	thunkCls, ok := thunk.(machine.Closure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: thunk must be a procedure, got %T", thunk)
	}

	afterCls, ok := after.(machine.Closure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: after must be a procedure, got %T", after)
	}

	// Create a new winding frame
	frame := machine.NewDynamicWindFrame(beforeCls, afterCls)

	// 1. Call before thunk (in current dynamic extent)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
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
	sub2.SetWindingStack(mc.WindingStack()) // Include new frame
	sub2.SetEscapeCont(escapeCont)          // Allow call/cc to find continuation
	_, err = sub2.ApplyCallable(thunkCls)
	if err != nil {
		mc.PopWindingFrame() // Clean up on Apply error
		return err
	}
	thunkErr := sub2.Run()
	thunkResult := sub2.GetValues()

	// 4. Pop frame from winding stack (exiting this dynamic extent)
	mc.PopWindingFrame()

	// 5. Call after thunk (back to original dynamic extent)
	sub3 := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub3)
	sub3.SetWindingStack(mc.WindingStack())
	_, err = sub3.ApplyCallable(afterCls)
	if err != nil {
		return err
	}
	err = sub3.Run()
	if err != nil {
		return err
	}

	// 6. Handle thunk's result/error
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
			return values.WrapForeignErrorf(values.ErrNotAList, "values: improper argument list")
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

	producerCls, err := helpers.RequireType[machine.Closure](producer, values.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	consumerCls, err := helpers.RequireType[machine.Closure](consumer, values.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	// Call producer with no arguments
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
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
	sub2.SetWindingStack(mc.WindingStack())
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
