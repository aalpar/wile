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
func PrimApply(ctx context.Context, mc *machine.MachineContext) error {
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
		v, err := finalTuple.ForEach(ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
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

	// Resolve the callable to a MachineClosure, handling case-lambda dispatch.
	var mcls *machine.MachineClosure
	switch p := proc.(type) {
	case *machine.MachineClosure:
		mcls = p
	case *machine.CaseLambdaClosure:
		var found bool
		mcls, found = p.FindMatchingClause(len(prefixArgs))
		if !found {
			return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments,
				"apply: no matching clause in case-lambda for %d arguments", len(prefixArgs))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "apply: expected a procedure but got %T", proc)
	}

	sub := mc.NewSubContext()
	_, err := sub.Apply(mcls, prefixArgs...)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		// Propagate continuation escapes
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
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
func PrimCallCC(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)

	mcls, err := helpers.RequireType[*machine.MachineClosure](proc, values.ErrNotAProcedure, "call/cc")
	if err != nil {
		return err
	}

	// Capture the current continuation and winding stack.
	var cont *machine.MachineContinuation
	var escapeCont *machine.MachineContinuation
	if mc.Parent() != nil {
		cont = mc.Parent().Copy()
	} else {
		// Sub-context: capture our own state as the inner continuation.
		cont = machine.NewMachineContinuationFromMachineContext(mc, 1)
		if mc.EscapeCont() != nil {
			escapeCont = mc.EscapeCont().Copy()
		}
	}
	windingStack := mc.WindingStack().Copy()

	contClosure := newEscapeContinuationClosureWithWinding(mc.EnvironmentFrame().TopLevel(), cont, windingStack, escapeCont)

	if mc.Parent() != nil {
		// Inline mode: apply the lambda directly in the current VM context.
		// When the lambda returns normally, RestoreContinuation pops mc.cont,
		// resuming from the caller of call/cc. When the lambda invokes the
		// continuation, the escape propagates through the VM to RunWithEscapeHandling.
		_, err := mc.Apply(mcls, contClosure)
		if err != nil {
			return err
		}
		// Compensate for OperationForeignFunctionCall's mc.pc++ (Apply set pc=0).
		mc.SetPC(mc.PC() - 1)
		return nil
	}

	// Sub-context mode: run the lambda in an isolated context.
	sub := mc.NewSubContext()
	sub.SetWindingStack(mc.WindingStack())
	_, err = sub.Apply(mcls, contClosure)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			// Only handle escapes targeting OUR continuation.
			if escapeErr.Continuation != cont {
				return err
			}
			sourceStack := sub.WindingStack()
			restoreErr := mc.RestoreWithWindingFrom(escapeErr.Continuation, sourceStack, escapeErr.WindingStack)
			if restoreErr != nil {
				return restoreErr
			}
			mc.SetValue(escapeErr.Value)
			escapeErr.Handled = true
			return escapeErr
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	mc.SetValue(sub.GetValue())
	return nil
}

// newEscapeContinuationClosureWithWinding creates a closure that escapes to the captured
// continuation when called, including winding stack information for proper dynamic-wind handling.
// The escapeCont parameter is used for continuations captured inside sub-contexts - it represents
// where execution should continue after the inner continuation completes.
func newEscapeContinuationClosureWithWinding(
	env *environment.EnvironmentFrame,
	cont *machine.MachineContinuation,
	windingStack machine.WindingStack,
	escapeCont *machine.MachineContinuation,
) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		// Get the value passed to the continuation (from the closure's argument)
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		// Return an escape error that will propagate up through sub-contexts
		return &machine.ErrContinuationEscape{
			Continuation: cont,
			Value:        val,
			WindingStack: windingStack, // Target winding for restoration
			EscapeCont:   escapeCont,   // Outer continuation for sub-context escapes
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
func PrimDynamicWind(ctx context.Context, mc *machine.MachineContext) error {
	before := mc.Arg(0)
	thunk := mc.Arg(1)
	after := mc.Arg(2)

	beforeCls, ok := before.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: before must be a procedure, got %T", before)
	}

	thunkCls, ok := thunk.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: thunk must be a procedure, got %T", thunk)
	}

	afterCls, ok := after.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: after must be a procedure, got %T", after)
	}

	// Create a new winding frame
	frame := machine.NewDynamicWindFrame(beforeCls, afterCls)

	// 1. Call before thunk (in current dynamic extent)
	sub := mc.NewSubContext()
	sub.SetWindingStack(mc.WindingStack())
	_, err := sub.Apply(beforeCls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
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
	sub2.SetWindingStack(mc.WindingStack()) // Include new frame
	sub2.SetEscapeCont(escapeCont)          // Allow call/cc to find continuation
	_, err = sub2.Apply(thunkCls)
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
	sub3.SetWindingStack(mc.WindingStack())
	_, err = sub3.Apply(afterCls)
	if err != nil {
		return err
	}
	err = sub3.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// 6. Handle thunk's result/error
	if thunkErr != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(thunkErr, &escapeErr) {
			// Propagate the escape (after thunk was already called)
			return thunkErr
		}
		if !errors.Is(thunkErr, machine.ErrMachineHalt) {
			return thunkErr
		}
	}

	mc.SetValues(thunkResult...)
	return nil
}

// PrimValues implements the values primitive.
// Returns multiple values as specified by R7RS. With no arguments returns no values.
// With one or more arguments, returns all arguments as multiple values.
func PrimValues(_ context.Context, mc *machine.MachineContext) error {
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
func PrimCallWithValues(ctx context.Context, mc *machine.MachineContext) error {
	producer := mc.Arg(0)
	consumer := mc.Arg(1)

	producerCls, err := helpers.RequireType[*machine.MachineClosure](producer, values.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	consumerCls, err := helpers.RequireType[*machine.MachineClosure](consumer, values.ErrNotAProcedure, "call-with-values")
	if err != nil {
		return err
	}

	// Call producer with no arguments
	sub := mc.NewSubContext()
	_, err = sub.Apply(producerCls)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// Get all values returned by producer
	producedValues := sub.GetValues()

	// Call consumer with all produced values as arguments
	sub2 := mc.NewSubContext()
	_, err = sub2.Apply(consumerCls, producedValues...)
	if err != nil {
		return err
	}
	err = sub2.Run()
	if err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// Return what consumer returned
	mc.SetValues(sub2.GetValues()...)
	return nil
}
