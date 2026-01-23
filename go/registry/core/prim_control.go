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

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimApply implements the apply primitive.
// Applies a procedure to a list of arguments.
func PrimApply(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	restVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "apply: expected a procedure but got %T", proc)
	}

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
		v, err := finalTuple.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
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
	mc.SetValue(sub.GetValue())
	return nil
}

// PrimMap implements the (map) primitive.
// Applies a procedure to elements of one or more lists and returns a list of results.
func PrimMap(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	listsVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "map: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(listsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "map: expected at least one list")
	}

	// Collect all lists into a slice
	var lists []values.Value
	current := listsVal
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "map: improper argument list")
		}
		lists = append(lists, tuple.Car())
		current = tuple.Cdr()
	}

	// Check if any list is empty
	for i, lst := range lists {
		if values.IsEmptyList(lst) {
			mc.SetValue(values.EmptyList)
			return nil
		}
		if _, ok := lst.(values.Tuple); !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "map: argument %d is not a list", i+1)
		}
	}

	var results values.Vector
	sub := mc.NewSubContext()

	// Iterate through all lists in parallel
	for {
		// Collect one element from each list
		args := make(values.Vector, len(lists))
		allDone := false
		for i, lst := range lists {
			if values.IsEmptyList(lst) {
				allDone = true
				break
			}
			tuple, ok := lst.(values.Tuple)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "map: argument %d is an improper list", i+1)
			}
			args[i] = tuple.Car()
			lists[i] = tuple.Cdr()
		}
		if allDone {
			break
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
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
		results = append(results, sub.GetValue())
	}

	mc.SetValue(values.List(results...))
	return nil
}

// PrimForEach implements the (for-each) primitive.
// Applies procedure to each list element for side effects.
func PrimForEach(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	listsVal := mc.Arg(1)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "for-each: expected a procedure but got %T", proc)
	}

	if values.IsEmptyList(listsVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "for-each: expected at least one list")
	}

	// Collect all lists into a slice
	var lists []values.Value
	current := listsVal
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "for-each: improper argument list")
		}
		lists = append(lists, tuple.Car())
		current = tuple.Cdr()
	}

	// Check if any list is empty
	for i, lst := range lists {
		if values.IsEmptyList(lst) {
			mc.SetValues()
			return nil
		}
		if _, ok := lst.(values.Tuple); !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "for-each: argument %d is not a list", i+1)
		}
	}

	sub := mc.NewSubContext()

	// Iterate through all lists in parallel
	for {
		// Collect one element from each list
		args := make(values.Vector, len(lists))
		allDone := false
		for i, lst := range lists {
			if values.IsEmptyList(lst) {
				allDone = true
				break
			}
			tuple, ok := lst.(values.Tuple)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "for-each: argument %d is an improper list", i+1)
			}
			args[i] = tuple.Car()
			lists[i] = tuple.Cdr()
		}
		if allDone {
			break
		}

		// Apply proc to collected arguments
		_, err := sub.Apply(mcls, args...)
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
	}

	mc.SetValues()
	return nil
}

// PrimCallCC implements the call/cc primitive.
// Captures current continuation and passes to procedure.
func PrimCallCC(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call/cc: expected a procedure but got %T", proc)
	}

	// Capture the current continuation
	// mc.cont is the continuation that will be restored when this foreign function returns
	// (i.e., the continuation to the caller of call/cc). We copy it to avoid mutation issues.
	cont := mc.Parent()
	if cont != nil {
		cont = cont.Copy()
	}

	// Create a closure that, when called, restores this continuation
	contClosure := newEscapeContinuationClosure(mc.EnvironmentFrame().TopLevel(), cont)

	// Call the procedure with the continuation closure
	sub := mc.NewSubContext()
	_, err := sub.Apply(mcls, contClosure)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		// Check if this is a continuation escape
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			// Restore the continuation and set the escape value
			// Then propagate the escape so the caller knows not to increment PC
			mc.Restore(escapeErr.Continuation)
			mc.SetValue(escapeErr.Value)
			escapeErr.Handled = true
			return escapeErr
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// If we get here, the procedure returned normally (didn't invoke the continuation)
	mc.SetValue(sub.GetValue())
	return nil
}

// newEscapeContinuationClosure creates a closure that escapes to the captured continuation when called.
func newEscapeContinuationClosure(env *environment.EnvironmentFrame, cont *machine.MachineContinuation) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		// Get the value passed to the continuation (from the closure's argument)
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		// Return an escape error that will propagate up through sub-contexts
		return &machine.ErrContinuationEscape{
			Continuation: cont,
			Value:        val,
		}
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}

// PrimDynamicWind implements the (dynamic-wind) primitive.
// Calls a thunk with before and after handlers that execute on entry and exit.
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

	// Call before thunk
	sub := mc.NewSubContext()
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

	// Call main thunk
	sub2 := mc.NewSubContext()
	_, err = sub2.Apply(thunkCls)
	if err != nil {
		return err
	}
	thunkErr := sub2.Run()
	thunkResult := sub2.GetValues()

	// Always call after thunk, even if main thunk escaped
	sub3 := mc.NewSubContext()
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

	// Now handle thunk's result/error
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

	producerCls, ok := producer.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-values: producer must be a procedure, got %T", producer)
	}

	consumerCls, ok := consumer.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-values: consumer must be a procedure, got %T", consumer)
	}

	// Call producer with no arguments
	sub := mc.NewSubContext()
	_, err := sub.Apply(producerCls)
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
