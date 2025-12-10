package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimApply implements the apply primitive.
// Applies a procedure to a list of arguments.
func PrimApply(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "apply: expected a procedure but got %T", proc)
	}

	// R7RS: (apply proc arg1 ... args) combines arg1 ... with the final list args
	// restVal is a list containing (arg1 ... args) where args is the final list
	restList, ok := restVal.(*values.Pair)
	if !ok || values.IsEmptyList(restVal) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "apply: expected at least one argument list")
	}

	// Collect all elements from rest except the last one, which is the final args list
	var prefixArgs values.Vector
	var finalList values.Value
	for {
		car := restList.Car()
		cdr := restList.Cdr()
		if values.IsEmptyList(cdr) {
			// This is the last element - it's the final args list
			finalList = car
			break
		}
		prefixArgs = append(prefixArgs, car)
		restList, ok = cdr.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: improper rest argument list")
		}
	}

	// Now append elements from finalList to prefixArgs
	if !values.IsEmptyList(finalList) {
		finalPair, ok := finalList.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "apply: final argument must be a list but got %T", finalList)
		}
		v, err := finalPair.ForEach(func(i int, hasNext bool, elem values.Value) error {
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
	if _, err := sub.Apply(mcls, prefixArgs...); err != nil {
		return err
	}
	if err := sub.Run(ctx); err != nil {
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
