package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMax implements the max primitive.
// Returns the maximum of the given numbers.
func PrimMax(_ context.Context, mc *machine.MachineContext) error {
	first := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	max, ok := first.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "max: expected a number but got %T", first)
	}
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(max)
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "max: expected a pair but got %T", rest)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "max: expected a number but got %T", pr.Car())
		}
		if max.LessThan(curr) {
			max = curr
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "max: not a proper list")
	}
	mc.SetValue(max)
	return nil
}
