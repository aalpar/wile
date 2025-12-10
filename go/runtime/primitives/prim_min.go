package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMin implements the min primitive.
// Returns the minimum of the given numbers.
func PrimMin(_ context.Context, mc *machine.MachineContext) error {
	first := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	min, ok := first.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "min: expected a number but got %T", first)
	}
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := rest.(*values.Pair)
	if !ok {
		if values.IsEmptyList(rest) {
			mc.SetValue(min)
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "min: expected a pair but got %T", rest)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "min: expected a number but got %T", v)
		}
		if curr.LessThan(min) {
			min = curr
		}
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "min: not a proper list")
	}
	mc.SetValue(min)
	return nil
}
