package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimNumLt implements the < primitive for numeric less-than comparison.
// Returns #t if arguments are in strictly increasing order.
func PrimNumLt(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	prev, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "<: expected a number but got %T", o0)
	}
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := rest.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "<: expected a pair but got %T", rest)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		curr, ok := v.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "<: expected a number but got %T", v)
		}
		if !prev.LessThan(curr) {
			return values.ErrCannotCompare
		}
		prev = curr
		return nil
	})
	if errors.Is(err, values.ErrCannotCompare) {
		mc.SetValue(values.FalseValue)
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "<: expected a proper list")
	}
	mc.SetValue(values.TrueValue)
	return nil
}
