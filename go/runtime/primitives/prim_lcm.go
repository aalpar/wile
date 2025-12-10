package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimLcm implements the (lcm) primitive.
// Returns the least common multiple of the given integers.
func PrimLcm(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewInteger(1))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "lcm: expected a list but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(1))
		return nil
	}
	first, ok := pr.Car().(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "lcm: expected an integer but got %T", pr.Car())
	}
	result := first.Value
	if result < 0 {
		result = -result
	}
	rest, ok := pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(values.NewInteger(result))
		return nil
	}
	rest.ForEach(func(i int, hasNext bool, next values.Value) error {
		n, ok := next.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "lcm: expected an integer but got %T", pr.Car())
		}
		v := n.Value
		if v < 0 {
			v = -v
		}
		result = result / GcdInt(result, v) * v
		return nil
	})
	mc.SetValue(values.NewInteger(result))
	return nil
}
