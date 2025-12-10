package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimGcd implements the (gcd) primitive.
// Returns greatest common divisor.
func PrimGcd(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.NewInteger(0))
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "gcd: expected a list but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	first, ok := pr.Car().(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "gcd: expected an integer but got %T", pr.Car())
	}
	result := first.Value
	if result < 0 {
		result = -result
	}
	rest := pr.Cdr().(*values.Pair)
	rest.ForEach(func(i int, hasNext bool, next values.Value) error {
		n, ok := next.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "gcd: expected an integer but got %T", pr.Car())
		}
		v := n.Value
		if v < 0 {
			v = -v
		}
		result = GcdInt(result, v)
		return nil
	})
	mc.SetValue(values.NewInteger(result))
	return nil
}
