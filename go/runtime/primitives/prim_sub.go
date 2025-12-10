package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimSub implements the - primitive.
// With one argument returns its negation: (- x) => -x
// With multiple arguments subtracts from left to right: (- x y z) => x - y - z
func PrimSub(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	nbr0, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o0)
	}
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := o1.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "sub: expected a pair but got %T", o1)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0).Subtract(nbr0))
		return nil
	}
	o2 := pr.Car()
	nbr2, ok := o2.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o2)
	}
	nbr2 = nbr0.Subtract(nbr2)
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr2)
		return nil
	}
	v, err := pr.ForEach(func(i int, hasNext bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "sub: expected a number but got %T", o)
		}
		nbr2 = nbr2.Subtract(v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "sub: error processing pair for - operation: %s", pr.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "sub: expected a list but got %s", v.SchemeString())
	}
	mc.SetValue(nbr2)
	return nil
}
