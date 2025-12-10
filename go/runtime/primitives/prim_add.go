package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimAdd implements the + primitive.
// With no arguments returns 0. With one argument returns that argument.
// With multiple arguments returns their sum.
func PrimAdd(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "add: expected a pair but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "add: expected a number but got %T", o)
	}
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr)
		return nil
	}
	v, err := pr.ForEach(func(i int, hasNext bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "add: expected a number but got %T", o)
		}
		nbr = nbr.Add(v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "add: error processing pair for + operation: %s", pr.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "add: expected a list but got %s", v.SchemeString())
	}
	mc.SetValue(nbr)
	return nil
}
