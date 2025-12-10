package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimMul implements the * primitive.
// With no arguments returns 1. With one argument returns that argument.
// With multiple arguments returns their product.
func PrimMul(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "mul: expected a pair but got %T", o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(values.NewInteger(1))
		return nil
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "mul: expected a number but got %T", o)
	}
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "mul: expected a list but got %T", pr)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "mul: expected a number but got %T", o)
		}
		nbr = nbr.Multiply(v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "mul: error processing pair for * operation: %s", pr.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "mul: expected a list but got %s", v.SchemeString())
	}
	mc.SetValue(nbr)
	return nil
}
