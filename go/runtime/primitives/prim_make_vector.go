package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimMakeVector implements the (make-vector) primitive.
// Creates a vector of the given size, optionally filled with a specified value.
func PrimMakeVector(_ context.Context, mc *machine.MachineContext) error {
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	size, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-vector: expected an integer but got %T", k)
	}
	if size.Value < 0 {
		return values.NewForeignError("make-vector: size must be non-negative")
	}
	var fill values.Value = values.FalseValue
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			fill = pr.Car()
		}
	}
	elems := make(values.Vector, size.Value)
	for i := range elems {
		elems[i] = fill
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}
