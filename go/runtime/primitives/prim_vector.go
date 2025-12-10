package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimVector implements the vector primitive.
// Creates a vector from the given arguments. With no arguments creates an empty vector.
func PrimVector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewVector())
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "vector: expected a list but got %T", o)
	}
	var elems values.Vector
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		elems = append(elems, v)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "vector: not a proper list")
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}
