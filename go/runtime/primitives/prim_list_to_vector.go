package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimListToVector implements the (list->vector) primitive.
// Converts a list to a vector.
func PrimListToVector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewVector())
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->vector: expected a list but got %T", o)
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
		return values.WrapForeignErrorf(values.ErrNotAList, "list->vector: expected a proper list")
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}
