package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimVectorToList implements the vector->list primitive.
// Converts a vector to a list with the same elements in the same order.
func PrimVectorToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector->list: expected a vector but got %T", o)
	}
	var result values.Value = values.EmptyList
	for i := len(*v) - 1; i >= 0; i-- {
		result = values.NewCons((*v)[i], result)
	}
	mc.SetValue(result)
	return nil
}
