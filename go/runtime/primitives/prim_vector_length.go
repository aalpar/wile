package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimVectorLength implements the vector-length primitive.
// Returns the number of elements in a vector as an integer.
func PrimVectorLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-length: expected a vector but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(len(*v))))
	return nil
}
