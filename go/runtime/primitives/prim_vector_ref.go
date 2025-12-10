package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimVectorRef implements the vector-ref primitive.
// Returns the element of a vector at the given index.
func PrimVectorRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-ref: expected a vector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-ref: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*v)) {
		return values.NewForeignError("vector-ref: index out of bounds")
	}
	mc.SetValue((*v)[idx.Value])
	return nil
}
