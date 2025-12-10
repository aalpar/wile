package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimBytevectorU8Ref implements the bytevector-u8-ref primitive.
// Returns byte at index.
func PrimBytevectorU8Ref(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-u8-ref: expected a bytevector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-ref: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*bv)) {
		return values.NewForeignError("bytevector-u8-ref: index out of bounds")
	}
	mc.SetValue(values.NewInteger(int64((*bv)[idx.Value].Value)))
	return nil
}
