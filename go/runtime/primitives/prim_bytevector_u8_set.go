package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimBytevectorU8Set implements the bytevector-u8-set! primitive.
// Sets byte at index.
func PrimBytevectorU8Set(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-u8-set!: expected a bytevector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-set!: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*bv)) {
		return values.NewForeignError("bytevector-u8-set!: index out of bounds")
	}
	byteVal, ok := obj.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-set!: expected an integer but got %T", obj)
	}
	if byteVal.Value < 0 || byteVal.Value > 255 {
		return values.NewForeignError("bytevector-u8-set!: value must be a byte (0-255)")
	}
	(*bv)[idx.Value] = values.Byte{Value: uint8(byteVal.Value)}
	mc.SetValues()
	return nil
}
