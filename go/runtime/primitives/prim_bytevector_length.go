package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimBytevectorLength implements the bytevector-length primitive.
// Returns length of bytevector.
func PrimBytevectorLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-length: expected a bytevector but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(len(*bv))))
	return nil
}
