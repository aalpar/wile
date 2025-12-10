package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimBytevector implements the bytevector primitive.
// Creates bytevector from byte arguments.
func PrimBytevector(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "bytevector: expected a list but got %T", o)
	}
	var bytes []values.Byte
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		intVal, ok := v.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector: expected an integer but got %T", v)
		}
		if intVal.Value < 0 || intVal.Value > 255 {
			return values.NewForeignError("bytevector: value must be a byte (0-255)")
		}
		bytes = append(bytes, values.Byte{Value: uint8(intVal.Value)})
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector: not a proper list")
	}
	bv := values.ByteVector(bytes)
	mc.SetValue(&bv)
	return nil
}
