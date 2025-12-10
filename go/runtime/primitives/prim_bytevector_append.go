package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimBytevectorAppend implements the bytevector-append primitive.
// Concatenates bytevectors.
func PrimBytevectorAppend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "bytevector-append: expected a list but got %T", o)
	}
	var result []values.Byte
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		bv, ok := v.(*values.ByteVector)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-append: expected a bytevector but got %T", v)
		}
		result = append(result, (*bv)...)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector-append: not a proper list")
	}
	bv := values.ByteVector(result)
	mc.SetValue(&bv)
	return nil
}
