package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimBytevectorCopy implements the bytevector-copy primitive.
// Returns a copy of a bytevector.
func PrimBytevectorCopy(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-copy: expected a bytevector but got %T", o)
	}

	start := int64(0)
	end := int64(len(*bv))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			startVal, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy: start must be an integer but got %T", pr.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := pr.Cdr()
			if !values.IsEmptyList(cdr) {
				pr2, ok := cdr.(*values.Pair)
				if ok && !values.IsEmptyList(pr2) {
					endVal, ok := pr2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy: end must be an integer but got %T", pr2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(*bv)) {
		return values.NewForeignError("bytevector-copy: start index out of bounds")
	}
	if end < start || end > int64(len(*bv)) {
		return values.NewForeignError("bytevector-copy: end index out of bounds")
	}

	result := make(values.ByteVector, end-start)
	copy(result, (*bv)[start:end])
	mc.SetValue(&result)
	return nil
}
