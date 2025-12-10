package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimUtf8ToString implements the utf8->string primitive.
// Converts a UTF-8 encoded bytevector to a string with optional start and end indices.
func PrimUtf8ToString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "utf8->string: expected a bytevector but got %T", o)
	}

	start := int64(0)
	end := int64(len(*bv))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			startVal, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "utf8->string: start must be an integer but got %T", pr.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := pr.Cdr()
			if !values.IsEmptyList(cdr) {
				pr2, ok := cdr.(*values.Pair)
				if ok && !values.IsEmptyList(pr2) {
					endVal, ok := pr2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "utf8->string: end must be an integer but got %T", pr2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(*bv)) {
		return values.NewForeignError("utf8->string: start index out of bounds")
	}
	if end < start || end > int64(len(*bv)) {
		return values.NewForeignError("utf8->string: end index out of bounds")
	}

	// Convert bytes to string
	bytes := make([]byte, end-start)
	for i := start; i < end; i++ {
		bytes[i-start] = (*bv)[i].Value
	}
	mc.SetValue(values.NewString(string(bytes)))
	return nil
}
