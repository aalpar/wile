package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimStringToUtf8 implements the string->utf8 primitive.
// Converts a string to a UTF-8 encoded bytevector with optional start and end indices.
func PrimStringToUtf8(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->utf8: expected a string but got %T", o)
	}

	s := str.Value
	start := int64(0)
	end := int64(len(s))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			startVal, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "string->utf8: start must be an integer but got %T", pr.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := pr.Cdr()
			if !values.IsEmptyList(cdr) {
				pr2, ok := cdr.(*values.Pair)
				if ok && !values.IsEmptyList(pr2) {
					endVal, ok := pr2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "string->utf8: end must be an integer but got %T", pr2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(s)) {
		return values.NewForeignError("string->utf8: start index out of bounds")
	}
	if end < start || end > int64(len(s)) {
		return values.NewForeignError("string->utf8: end index out of bounds")
	}

	// Convert string to bytevector
	bytes := []byte(s[start:end])
	bv := make(values.ByteVector, len(bytes))
	for i, b := range bytes {
		bv[i] = values.Byte{Value: b}
	}
	mc.SetValue(&bv)
	return nil
}
