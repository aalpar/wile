package primitives

import (
	"context"
	"strconv"

	"skeme/machine"
	"skeme/values"
)

// PrimNumberToString implements the number->string primitive.
// Converts a number to its string representation with optional radix.
func PrimNumberToString(_ context.Context, mc *machine.MachineContext) error {
	n := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
			if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
				return values.NewForeignError("number->string: radix must be 2, 8, 10, or 16")
			}
		}
	}
	switch v := n.(type) {
	case *values.Integer:
		mc.SetValue(values.NewString(strconv.FormatInt(v.Value, radix)))
	case *values.Float:
		mc.SetValue(values.NewString(strconv.FormatFloat(v.Value, 'g', -1, 64)))
	case *values.Rational:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.Complex:
		mc.SetValue(values.NewString(v.SchemeString()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected a number but got %T", n)
	}
	return nil
}
