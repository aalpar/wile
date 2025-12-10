package primitives

import (
	"context"
	"strconv"

	"skeme/machine"
	"skeme/values"
)

// PrimStringToNumber implements the string->number primitive.
// Parses a string as a number with optional radix. Returns #f on parse failure.
func PrimStringToNumber(_ context.Context, mc *machine.MachineContext) error {
	s := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->number: expected a string but got %T", s)
	}
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "string->number: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
		}
	}
	// Try integer first
	if i, err := strconv.ParseInt(str.Value, radix, 64); err == nil {
		mc.SetValue(values.NewInteger(i))
		return nil
	}
	// Try float (only for base 10)
	if radix == 10 {
		if f, err := strconv.ParseFloat(str.Value, 64); err == nil {
			mc.SetValue(values.NewFloat(f))
			return nil
		}
	}
	mc.SetValue(values.FalseValue)
	return nil
}
