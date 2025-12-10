package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimIntegerQ implements the integer? predicate.
// Returns #t if the argument is an integer (exact or inexact).
// Inexact integers are floating-point numbers with zero fractional part.
func PrimIntegerQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.TrueValue)
	case *values.Rational:
		if v.IsInteger() {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
	case *values.Float:
		f := v.Value
		if f == float64(int64(f)) {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
	case *values.Complex:
		if imag(v.Value) == 0 {
			r := real(v.Value)
			if r == float64(int64(r)) {
				mc.SetValue(values.TrueValue)
			} else {
				mc.SetValue(values.FalseValue)
			}
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
