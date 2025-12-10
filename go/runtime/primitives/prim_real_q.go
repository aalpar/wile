package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimRealQ implements the (real?) primitive.
// Returns #t if argument is a real number.
func PrimRealQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer, *values.Float, *values.Rational:
		mc.SetValue(values.TrueValue)
	case *values.Complex:
		if imag(v.Value) == 0 {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
