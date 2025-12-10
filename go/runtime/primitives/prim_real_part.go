package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimRealPart implements the (real-part) primitive.
// Returns real part of complex number.
func PrimRealPart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(real(v.Value)))
	case *values.Integer:
		mc.SetValue(values.NewFloat(float64(v.Value)))
	case *values.Float:
		mc.SetValue(v)
	case *values.Rational:
		mc.SetValue(values.NewFloat(v.Float64()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "real-part: expected a number but got %T", o)
	}
	return nil
}
