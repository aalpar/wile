package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimImagPart implements the (imag-part) primitive.
// Returns imaginary part of complex number.
func PrimImagPart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(imag(v.Value)))
	case *values.Integer, *values.Float, *values.Rational:
		mc.SetValue(values.NewFloat(0))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "imag-part: expected a number but got %T", o)
	}
	return nil
}
