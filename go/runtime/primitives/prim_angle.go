package primitives

import (
	"context"
	"math"
	"math/cmplx"

	"skeme/machine"
	"skeme/values"
)

// PrimAngle implements the angle primitive.
// Returns the angle of a complex number in polar form.
func PrimAngle(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Phase(v.Value)))
	case *values.Integer:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.Float:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.Rational:
		if v.Rat().Sign() >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "angle: expected a number but got %T", o)
	}
	return nil
}
