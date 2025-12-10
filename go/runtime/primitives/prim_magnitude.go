package primitives

import (
	"context"
	"math"
	"math/cmplx"

	"wile/machine"
	"wile/values"
)

// PrimMagnitude implements the (magnitude) primitive.
// Returns the magnitude (absolute value) of a complex number.
func PrimMagnitude(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Abs(v.Value)))
	case *values.Integer:
		val := v.Value
		if val < 0 {
			val = -val
		}
		mc.SetValue(values.NewFloat(float64(val)))
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.Rational:
		mc.SetValue(values.NewFloat(math.Abs(v.Float64())))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "magnitude: expected a number but got %T", o)
	}
	return nil
}
