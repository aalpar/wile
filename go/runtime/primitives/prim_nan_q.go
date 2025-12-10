package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimNanQ implements the nan? primitive.
// Returns #t if the number is NaN, #f otherwise.
func PrimNanQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.FalseValue)
	case *values.Rational:
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(math.IsNaN(v.Value)))
	case *values.Complex:
		real := real(v.Value)
		imag := imag(v.Value)
		isNaN := math.IsNaN(real) || math.IsNaN(imag)
		mc.SetValue(utils.BoolToBoolean(isNaN))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "nan?: expected a number but got %T", o)
	}
	return nil
}
