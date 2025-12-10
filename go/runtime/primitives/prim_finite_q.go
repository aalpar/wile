package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimFiniteQ implements the (finite?) primitive.
// Returns #t if number is finite.
func PrimFiniteQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.TrueValue)
	case *values.Rational:
		mc.SetValue(values.TrueValue)
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(!math.IsInf(v.Value, 0) && !math.IsNaN(v.Value)))
	case *values.Complex:
		real := real(v.Value)
		imag := imag(v.Value)
		isFinite := !math.IsInf(real, 0) && !math.IsNaN(real) && !math.IsInf(imag, 0) && !math.IsNaN(imag)
		mc.SetValue(utils.BoolToBoolean(isFinite))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "finite?: expected a number but got %T", o)
	}
	return nil
}
