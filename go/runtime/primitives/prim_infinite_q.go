package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimInfiniteQ implements the (infinite?) primitive.
// Returns #t if number is infinite.
func PrimInfiniteQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.FalseValue)
	case *values.Rational:
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(math.IsInf(v.Value, 0)))
	case *values.Complex:
		real := real(v.Value)
		imag := imag(v.Value)
		isInfinite := math.IsInf(real, 0) || math.IsInf(imag, 0)
		mc.SetValue(utils.BoolToBoolean(isInfinite))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "infinite?: expected a number but got %T", o)
	}
	return nil
}
