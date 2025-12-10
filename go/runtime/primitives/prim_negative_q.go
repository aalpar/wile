package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimNegativeQ implements the negative? primitive.
// Returns #t if the number is negative, #f otherwise.
func PrimNegativeQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(utils.BoolToBoolean(v.Value < 0))
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(v.Value < 0))
	case *values.Rational:
		mc.SetValue(utils.BoolToBoolean(v.Rat().Sign() < 0))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "negative?: expected a real number but got %T", o)
	}
	return nil
}
