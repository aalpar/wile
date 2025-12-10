package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimPositiveQ implements the (positive?) primitive.
// Returns #t if number is positive.
func PrimPositiveQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(utils.BoolToBoolean(v.Value > 0))
	case *values.Float:
		mc.SetValue(utils.BoolToBoolean(v.Value > 0))
	case *values.Rational:
		mc.SetValue(utils.BoolToBoolean(v.Rat().Sign() > 0))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "positive?: expected a real number but got %T", o)
	}
	return nil
}
