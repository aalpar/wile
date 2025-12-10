package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimFloor implements the (floor) primitive.
// Returns largest integer <= argument.
func PrimFloor(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Floor(v.Value)))
	case *values.Rational:
		f := v.Float64()
		mc.SetValue(values.NewFloat(math.Floor(f)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor: expected a real number but got %T", o)
	}
	return nil
}
