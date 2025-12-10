package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimRound implements the (round) primitive.
// Rounds to nearest integer (half to even).
func PrimRound(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Round(v.Value)))
	case *values.Rational:
		f := v.Float64()
		mc.SetValue(values.NewFloat(math.Round(f)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "round: expected a real number but got %T", o)
	}
	return nil
}
