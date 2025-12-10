package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimCeiling implements the (ceiling) primitive.
// Returns the smallest integer not less than the argument.
func PrimCeiling(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Ceil(v.Value)))
	case *values.Rational:
		f := v.Float64()
		mc.SetValue(values.NewFloat(math.Ceil(f)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "ceiling: expected a real number but got %T", o)
	}
	return nil
}
