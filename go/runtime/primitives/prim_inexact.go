package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimInexact implements the (inexact) primitive.
// Converts exact number to inexact.
func PrimInexact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewFloat(float64(v.Value)))
	case *values.Float:
		mc.SetValue(v)
	case *values.Rational:
		mc.SetValue(values.NewFloat(v.Float64()))
	case *values.Complex:
		mc.SetValue(v)
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "inexact: expected a number but got %T", o)
	}
	return nil
}
