package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimInexactQ implements the (inexact?) primitive.
// Returns #t if number is inexact.
func PrimInexactQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch o.(type) {
	case *values.Float, *values.Complex:
		mc.SetValue(values.TrueValue)
	case *values.Integer, *values.Rational:
		mc.SetValue(values.FalseValue)
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "inexact?: expected a number but got %T", o)
	}
	return nil
}
