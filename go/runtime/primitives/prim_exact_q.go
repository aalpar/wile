package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimExactQ implements the (exact?) primitive.
// Returns #t if the number is exact.
func PrimExactQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch o.(type) {
	case *values.Integer, *values.Rational:
		mc.SetValue(values.TrueValue)
	case *values.Float, *values.Complex:
		mc.SetValue(values.FalseValue)
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact?: expected a number but got %T", o)
	}
	return nil
}
