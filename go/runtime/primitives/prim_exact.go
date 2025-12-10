package primitives

import (
	"context"
	"math/big"

	"skeme/machine"
	"skeme/values"
)

// PrimExact implements the (exact) primitive.
// Converts an inexact number to an exact representation.
func PrimExact(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer, *values.Rational:
		mc.SetValue(v)
	case *values.Float:
		// Convert float to rational
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("exact: cannot convert infinity or NaN to exact")
		}
		mc.SetValue(values.NewRationalFromRat(r))
	case *values.Complex:
		return values.NewForeignError("exact: complex numbers not supported")
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact: expected a number but got %T", o)
	}
	return nil
}
