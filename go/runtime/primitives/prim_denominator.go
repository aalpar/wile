package primitives

import (
	"context"
	"math/big"

	"skeme/machine"
	"skeme/values"
)

// PrimDenominator implements the (denominator) primitive.
// Returns the denominator of a rational number.
func PrimDenominator(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewInteger(1))
	case *values.Rational:
		denom := v.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	case *values.Float:
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "denominator: expected a rational number but got %T", o)
	}
	return nil
}
