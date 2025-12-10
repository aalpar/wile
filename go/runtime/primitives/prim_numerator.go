package primitives

import (
	"context"
	"math/big"

	"wile/machine"
	"wile/values"
)

// PrimNumerator implements the numerator primitive.
// Returns the numerator of a rational number.
func PrimNumerator(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Rational:
		num := v.Num()
		if num.IsInt64() {
			mc.SetValue(values.NewInteger(num.Int64()))
		} else {
			// Return as rational with denominator 1
			mc.SetValue(values.NewRationalFromBigInt(num, big.NewInt(1)))
		}
	case *values.Float:
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return values.NewForeignError("numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		if num.IsInt64() {
			mc.SetValue(values.NewInteger(num.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(num, big.NewInt(1)))
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "numerator: expected a rational number but got %T", o)
	}
	return nil
}
