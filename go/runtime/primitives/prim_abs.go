package primitives

import (
	"context"
	"math"
	"math/big"

	"wile/machine"
	"wile/values"
)

// PrimAbs implements the abs primitive.
// Returns the absolute value of a real number.
func PrimAbs(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			mc.SetValue(values.NewInteger(-v.Value))
		} else {
			mc.SetValue(v)
		}
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.Rational:
		result := new(big.Rat).Abs(v.Rat())
		mc.SetValue(values.NewRationalFromRat(result))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "abs: expected a real number but got %T", o)
	}
	return nil
}
