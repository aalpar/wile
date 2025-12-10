package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimAtan implements the atan primitive.
// Returns the arc tangent of a number (one or two arguments).
func PrimAtan(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	y, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "atan: %v", err)
	}
	if rest == values.EmptyList {
		mc.SetValue(values.NewFloat(math.Atan(y)))
	} else {
		xArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("atan: expected a list for rest arguments")
		}
		x, err := ToFloat64(xArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(values.NewFloat(math.Atan2(y, x)))
	}
	return nil
}
