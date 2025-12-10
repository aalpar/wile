package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimAsin implements the asin primitive.
// Returns the arc sine of a number.
func PrimAsin(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "asin: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Asin(x)))
	return nil
}
