package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimSin implements the (sin) primitive.
// Returns the sine of a number.
func PrimSin(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "sin: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Sin(x)))
	return nil
}
