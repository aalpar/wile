package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/values"
)

// PrimExp implements the (exp) primitive.
// Returns e raised to the given power.
func PrimExp(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "exp: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Exp(x)))
	return nil
}
