package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/values"
)

// PrimAcos implements the acos primitive.
// Returns the arc cosine of a number.
func PrimAcos(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "acos: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Acos(x)))
	return nil
}
