package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/values"
)

// PrimTan implements the tan primitive.
// Returns the tangent of a number in radians.
func PrimTan(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "tan: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Tan(x)))
	return nil
}
