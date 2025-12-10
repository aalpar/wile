package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimCos implements the (cos) primitive.
// Returns the cosine of the argument in radians.
func PrimCos(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "cos: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Cos(x)))
	return nil
}
