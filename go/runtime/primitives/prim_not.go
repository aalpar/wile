package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimNot implements the not primitive.
// Returns #t if the argument is #f, #f otherwise.
func PrimNot(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	// In Scheme, only #f is false; everything else is true
	mc.SetValue(utils.BoolToBoolean(o == values.FalseValue))
	return nil
}
