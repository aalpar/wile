package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimExactIntegerQ implements the (exact-integer?) primitive.
// Returns #t if the argument is an exact integer.
func PrimExactIntegerQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Integer)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
