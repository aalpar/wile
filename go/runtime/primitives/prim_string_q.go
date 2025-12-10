package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimStringQ implements the string? primitive.
// Returns #t if the argument is a string, #f otherwise.
func PrimStringQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.String)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
