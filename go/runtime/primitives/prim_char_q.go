package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimCharQ implements the (char?) primitive.
// Returns #t if the argument is a character, #f otherwise.
func PrimCharQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Character)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
