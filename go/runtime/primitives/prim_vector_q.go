package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimVectorQ implements the vector? primitive.
// Returns #t if the argument is a vector, #f otherwise.
func PrimVectorQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Vector)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
