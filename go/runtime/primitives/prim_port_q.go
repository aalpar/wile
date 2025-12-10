package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimPortQ implements the port? primitive.
// Returns #t if the argument is a port (input or output), #f otherwise.
func PrimPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, okIn := o.(*values.CharacterInputPort)
	_, okOut := o.(*values.CharacterOutputPort)
	mc.SetValue(utils.BoolToBoolean(okIn || okOut))
	return nil
}
