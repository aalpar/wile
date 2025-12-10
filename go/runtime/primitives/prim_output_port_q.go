package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimOutputPortQ implements the output-port? primitive.
// Returns #t if the argument is an output port, #f otherwise.
func PrimOutputPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.CharacterOutputPort)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
