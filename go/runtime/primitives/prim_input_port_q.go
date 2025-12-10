package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimInputPortQ implements the (input-port?) primitive.
// Returns #t if argument is input port.
func PrimInputPortQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.CharacterInputPort)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
