package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimBooleanQ implements the boolean? primitive.
// Returns #t if argument is a boolean.
func PrimBooleanQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Boolean)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
