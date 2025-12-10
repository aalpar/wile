package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimPromiseQ implements the (promise?) primitive.
// Returns #t if argument is a promise.
func PrimPromiseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Promise)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
