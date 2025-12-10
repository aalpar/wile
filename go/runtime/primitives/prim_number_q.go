package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimNumberQ implements the number? primitive.
// Returns #t if the argument is a number, #f otherwise.
func PrimNumberQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(values.Number)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
