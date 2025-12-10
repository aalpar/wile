package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimComplexQ implements the (complex?) primitive.
// Returns #t if the argument is a complex number, #f otherwise.
func PrimComplexQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(values.Number)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
