package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimEvenQ implements the (even?) primitive.
// Returns #t if the integer is even.
func PrimEvenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "even?: expected an integer but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(v.Value%2 == 0))
	return nil
}
