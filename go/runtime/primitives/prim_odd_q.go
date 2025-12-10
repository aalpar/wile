package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimOddQ implements the odd? primitive.
// Returns #t if the integer is odd, #f otherwise.
func PrimOddQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(v.Value%2 != 0))
	return nil
}
