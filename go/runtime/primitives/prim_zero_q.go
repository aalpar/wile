package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimZeroQ implements the zero? primitive.
// Returns #t if the number is zero, #f otherwise.
func PrimZeroQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	n, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "zero?: expected a number but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(n.IsZero()))
	return nil
}
