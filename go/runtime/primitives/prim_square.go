package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimSquare implements the (square) primitive.
// Returns the square of a number.
func PrimSquare(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	n, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "square: expected a number but got %T", o)
	}
	mc.SetValue(n.Multiply(n))
	return nil
}
