package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimFloorRemainder implements the (floor-remainder) primitive.
// Returns floor division remainder.
func PrimFloorRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor-remainder: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor-remainder: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("floor-remainder: division by zero")
	}
	_, r := FloorDivide(n0.Value, n1.Value)
	mc.SetValue(values.NewInteger(r))
	return nil
}
