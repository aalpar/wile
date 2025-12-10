package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimFloorDiv implements the (floor/) primitive.
// Returns floor quotient and remainder.
func PrimFloorDiv(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor/: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "floor/: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("floor/: division by zero")
	}
	q, r := FloorDivide(n0.Value, n1.Value)
	mc.SetValues(values.NewInteger(q), values.NewInteger(r))
	return nil
}
