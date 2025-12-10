package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimModulo implements the modulo primitive.
// Returns the modulo of two integers with the sign of the divisor.
func PrimModulo(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "modulo: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "modulo: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("modulo: division by zero")
	}
	result := n0.Value % n1.Value
	// Adjust result to have the same sign as n1 (Scheme semantics)
	if (result < 0 && n1.Value > 0) || (result > 0 && n1.Value < 0) {
		result += n1.Value
	}
	mc.SetValue(values.NewInteger(result))
	return nil
}
