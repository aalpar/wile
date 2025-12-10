package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimQuotient implements the (quotient) primitive.
// Returns truncated integer quotient.
func PrimQuotient(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "quotient: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "quotient: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("quotient: division by zero")
	}
	mc.SetValue(values.NewInteger(n0.Value / n1.Value))
	return nil
}
