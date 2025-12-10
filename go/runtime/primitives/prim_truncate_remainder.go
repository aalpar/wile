package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimTruncateRemainder implements the truncate-remainder primitive.
// Returns the remainder of truncate division of two integers.
func PrimTruncateRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "truncate-remainder: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "truncate-remainder: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("truncate-remainder: division by zero")
	}
	mc.SetValue(values.NewInteger(n0.Value % n1.Value))
	return nil
}
