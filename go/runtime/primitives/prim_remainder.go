package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimRemainder implements the (remainder) primitive.
// Returns remainder with sign of dividend.
func PrimRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "remainder: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "remainder: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("remainder: division by zero")
	}
	mc.SetValue(values.NewInteger(n0.Value % n1.Value))
	return nil
}
