package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimTruncateDiv implements the truncate/ primitive.
// Returns both the truncated quotient and remainder of dividing two integers.
func PrimTruncateDiv(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	n0, ok := o0.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "truncate/: expected an integer but got %T", o0)
	}
	n1, ok := o1.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "truncate/: expected an integer but got %T", o1)
	}
	if n1.Value == 0 {
		return values.NewForeignError("truncate/: division by zero")
	}
	q := n0.Value / n1.Value
	r := n0.Value % n1.Value
	mc.SetValues(values.NewInteger(q), values.NewInteger(r))
	return nil
}
