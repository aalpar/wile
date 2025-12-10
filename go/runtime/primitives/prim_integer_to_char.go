package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimIntegerToChar implements the (integer->char) primitive.
// Converts a Unicode code point (integer) to a character.
func PrimIntegerToChar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	n, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "integer->char: expected an integer but got %T", o)
	}
	mc.SetValue(values.NewCharacter(rune(n.Value)))
	return nil
}
