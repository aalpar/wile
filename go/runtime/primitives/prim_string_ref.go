package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimStringRef implements the string-ref primitive.
// Returns the character at the given index in the string.
func PrimStringRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-ref: expected a string but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "string-ref: expected an integer but got %T", k)
	}
	runes := []rune(s.Value)
	if idx.Value < 0 || idx.Value >= int64(len(runes)) {
		return values.NewForeignError("string-ref: index out of bounds")
	}
	mc.SetValue(values.NewCharacter(runes[idx.Value]))
	return nil
}
