package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimCharToInteger implements the (char->integer) primitive.
// Returns the Unicode code point of the character as an integer.
func PrimCharToInteger(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char->integer: expected a character but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(ch.Value)))
	return nil
}
