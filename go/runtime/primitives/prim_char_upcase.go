package primitives

import (
	"context"
	"unicode"

	"skeme/machine"
	"skeme/values"
)

// PrimCharUpcase implements the (char-upcase) primitive.
// Returns the uppercase version of the character.
func PrimCharUpcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-upcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToUpper(ch.Value)))
	return nil
}
