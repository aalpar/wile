package primitives

import (
	"context"
	"unicode"

	"wile/machine"
	"wile/values"
)

// PrimCharDowncase implements the (char-downcase) primitive.
// Returns the lowercase version of the character.
func PrimCharDowncase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-downcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}
