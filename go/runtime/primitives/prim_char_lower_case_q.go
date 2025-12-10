package primitives

import (
	"context"
	"unicode"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimCharLowerCaseQ implements the (char-lower-case?) primitive.
// Returns #t if the character is lowercase, #f otherwise.
func PrimCharLowerCaseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-lower-case?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsLower(ch.Value)))
	return nil
}
