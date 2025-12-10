package primitives

import (
	"context"
	"unicode"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimCharUpperCaseQ implements the (char-upper-case?) primitive.
// Returns #t if the character is uppercase, #f otherwise.
func PrimCharUpperCaseQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-upper-case?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsUpper(ch.Value)))
	return nil
}
