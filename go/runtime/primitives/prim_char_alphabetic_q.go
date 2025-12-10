package primitives

import (
	"context"
	"unicode"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimCharAlphabeticQ implements the (char-alphabetic?) primitive.
// Returns #t if the character is alphabetic, #f otherwise.
func PrimCharAlphabeticQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-alphabetic?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsLetter(ch.Value)))
	return nil
}
