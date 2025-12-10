package primitives

import (
	"context"
	"unicode"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimCharWhitespaceQ implements the (char-whitespace?) primitive.
// Returns #t if the character is whitespace, #f otherwise.
func PrimCharWhitespaceQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-whitespace?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsSpace(ch.Value)))
	return nil
}
