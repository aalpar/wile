package primitives

import (
	"context"
	"unicode"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimCharNumericQ implements the (char-numeric?) primitive.
// Returns #t if the character is a numeric digit, #f otherwise.
func PrimCharNumericQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-numeric?: expected a character but got %T", o)
	}
	mc.SetValue(utils.BoolToBoolean(unicode.IsDigit(ch.Value)))
	return nil
}
