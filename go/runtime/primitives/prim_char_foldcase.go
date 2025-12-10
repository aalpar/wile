package primitives

import (
	"context"
	"unicode"

	"wile/machine"
	"wile/values"
)

// PrimCharFoldcase implements the (char-foldcase) primitive.
// Returns the case-folded version of the character for case-insensitive comparison.
func PrimCharFoldcase(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char-foldcase: expected a character but got %T", o)
	}
	mc.SetValue(values.NewCharacter(unicode.ToLower(ch.Value)))
	return nil
}
