package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimCharLt implements the (char<?) primitive.
// Returns #t if the first character is less than the second, #f otherwise.
func PrimCharLt(_ context.Context, mc *machine.MachineContext) error {
	c1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	c2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char<?: expected a character but got %T", c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char<?: expected a character but got %T", c2)
	}
	mc.SetValue(utils.BoolToBoolean(ch1.Value < ch2.Value))
	return nil
}
