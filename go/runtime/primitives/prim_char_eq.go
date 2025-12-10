package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimCharEq implements char=?.
// Returns #t if both characters have the same code point.
func PrimCharEq(_ context.Context, mc *machine.MachineContext) error {
	c1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	c2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char=?: expected a character but got %T", c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char=?: expected a character but got %T", c2)
	}
	mc.SetValue(utils.BoolToBoolean(ch1.Value == ch2.Value))
	return nil
}
