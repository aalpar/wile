package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimCharGe implements the (char>=?) primitive.
// Returns #t if the first character is greater than or equal to the second, #f otherwise.
func PrimCharGe(_ context.Context, mc *machine.MachineContext) error {
	c1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	c2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	ch1, ok := c1.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char>=?: expected a character but got %T", c1)
	}
	ch2, ok := c2.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "char>=?: expected a character but got %T", c2)
	}
	mc.SetValue(utils.BoolToBoolean(ch1.Value >= ch2.Value))
	return nil
}
