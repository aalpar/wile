package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimDigitValue implements the (digit-value) primitive.
// Returns the numeric value of a digit character, or #f if not a digit.
func PrimDigitValue(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := o.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "digit-value: expected a character but got %T", o)
	}
	if ch.Value >= '0' && ch.Value <= '9' {
		mc.SetValue(values.NewInteger(int64(ch.Value - '0')))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
