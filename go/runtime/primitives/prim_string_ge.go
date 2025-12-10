package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimStringSe implements the string>=? primitive.
// Returns #t if the first string is greater than or equal to the second string.
func PrimStringSe(_ context.Context, mc *machine.MachineContext) error {
	s1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string>=?: expected a string but got %T", s1)
	}
	str2, ok := s2.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string>=?: expected a string but got %T", s2)
	}
	mc.SetValue(utils.BoolToBoolean(str1.Value >= str2.Value))
	return nil
}
