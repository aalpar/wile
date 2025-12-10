package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimStringGt implements the string>? primitive.
// Returns #t if the first string is greater than the second string.
func PrimStringGt(_ context.Context, mc *machine.MachineContext) error {
	s1 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s2 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	str1, ok := s1.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string>?: expected a string but got %T", s1)
	}
	str2, ok := s2.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string>?: expected a string but got %T", s2)
	}
	mc.SetValue(utils.BoolToBoolean(str1.Value > str2.Value))
	return nil
}
