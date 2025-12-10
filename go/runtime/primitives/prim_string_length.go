package primitives

import (
	"context"
	"unicode/utf8"

	"skeme/machine"
	"skeme/values"
)

// PrimStringLength implements string-length.
// Returns the number of characters (runes) in the string.
func PrimStringLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(utf8.RuneCountInString(s.Value))))
	return nil
}
