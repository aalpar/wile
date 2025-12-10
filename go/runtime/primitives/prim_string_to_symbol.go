package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimStringToSymbol implements the string->symbol primitive.
// Converts a string to an interned symbol.
func PrimStringToSymbol(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->symbol: expected a string but got %T", o)
	}
	sym := values.NewSymbol(s.Value)
	// Intern the symbol
	sym = mc.EnvironmentFrame().InternSymbol(sym)
	mc.SetValue(sym)
	return nil
}
