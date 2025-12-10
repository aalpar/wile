package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimSymbolToString implements the symbol->string primitive.
// Converts a symbol to a string.
func PrimSymbolToString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	sym, ok := o.(*values.Symbol)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotASymbol, "symbol->string: expected a symbol but got %T", o)
	}
	mc.SetValue(values.NewString(sym.Key))
	return nil
}
