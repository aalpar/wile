package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// PrimSymbolQ implements the symbol? primitive.
// Returns #t if the argument is a symbol, #f otherwise.
func PrimSymbolQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.Symbol)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
