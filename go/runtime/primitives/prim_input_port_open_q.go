package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimInputPortOpenQ implements the (input-port-open?) primitive.
// Returns #t if input port is open.
func PrimInputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.CharacterInputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "input-port-open?: expected an input port but got %T", o)
	}
	// For now, we assume all ports are open (proper tracking would require port state)
	mc.SetValue(values.TrueValue)
	return nil
}
