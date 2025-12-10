package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimOutputPortOpenQ implements the output-port-open? primitive.
// Returns #t if the output port is open, #f otherwise.
func PrimOutputPortOpenQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*values.CharacterOutputPort)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnOutputPort, "output-port-open?: expected an output port but got %T", o)
	}
	// For now, we assume all ports are open (proper tracking would require port state)
	mc.SetValue(values.TrueValue)
	return nil
}
