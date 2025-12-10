package primitives

import (
	"context"

	"wile/machine"
)

// PrimCurrentInputPort implements the (current-input-port) primitive.
// Returns the current input port.
func PrimCurrentInputPort(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(GetCurrentInputPort())
	return nil
}
