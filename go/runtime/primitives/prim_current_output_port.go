package primitives

import (
	"context"

	"wile/machine"
)

// PrimCurrentOutputPort implements the (current-output-port) primitive.
// Returns the current output port.
func PrimCurrentOutputPort(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(GetCurrentOutputPort())
	return nil
}
