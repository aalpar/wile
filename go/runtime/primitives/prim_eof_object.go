package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.EofObject)
	return nil
}
