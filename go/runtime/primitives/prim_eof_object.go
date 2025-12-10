package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimEofObject implements the (eof-object) primitive.
// Returns the EOF object.
func PrimEofObject(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.EofObject)
	return nil
}
