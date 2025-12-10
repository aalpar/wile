package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimEofObjectQ implements the (eof-object?) primitive.
// Returns #t if the argument is the EOF object.
func PrimEofObjectQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	mc.SetValue(utils.BoolToBoolean(o == values.EofObject))
	return nil
}
