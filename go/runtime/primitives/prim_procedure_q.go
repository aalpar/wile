package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
)

// PrimProcedureQ implements the (procedure?) primitive.
// Returns #t if argument is a procedure.
func PrimProcedureQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	_, ok := o.(*machine.MachineClosure)
	mc.SetValue(utils.BoolToBoolean(ok))
	return nil
}
