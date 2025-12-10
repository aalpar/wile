package primitives

import (
	"context"
	"os"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimFileExistsQ implements the (file-exists?) primitive.
// Returns #t if file exists.
func PrimFileExistsQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "file-exists?: expected a string but got %T", o)
	}
	_, err := os.Stat(filename.Value)
	mc.SetValue(utils.BoolToBoolean(err == nil))
	return nil
}
