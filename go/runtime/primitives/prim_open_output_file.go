package primitives

import (
	"context"
	"os"

	"skeme/machine"
	"skeme/values"
)

// PrimOpenOutputFile implements the open-output-file primitive.
// Opens a file for writing and returns an output port.
func PrimOpenOutputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-output-file: expected a string but got %T", o)
	}
	file, err := os.Create(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "open-output-file: %v", err)
	}
	mc.SetValue(values.NewCharacterOutputPort(file))
	return nil
}
