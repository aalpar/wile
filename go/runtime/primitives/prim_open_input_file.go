package primitives

import (
	"context"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimOpenInputFile implements the open-input-file primitive.
// Opens a file for reading and returns an input port.
func PrimOpenInputFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "open-input-file: expected a string but got %T", o)
	}
	file, err := os.Open(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "open-input-file: %v", err)
	}
	mc.SetValue(values.NewCharacterInputPortFromReader(file))
	return nil
}
