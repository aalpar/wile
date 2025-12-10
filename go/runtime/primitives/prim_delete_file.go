package primitives

import (
	"context"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimDeleteFile implements the (delete-file) primitive.
// Deletes a file from the filesystem.
func PrimDeleteFile(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "delete-file: expected a string but got %T", o)
	}
	if err := os.Remove(filename.Value); err != nil {
		return values.WrapForeignErrorf(err, "delete-file: %v", err)
	}
	mc.SetValues()
	return nil
}
