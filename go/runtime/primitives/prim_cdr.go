package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimCdr implements the cdr primitive.
// Returns the second element of a pair.
func PrimCdr(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "cdr: expected a pair but got %T", o)
	}
	mc.SetValue(v.Cdr())
	return nil
}
