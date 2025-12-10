package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimBytevectorQ implements the bytevector? primitive.
// Returns #t if argument is a bytevector.
func PrimBytevectorQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if _, ok := o.(*values.ByteVector); ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
