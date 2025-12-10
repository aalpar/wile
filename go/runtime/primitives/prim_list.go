package primitives

import (
	"context"

	"wile/machine"
)

// PrimList implements the (list) primitive.
// Creates a list from the given arguments.
func PrimList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	// The variadic args come as a list - just return them
	mc.SetValue(o)
	return nil
}
