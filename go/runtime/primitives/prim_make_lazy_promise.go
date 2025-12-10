package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimMakeLazyPromise implements the (delay-force) primitive.
// Creates a lazy promise that delays evaluation of a thunk.
func PrimMakeLazyPromise(_ context.Context, mc *machine.MachineContext) error {
	thunk := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	mc.SetValue(values.NewPromise(thunk))
	return nil
}
