package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimMakePromise implements the (make-promise) primitive.
// Creates a promise from a value, wrapping it if not already a promise.
func PrimMakePromise(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	// If already a promise, return it unchanged
	if p, ok := o.(*values.Promise); ok {
		mc.SetValue(p)
		return nil
	}
	// Otherwise, wrap in an already-forced promise
	mc.SetValue(values.NewForcedPromise(o))
	return nil
}
