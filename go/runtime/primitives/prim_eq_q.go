package primitives

import (
	"context"

	"wile/machine"
	"wile/utils"
)

// PrimEqQ implements the eq? predicate for object identity.
// Returns #t if both arguments are the same object (pointer equality).
func PrimEqQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	// eq? tests for object identity - same pointer or same immediate value
	// Go's == compares pointers by address for reference types
	mc.SetValue(utils.BoolToBoolean(o0 == o1))
	return nil
}
