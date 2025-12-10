package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimEqualQ implements the equal? predicate for structural equality.
// Returns #t if both arguments have the same structure and values.
func PrimEqualQ(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o1 := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	mc.SetValue(utils.BoolToBoolean(values.EqualTo(o0, o1)))
	return nil
}
