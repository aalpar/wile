package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimNullQ implements the null? predicate.
// Returns #t if the argument is the empty list '().
func PrimNullQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	mc.SetValue(utils.BoolToBoolean(values.IsEmptyList(o)))
	return nil
}
