package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimPairQ implements the pair? predicate.
// Returns #t if the argument is a pair (cons cell).
func PrimPairQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	mc.SetValue(utils.BoolToBoolean(ok && !values.IsEmptyList(pr)))
	return nil
}
