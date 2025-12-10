package primitives

import (
	"context"

	"skeme/machine"
	"skeme/utils"
	"skeme/values"
)

// PrimListQ implements the (list?) primitive.
// Returns #t if the argument is a proper list, #f otherwise.
func PrimListQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.TrueValue)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(utils.BoolToBoolean(pr.IsList()))
	return nil
}
