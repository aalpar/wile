package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimCar implements the car primitive.
// Returns the first element of a pair.
func PrimCar(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "car: expected a pair but got %T", o)
	}
	mc.SetValue(v.Car())
	return nil
}
