package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimCons implements the (cons) primitive.
// Creates a new pair from the car and cdr arguments.
func PrimCons(_ context.Context, mc *machine.MachineContext) error {
	car := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	cdr := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	mc.SetValue(values.NewCons(car, cdr))
	return nil
}
