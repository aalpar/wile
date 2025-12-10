package primitives

import (
	"context"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimEmergencyExit implements the (emergency-exit) primitive.
// Exits the program immediately without cleanup or finalization.
func PrimEmergencyExit(_ context.Context, mc *machine.MachineContext) error {
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	code := 0
	if rest != values.EmptyList {
		if pr, ok := rest.(*values.Pair); ok && !values.IsEmptyList(pr) {
			switch v := pr.Car().(type) {
			case *values.Integer:
				code = int(v.Value)
			case *values.Boolean:
				if !v.Value {
					code = 1
				}
			}
		}
	}
	os.Exit(code)
	return nil
}
