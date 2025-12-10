package primitives

import (
	"context"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimCommandLine implements the (command-line) primitive.
// Returns a list of command line arguments.
func PrimCommandLine(_ context.Context, mc *machine.MachineContext) error {
	args := os.Args
	list := values.EmptyList
	for i := len(args) - 1; i >= 0; i-- {
		list = values.NewCons(values.NewString(args[i]), list)
	}
	mc.SetValue(list)
	return nil
}
