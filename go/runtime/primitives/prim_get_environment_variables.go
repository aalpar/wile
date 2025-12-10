package primitives

import (
	"context"
	"os"
	"strings"

	"wile/machine"
	"wile/values"
)

// PrimGetEnvironmentVariables implements the (get-environment-variables) primitive.
// Returns all environment variables.
func PrimGetEnvironmentVariables(_ context.Context, mc *machine.MachineContext) error {
	env := os.Environ()
	list := values.EmptyList
	for i := len(env) - 1; i >= 0; i-- {
		parts := strings.SplitN(env[i], "=", 2)
		if len(parts) == 2 {
			pair := values.NewCons(values.NewString(parts[0]), values.NewString(parts[1]))
			list = values.NewCons(pair, list)
		}
	}
	mc.SetValue(list)
	return nil
}
