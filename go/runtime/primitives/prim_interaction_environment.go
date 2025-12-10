package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimInteractionEnvironment implements the (interaction-environment) primitive.
// Returns the REPL environment (the current top-level environment).
func PrimInteractionEnvironment(_ context.Context, mc *machine.MachineContext) error {
	// Return the current top-level environment wrapped as a SchemeEnvironment
	topLevel := mc.EnvironmentFrame().TopLevel()
	mc.SetValue(values.NewSchemeEnvironment("interaction-environment", topLevel))
	return nil
}
