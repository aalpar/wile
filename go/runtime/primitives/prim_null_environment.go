package primitives

import (
	"context"

	"skeme/environment"
	"skeme/machine"
	"skeme/values"
)

// PrimNullEnvironment implements the null-environment primitive.
// Returns an empty R5RS environment with no bindings.
func PrimNullEnvironment(_ context.Context, mc *machine.MachineContext) error {
	version := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	versionInt, ok := version.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "null-environment: expected an integer but got %T", version)
	}

	// R7RS specifies version 5 (for R5RS)
	switch versionInt.Value {
	case 5, 7:
		// Create a new empty top-level environment with only syntax bindings
		// For now, we return a fresh top-level environment
		newEnv := environment.NewTipTopEnvironmentFrame()
		mc.SetValue(values.NewSchemeEnvironment("null-environment", newEnv))
		return nil
	default:
		return values.NewForeignError("null-environment: unsupported version, expected 5 or 7")
	}
}
