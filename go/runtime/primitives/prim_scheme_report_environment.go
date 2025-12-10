package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimSchemeReportEnvironment implements the (scheme-report-environment) primitive.
// Returns R5RS env.
func PrimSchemeReportEnvironment(_ context.Context, mc *machine.MachineContext) error {
	version := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	versionInt, ok := version.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "scheme-report-environment: expected an integer but got %T", version)
	}

	// R7RS specifies version 5 (for R5RS) or 7 (for R7RS)
	switch versionInt.Value {
	case 5, 7:
		// Return the current top-level environment
		// In a full implementation, this would return a restricted environment
		topLevel := mc.EnvironmentFrame().TopLevel()
		mc.SetValue(values.NewSchemeEnvironment("scheme-report-environment", topLevel))
		return nil
	default:
		return values.NewForeignError("scheme-report-environment: unsupported version, expected 5 or 7")
	}
}
