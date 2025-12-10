package primitives

import (
	"context"

	"skeme/environment"
	"skeme/machine"
	"skeme/values"
)

// PrimEnvironment implements the (environment) primitive.
// Constructs a new environment from import specifiers.
func PrimEnvironment(ctx context.Context, mc *machine.MachineContext) error {
	// Get variadic import specs (collected as a list in arg 0)
	argsVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	// Create fresh top-level environment
	newEnv := environment.NewTipTopEnvironmentFrame()

	// Share the library registry from caller's environment
	callerEnv := mc.EnvironmentFrame().TopLevel()
	registry := callerEnv.LibraryRegistry()
	if registry == nil {
		return values.NewForeignErrorf("environment: no library registry available")
	}
	newEnv.SetLibraryRegistry(registry)

	// Handle empty arguments case
	if values.IsEmptyList(argsVal) {
		mc.SetValue(values.NewSchemeEnvironment("environment", newEnv))
		return nil
	}

	args, ok := argsVal.(*values.Pair)
	if !ok {
		return values.NewForeignErrorf("environment: expected list of import specs, got %T", argsVal)
	}

	// Process each import spec
	_, err := args.ForEach(func(i int, hasNext bool, specVal values.Value) error {
		// Parse the import set from datum
		importSet, err := machine.ParseImportSetFromDatum(specVal)
		if err != nil {
			return values.WrapForeignErrorf(err, "environment: invalid import spec")
		}

		// Load the library (uses callerEnv for registry access)
		lib, err := machine.LoadLibrary(ctx, importSet.LibraryName, callerEnv)
		if err != nil {
			return values.WrapForeignErrorf(err, "environment: failed to load %s",
				importSet.LibraryName.SchemeString())
		}

		// Apply modifiers (only, except, prefix, rename)
		bindings, err := importSet.ApplyToExports(lib)
		if err != nil {
			return values.WrapForeignErrorf(err, "environment: error in import set for %s",
				importSet.LibraryName.SchemeString())
		}

		// Copy bindings to new environment
		if err := machine.CopyLibraryBindingsToEnv(lib, bindings, newEnv); err != nil {
			return values.WrapForeignErrorf(err, "environment: error copying bindings from %s",
				importSet.LibraryName.SchemeString())
		}

		return nil
	})

	if err != nil {
		return err
	}

	mc.SetValue(values.NewSchemeEnvironment("environment", newEnv))
	return nil
}
