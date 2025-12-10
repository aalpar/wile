package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimFeatures implements the (features) primitive.
// Returns list of implementation features.
func PrimFeatures(_ context.Context, mc *machine.MachineContext) error {
	features := machine.AllFeatures()

	// Build a list of symbols
	result := values.EmptyList
	// Build the list in reverse order to get correct ordering
	for i := len(features) - 1; i >= 0; i-- {
		sym := mc.EnvironmentFrame().InternSymbol(values.NewSymbol(features[i]))
		result = values.NewCons(sym, result)
	}

	mc.SetValue(result)
	return nil
}
