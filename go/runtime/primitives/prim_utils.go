package primitives

import (
	"context"
	"errors"

	"wile/machine"
)

// TODO: refactor duplicated1 and duplicated2 into a single function - AI suggestion
// TODO: consider moving to machine package - AI suggestion
// TODO: consider making more generic to handle multiple arguments
// TODO: thunk could be machine.Operation - AI suggestion
// TODO: consider returning the value instead of setting it in mc - AI suggestion
// TODO: consider passing mc as argument instead of context - AI suggestion
// TODO: consider passing ctx as argument instead of using mc.NewSubContext() - AI suggestion
// TODO: consider handling continuation escapes and machine halt outside - AI suggestion
// TODO: consider renaming to runInSubContextWithArgument - AI suggestion
// TODO: consider adding comments to explain the function - AI suggestion
// TODO: consider adding unit tests for this function - AI suggestion
// TODO: refactor duplicated1 and duplicated2 into a single function - AI suggestion
// TODO: consider moving to machine package - AI suggestion
// TODO: consider making more generic to handle multiple arguments
// TODO: values.Port could be values.Value - AI suggestion
// TODO: thunk could be machine.Operation - AI suggestion
// TODO: consider returning the value instead of setting it in mc - AI suggestion
// TODO: consider passing mc as argument instead of context - AI suggestion
// TODO: consider passing ctx as argument instead of using mc.NewSubContext() - AI suggestion
// TODO: consider handling continuation escapes and machine halt outside - AI suggestion
// TODO: consider renaming to runInSubContextWithArgument - AI suggestion
// TODO: consider adding comments to explain the function - AI suggestion
// TODO: consider adding unit tests for this function - AI suggestion
// TODO: Port needs to be an interface to allow different port types
func duplicated1(ctx context.Context, mc *machine.MachineContext, thunk *machine.MachineClosure) error {
	// Call thunk in sub-context
	sub := mc.NewSubContext()
	_, err := sub.Apply(thunk)
	if err != nil {
		return err
	}
	err = sub.Run(ctx)
	if err != nil {
		// Propagate continuation escapes
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	mc.SetValue(sub.GetValue())
	return nil
}
