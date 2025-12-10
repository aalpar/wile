package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimDynamicWind implements the (dynamic-wind) primitive.
// Calls a thunk with before and after handlers that execute on entry and exit.
func PrimDynamicWind(ctx context.Context, mc *machine.MachineContext) error {
	before := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	thunk := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	after := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()

	beforeCls, ok := before.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: before must be a procedure, got %T", before)
	}

	thunkCls, ok := thunk.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: thunk must be a procedure, got %T", thunk)
	}

	afterCls, ok := after.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "dynamic-wind: after must be a procedure, got %T", after)
	}

	// Call before thunk
	sub := mc.NewSubContext()
	if _, err := sub.Apply(beforeCls); err != nil {
		return err
	}
	if err := sub.Run(ctx); err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// Call main thunk
	sub2 := mc.NewSubContext()
	if _, err := sub2.Apply(thunkCls); err != nil {
		return err
	}
	thunkErr := sub2.Run(ctx)
	thunkResult := sub2.GetValues()

	// Always call after thunk, even if main thunk escaped
	sub3 := mc.NewSubContext()
	if _, err := sub3.Apply(afterCls); err != nil {
		return err
	}
	if err := sub3.Run(ctx); err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// Now handle thunk's result/error
	if thunkErr != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(thunkErr, &escapeErr) {
			// Propagate the escape (after thunk was already called)
			return thunkErr
		}
		if !errors.Is(thunkErr, machine.ErrMachineHalt) {
			return thunkErr
		}
	}

	mc.SetValues(thunkResult...)
	return nil
}
