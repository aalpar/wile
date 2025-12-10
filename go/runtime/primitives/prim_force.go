package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimForce implements the (force) primitive.
// Forces evaluation of a promise.
func PrimForce(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	promise, ok := o.(*values.Promise)
	if !ok {
		// R7RS says: if not a promise, return the value unchanged
		mc.SetValue(o)
		return nil
	}

	// If already forced, return cached result
	if promise.Forced {
		mc.SetValue(promise.Result)
		return nil
	}

	// Force the promise by invoking the thunk
	mcls, ok := promise.Thunk.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "force: promise thunk is not a procedure: %T", promise.Thunk)
	}

	sub := mc.NewSubContext()
	if _, err := sub.Apply(mcls); err != nil {
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

	result := sub.GetValue()

	// R7RS iterative forcing: if result is a promise, force it too
	if resultPromise, ok := result.(*values.Promise); ok {
		// Update our promise to point to the result promise's contents
		if resultPromise.Forced {
			promise.Result = resultPromise.Result
		} else {
			promise.Thunk = resultPromise.Thunk
			// Recursively force
			mc.SetValue(promise)
			return PrimForce(ctx, mc)
		}
	} else {
		promise.Result = result
	}

	promise.Forced = true
	promise.Thunk = nil
	mc.SetValue(promise.Result)
	return nil
}
