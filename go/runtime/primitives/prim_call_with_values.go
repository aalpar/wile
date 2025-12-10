package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimCallWithValues implements the call-with-values primitive.
// Calls producer, passes results to consumer.
func PrimCallWithValues(ctx context.Context, mc *machine.MachineContext) error {
	producer := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	consumer := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	producerCls, ok := producer.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-values: producer must be a procedure, got %T", producer)
	}

	consumerCls, ok := consumer.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call-with-values: consumer must be a procedure, got %T", consumer)
	}

	// Call producer with no arguments
	sub := mc.NewSubContext()
	if _, err := sub.Apply(producerCls); err != nil {
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

	// Get all values returned by producer
	producedValues := sub.GetValues()

	// Call consumer with all produced values as arguments
	sub2 := mc.NewSubContext()
	if _, err := sub2.Apply(consumerCls, producedValues...); err != nil {
		return err
	}
	if err := sub2.Run(ctx); err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// Return what consumer returned
	mc.SetValues(sub2.GetValues()...)
	return nil
}
