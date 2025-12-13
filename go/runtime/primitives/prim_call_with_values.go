// Copyright 2025 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
