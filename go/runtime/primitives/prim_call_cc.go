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

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimCallCC implements the call/cc primitive.
// Captures current continuation and passes to procedure.
func PrimCallCC(ctx context.Context, mc *machine.MachineContext) error {
	proc := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	mcls, ok := proc.(*machine.MachineClosure)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAProcedure, "call/cc: expected a procedure but got %T", proc)
	}

	// Capture the current continuation
	// mc.cont is the continuation that will be restored when this foreign function returns
	// (i.e., the continuation to the caller of call/cc). We copy it to avoid mutation issues.
	cont := mc.Parent()
	if cont != nil {
		cont = cont.Copy()
	}

	// Create a closure that, when called, restores this continuation
	contClosure := NewEscapeContinuationClosure(mc.EnvironmentFrame().TopLevel(), cont)

	// Call the procedure with the continuation closure
	sub := mc.NewSubContext()
	if _, err := sub.Apply(mcls, contClosure); err != nil {
		return err
	}
	if err := sub.Run(ctx); err != nil {
		// Check if this is a continuation escape
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			// Restore the continuation and set the escape value
			// Then propagate the escape so the caller knows not to increment PC
			mc.Restore(escapeErr.Continuation)
			mc.SetValue(escapeErr.Value)
			escapeErr.Handled = true
			return escapeErr
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	// If we get here, the procedure returned normally (didn't invoke the continuation)
	mc.SetValue(sub.GetValue())
	return nil
}

func NewEscapeContinuationClosure(env *environment.EnvironmentFrame, cont *machine.MachineContinuation) *machine.MachineClosure {
	fn := func(_ context.Context, innerMC *machine.MachineContext) error {
		// Get the value passed to the continuation (from the closure's argument)
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		// Return an escape error that will propagate up through sub-contexts
		return &machine.ErrContinuationEscape{
			Continuation: cont,
			Value:        val,
		}
	}
	return machine.NewForeignClosure(env, 1, false, fn)
}
