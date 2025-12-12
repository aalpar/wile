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

// PrimWithExceptionHandler implements the with-exception-handler primitive.
// (with-exception-handler handler thunk)
// Installs handler as exception handler during thunk execution.
func PrimWithExceptionHandler(ctx context.Context, mc *machine.MachineContext) error {
	handler := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	thunk := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	// Push handler onto exception handler stack
	mc.PushExceptionHandler(handler)

	// Run thunk in sub-context
	sub := mc.NewSubContext()
	sub.SetExceptionHandler(mc.ExceptionHandler())

	var thunkErr error
	switch t := thunk.(type) {
	case *machine.MachineClosure:
		if _, err := sub.Apply(t); err != nil {
			mc.PopExceptionHandler()
			return err
		}
	case *machine.CaseLambdaClosure:
		if _, err := sub.ApplyCaseLambda(t); err != nil {
			mc.PopExceptionHandler()
			return err
		}
	default:
		mc.PopExceptionHandler()
		return values.WrapForeignErrorf(values.ErrNotAProcedure,
			"with-exception-handler: thunk must be a procedure but got %T", thunk)
	}

	thunkErr = sub.Run(ctx)

	// Check for exception escape
	var excErr *machine.ErrExceptionEscape
	if errors.As(thunkErr, &excErr) && !excErr.Handled {
		return handleException(ctx, mc, excErr, handler)
	}

	// Pop handler on normal completion
	mc.PopExceptionHandler()

	// Check for other errors (but ignore halt)
	if thunkErr != nil && !errors.Is(thunkErr, machine.ErrMachineHalt) {
		return thunkErr
	}

	// Return thunk's result
	mc.SetValue(sub.GetValue())
	return nil
}

func handleException(ctx context.Context, mc *machine.MachineContext, excErr *machine.ErrExceptionEscape, handler values.Value) error {
	// Pop this handler before calling it (so re-raises use parent handler)
	mc.PopExceptionHandler()
	parentHandler := mc.ExceptionHandler()

	// Call handler with the condition
	sub := mc.NewSubContext()
	sub.SetExceptionHandler(parentHandler)

	switch h := handler.(type) {
	case *machine.MachineClosure:
		if _, err := sub.Apply(h, excErr.Condition); err != nil {
			return err
		}
	case *machine.CaseLambdaClosure:
		if _, err := sub.ApplyCaseLambda(h, excErr.Condition); err != nil {
			return err
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAProcedure,
			"with-exception-handler: handler must be a procedure but got %T", handler)
	}

	err := sub.Run(ctx)

	// Check if handler raised another exception
	var innerExc *machine.ErrExceptionEscape
	if errors.As(err, &innerExc) {
		// Propagate to parent handler
		return err
	}

	if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
		return err
	}

	// Handler returned normally
	if excErr.Continuable {
		// For continuable exceptions, return handler's value
		mc.SetValue(sub.GetValue())
		excErr.Handled = true
		return nil
	}

	// Non-continuable exception - handler should not return
	// R7RS says this is an error - raise a new exception
	return values.NewForeignError("exception handler returned from non-continuable exception")
}
