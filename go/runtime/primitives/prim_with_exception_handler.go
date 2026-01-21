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
	handler := mc.Arg(0)
	thunk := mc.Arg(1)

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

	thunkErr = sub.Run()

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

// callExceptionHandler invokes the exception handler with the given condition.
// Returns the handler's return value, or an error if the handler raised an exception
// or escaped via continuation.
func callExceptionHandler(ctx context.Context, mc *machine.MachineContext,
	condition values.Value, handler values.Value) (values.Value, error) {

	sub := mc.NewSubContext()
	sub.SetExceptionHandler(mc.ExceptionHandler())

	switch h := handler.(type) {
	case *machine.MachineClosure:
		if _, err := sub.Apply(h, condition); err != nil {
			return nil, err
		}
	case *machine.CaseLambdaClosure:
		if _, err := sub.ApplyCaseLambda(h, condition); err != nil {
			return nil, err
		}
	default:
		return nil, values.WrapForeignErrorf(values.ErrNotAProcedure,
			"with-exception-handler: handler must be a procedure but got %T", handler)
	}

	err := sub.Run()

	// Handler raised another exception - propagate it
	var innerExc *machine.ErrExceptionEscape
	if errors.As(err, &innerExc) {
		return nil, err
	}

	// Continuation escape - propagate it
	var contErr *machine.ErrContinuationEscape
	if errors.As(err, &contErr) {
		return nil, err
	}

	if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
		return nil, err
	}

	return sub.GetValue(), nil
}

// resumeFromContinuation resumes execution from a captured continuation with the given value.
// Returns the result of the resumed execution, or an error.
// If cont is nil (raise-continuable was in tail position), returns value directly.
func resumeFromContinuation(ctx context.Context, mc *machine.MachineContext,
	cont *machine.MachineContinuation, value values.Value) (values.Value, error) {

	if cont == nil {
		// raise-continuable was in tail position - no continuation to resume
		// The handler's return value becomes the final result
		return value, nil
	}

	resumeSub := mc.NewSubContext()
	resumeSub.SetExceptionHandler(mc.ExceptionHandler())
	resumeSub.Restore(cont)
	resumeSub.SetValue(value)

	err := resumeSub.Run()

	if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
		return nil, err
	}

	return resumeSub.GetValue(), nil
}

// handleException processes an exception by calling the handler and, for continuable
// exceptions, resuming execution from the raise-continuable call site per R7RS §6.11.
func handleException(ctx context.Context, mc *machine.MachineContext,
	excErr *machine.ErrExceptionEscape, handler values.Value) error {

	// Pop this handler before calling it (so re-raises use parent handler per R7RS)
	mc.PopExceptionHandler()

	for {
		// Call handler with the condition
		handlerResult, err := callExceptionHandler(ctx, mc, excErr.Condition, handler)
		if err != nil {
			return err
		}

		// Non-continuable exception - handler should not return
		if !excErr.Continuable {
			return values.NewForeignError("exception handler returned from non-continuable exception")
		}

		// Continuable: resume execution from the captured continuation
		// Push handler back so subsequent exceptions in resumed code use this handler
		mc.PushExceptionHandler(handler)

		resumeResult, resumeErr := resumeFromContinuation(ctx, mc, excErr.Continuation, handlerResult)

		// Check if resumed code raised another exception
		var newExcErr *machine.ErrExceptionEscape
		if errors.As(resumeErr, &newExcErr) && !newExcErr.Handled {
			// Pop handler (will be pushed again when we loop)
			mc.PopExceptionHandler()
			excErr = newExcErr
			continue // Loop to handle new exception
		}

		// Check for continuation escape from resumed code
		var contErr *machine.ErrContinuationEscape
		if errors.As(resumeErr, &contErr) {
			mc.PopExceptionHandler()
			return resumeErr // Propagate the escape
		}

		// Clean up handler stack
		mc.PopExceptionHandler()

		if resumeErr != nil && !errors.Is(resumeErr, machine.ErrMachineHalt) {
			return resumeErr
		}

		// Normal completion
		mc.SetValue(resumeResult)
		excErr.Handled = true
		return nil
	}
}
