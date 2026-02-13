// Copyright 2026 Aaron Alpar
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

package exceptions

import (
	"context"
	"errors"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
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
	// Exception handler automatically inherited from parent (M3 fix)
	sub := mc.NewSubContext()

	_, err := sub.ApplyCallable(thunk)
	if err != nil {
		mc.PopExceptionHandler()
		return err
	}

	thunkErr := sub.Run()

	// Check for exception escape
	var excErr *machine.ErrExceptionEscape
	if errors.As(thunkErr, &excErr) && !excErr.Handled {
		return handleException(mc, excErr, handler)
	}

	// Pop handler on normal completion
	mc.PopExceptionHandler()

	// Check for other errors
	if thunkErr != nil {
		return thunkErr
	}

	// Return thunk's result
	mc.SetValue(sub.GetValue())
	return nil
}

// callExceptionHandler invokes the exception handler with the given condition.
// Returns the handler's return value, or an error if the handler raised an exception
// or escaped via continuation.
func callExceptionHandler(mc *machine.MachineContext, condition values.Value, handler values.Value) (values.Value, error) {
	// Exception handler automatically inherited from parent (M3 fix)
	sub := mc.NewSubContext()

	_, err := sub.ApplyCallable(handler, condition)
	if err != nil {
		return nil, err
	}

	err = sub.Run()

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

	if err != nil {
		return nil, err
	}

	return sub.GetValue(), nil
}

// resumeFromContinuation resumes execution from a captured continuation with the given value.
// Returns the result of the resumed execution, or an error.
// If cont is nil (raise-continuable was in tail position), returns value directly.
func resumeFromContinuation(mc *machine.MachineContext, cont *machine.MachineContinuation, value values.Value) (values.Value, error) {
	if cont == nil {
		// raise-continuable was in tail position - no continuation to resume
		// The handler's return value becomes the final result
		return value, nil
	}

	// Exception handler automatically inherited from parent (M3 fix)
	resumeSub := mc.NewSubContext()
	resumeSub.Restore(cont)
	resumeSub.SetValue(value)

	err := resumeSub.Run()
	if err != nil {
		return nil, err
	}

	return resumeSub.GetValue(), nil
}

// handleException processes an exception by calling the handler and, for continuable
// exceptions, resuming execution from the raise-continuable call site per R7RS §6.11.
func handleException(mc *machine.MachineContext, excErr *machine.ErrExceptionEscape, handler values.Value) error {
	// Pop this handler before calling it (so re-raises use parent handler per R7RS)
	mc.PopExceptionHandler()

	// Unwind the winding stack: run after thunks for any dynamic-wind frames
	// that were entered between the exception handler installation and the raise point.
	// This ensures parameterize restores values before the handler sees them.
	if excErr.WindingStack != nil {
		// Find common ancestor between exception's winding stack and current
		currentStack := mc.WindingStack()
		commonDepth := machine.FindCommonWindingPrefix(excErr.WindingStack, currentStack)

		// Unwind frames that were entered after the handler was installed
		for i := len(excErr.WindingStack) - 1; i >= commonDepth; i-- {
			frame := excErr.WindingStack[i]
			if frame.After != nil {
				sub := mc.NewSubContext()
				sub.SetWindingStack(excErr.WindingStack[:i])
				_, err := sub.Apply(frame.After)
				if err != nil {
					return err
				}
				err = sub.Run()
				if err != nil {
					return err
				}
			}
		}
	}

	for {
		// Call handler with the condition
		handlerResult, err := callExceptionHandler(mc, excErr.Condition, handler)
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

		resumeResult, resumeErr := resumeFromContinuation(mc, excErr.Continuation, handlerResult)

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

		if resumeErr != nil {
			return resumeErr
		}

		// Normal completion
		mc.SetValue(resumeResult)
		excErr.Handled = true
		return nil
	}
}

// PrimRaise implements the raise primitive.
// (raise obj)
// Raises a non-continuable exception with obj as the condition.
func PrimRaise(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)

	return &machine.ErrExceptionEscape{
		Condition:    obj,
		Continuable:  false,
		Handled:      false,
		WindingStack: mc.WindingStack().Copy(),
		Source:       mc.CurrentSource(),
		StackTrace:   mc.CaptureStackTrace(20),
	}
}

// PrimRaiseContinuable implements the raise-continuable primitive.
// (raise-continuable obj)
// Raises a continuable exception with obj as the condition.
// If the handler returns, its return value becomes the value of raise-continuable,
// and execution continues from the call site per R7RS §6.11.
func PrimRaiseContinuable(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)

	// Copy continuation to prevent mutation issues during handler execution.
	// This follows the pattern established by call/cc.
	cont := mc.Parent()
	if cont != nil {
		cont = cont.Copy()
	}

	return &machine.ErrExceptionEscape{
		Condition:    obj,
		Continuable:  true,
		Continuation: cont,
		Handled:      false,
		WindingStack: mc.WindingStack().Copy(),
		Source:       mc.CurrentSource(),
		StackTrace:   mc.CaptureStackTrace(20),
	}
}

// PrimError implements the error primitive.
// (error message irritant ...)
// Creates an error object with the given message and irritants, then raises it.
func PrimError(ctx context.Context, mc *machine.MachineContext) error {
	message := mc.Arg(0)
	irritantsList := mc.Arg(1)

	msgStr, ok := message.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString,
			"error: message must be a string but got %T", message)
	}

	// Convert irritants list to slice
	var irritants []values.Value
	_, err := values.ForEach(ctx, irritantsList, func(_ context.Context, i int, hasNext bool, v values.Value) error {
		irritants = append(irritants, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "error: invalid irritants list")
	}

	// Create error object and raise it
	errObj := values.NewErrorObject(msgStr.Datum(), irritants...)

	return &machine.ErrExceptionEscape{
		Condition:    errObj,
		Continuable:  false,
		Handled:      false,
		WindingStack: mc.WindingStack().Copy(),
		Source:       mc.CurrentSource(),
		StackTrace:   mc.CaptureStackTrace(20),
	}
}

// PrimErrorObjectQ implements the error-object? predicate.
// Returns #t if the argument is an error object created by (error ...),
// #f otherwise.
func PrimErrorObjectQ(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	_, ok := obj.(*values.NativeError)
	mc.SetValue(schemeutil.BoolToBoolean(ok))
	return nil
}

// PrimErrorObjectMessage implements the error-object-message accessor.
// Returns the message string from an error object.
func PrimErrorObjectMessage(_ context.Context, mc *machine.MachineContext) error {
	errObj, err := helpers.RequireArg[*values.NativeError](mc, 0, values.ErrNotANativeError, "error-object-message")
	if err != nil {
		return err
	}
	mc.SetValue(errObj.Message())
	return nil
}

// PrimErrorObjectIrritants implements the error-object-irritants accessor.
// Returns the list of irritant objects from an error object.
func PrimErrorObjectIrritants(_ context.Context, mc *machine.MachineContext) error {
	errObj, err := helpers.RequireArg[*values.NativeError](mc, 0, values.ErrNotANativeError, "error-object-irritants")
	if err != nil {
		return err
	}
	mc.SetValue(errObj.Irritants())
	return nil
}

// PrimReadErrorQ implements the read-error? predicate.
// R7RS §6.11: Returns #t if obj is an error object raised during reading.
func PrimReadErrorQ(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	errObj, ok := obj.(*values.NativeError)
	mc.SetValue(schemeutil.BoolToBoolean(ok && errObj.IsReadError()))
	return nil
}

// PrimFileErrorQ implements the file-error? predicate.
// R7RS §6.11: Returns #t if obj is an error object raised during file operations.
func PrimFileErrorQ(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	errObj, ok := obj.(*values.NativeError)
	mc.SetValue(schemeutil.BoolToBoolean(ok && errObj.IsFileError()))
	return nil
}
