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

package core

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// Exception handling rides a continuation-mark-backed parameter so it travels with
// captured continuations (R7RS §6.11; see machine.RaiseInPlace and
// machine/exception_raise.go). with-exception-handler is defined in Scheme as
// (parameterize ((%exception-handlers (cons handler (%exception-handlers)))) (thunk));
// raise / raise-continuable / error and the Go-error bridge all funnel through the
// single machine.RaiseInPlace helper.

// PrimExceptionHandlerParameter returns the canonical exception-handler parameter
// (machine.ExceptionHandlerParam). Bootstrap binds its result to the global
// %exception-handlers, which the Scheme with-exception-handler parameterizes.
func PrimExceptionHandlerParameter(cc machine.CallContext) error {
	cc.SetValue(machine.ExceptionHandlerParam())
	return nil
}

// PrimRaise implements (raise obj): invokes the current exception handler on obj as
// a non-continuable exception.
func PrimRaise(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "raise")
	if err != nil {
		return err
	}
	return machine.RaiseInPlace(mc, mc.Arg(0), false)
}

// PrimRaiseContinuable implements (raise-continuable obj): invokes the current
// handler; if the handler returns, its value becomes the value of the
// raise-continuable expression and execution resumes at the call site (R7RS §6.11).
func PrimRaiseContinuable(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "raise-continuable")
	if err != nil {
		return err
	}
	return machine.RaiseInPlace(mc, mc.Arg(0), true)
}

// PrimError implements (error message irritant ...): builds an error object from
// MESSAGE and irritants, then raises it as a non-continuable exception.
func PrimError(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "error")
	if err != nil {
		return err
	}
	message := mc.Arg(0)
	irritantsList := mc.Arg(1)

	msgStr, ok := message.(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString,
			"error: message must be a string but got %T", message)
	}

	// Copy irritants out of the variadic rest-arg buffer (restArgBuf) into a fresh
	// slice; NewErrorObject retains them, and the rest-arg list is only valid for
	// the duration of this call.
	var irritants []values.Value
	irritantsTuple, ok := irritantsList.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"error: irritants must be a proper list but got %T", irritantsList)
	}
	err = helpers.ForEachList(mc.Context(), irritantsTuple, "error", func(_ context.Context, _ int, _ bool, v values.Value) error {
		irritants = append(irritants, v)
		return nil
	})
	if err != nil {
		return err
	}

	errObj := values.NewErrorObject(msgStr.Value, irritants...)
	return machine.RaiseInPlace(mc, errObj, false)
}

// PrimErrorObjectQ implements the error-object? predicate.
// Returns #t if the argument is an error object created by (error ...),
// #f otherwise.
func PrimErrorObjectQ(mc machine.CallContext) error {
	obj := mc.Arg(0)
	_, ok := obj.(*values.NativeError)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimErrorObjectMessage implements the error-object-message accessor.
// Returns the message string from an error object.
var PrimErrorObjectMessage = helpers.MakeUnaryAccessor(werr.ErrNotANativeError, "error-object-message", func(errObj *values.NativeError) values.Value {
	return errObj.Message()
})

// PrimErrorObjectIrritants implements the error-object-irritants accessor.
// Returns the list of irritant objects from an error object.
var PrimErrorObjectIrritants = helpers.MakeUnaryAccessor(werr.ErrNotANativeError, "error-object-irritants", func(errObj *values.NativeError) values.Value {
	return errObj.Irritants()
})

// PrimReadErrorQ implements the read-error? predicate.
// R7RS §6.11: Returns #t if obj is an error object raised during reading.
func PrimReadErrorQ(mc machine.CallContext) error {
	obj := mc.Arg(0)
	errObj, ok := obj.(*values.NativeError)
	mc.SetValue(values.BoolToBoolean(ok && errObj.IsReadError()))
	return nil
}

// PrimFileErrorQ implements the file-error? predicate.
// R7RS §6.11: Returns #t if obj is an error object raised during file operations.
func PrimFileErrorQ(mc machine.CallContext) error {
	obj := mc.Arg(0)
	errObj, ok := obj.(*values.NativeError)
	mc.SetValue(values.BoolToBoolean(ok && errObj.IsFileError()))
	return nil
}
