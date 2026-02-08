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

package machine

import (
	"context"
	"errors"
	"fmt"

	"github.com/aalpar/wile/values"
)

type ForeignFunction func(ctx context.Context, mc *MachineContext) error

type OperationForeignFunctionCall struct {
	Function ForeignFunction
}

func NewOperationForeignFunctionCall(ffn ForeignFunction) *OperationForeignFunctionCall {
	return &OperationForeignFunctionCall{
		Function: ffn,
	}
}

// goErrorToSchemeException converts a Go error to a Scheme exception escape.
// It detects ForeignFileError and ForeignReadError to set the appropriate
// NativeError kind per R7RS §6.11. The MachineContext is used to capture
// the source location and stack trace at the point where the error occurred.
func goErrorToSchemeException(mc *MachineContext, err error) error {
	kind := values.NativeErrorKindGeneric
	var fileErr *values.ForeignFileError
	var readErr *values.ForeignReadError
	if errors.As(err, &fileErr) {
		kind = values.NativeErrorKindFile
	} else if errors.As(err, &readErr) {
		kind = values.NativeErrorKindRead
	}
	errObj := values.NewErrorObjectWithCauseAndKind(err.Error(), err, kind)
	return &ErrExceptionEscape{
		Condition:   errObj,
		Continuable: false,
		Handled:     false,
		Source:      mc.CurrentSource(),
		StackTrace:  mc.CaptureStackTrace(20),
	}
}

func (p *OperationForeignFunctionCall) Apply(ctx context.Context, mc *MachineContext) (rmc *MachineContext, rerr error) {
	if p.Function == nil {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedNil, "foreign function is nil")
	}
	// Recover panics from the values package (e.g., ErrDivisionByZero, ErrNotANumber,
	// ErrNotAList) and convert them to Scheme exceptions. The Number interface methods
	// signal these conditions via panic because the interface returns Number, not
	// (Number, error). Recovered panics are always plain Go errors, never continuation
	// escapes or prompt aborts, so they go directly through error-to-exception conversion.
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		var err error
		switch v := r.(type) {
		case error:
			err = v
		default:
			err = fmt.Errorf("%v", v)
		}
		rmc = nil
		rerr = goErrorToSchemeException(mc, err)
	}()
	mc.counters.ForeignCalls++
	err := p.Function(ctx, mc)
	if err != nil {
		// Check if this is a continuation escape.
		// Continuation escapes propagate up through foreign function calls until they reach
		// the PrimCallCC that captured the continuation, which then handles the escape.
		var escapeErr *ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			if escapeErr.Handled {
				// Already handled by PrimCallCC - mc has been restored to target continuation
				return mc, nil
			}
			// Unhandled escape - propagate up so it can reach PrimCallCC
			return nil, err
		}

		// Check if this is a prompt abort - propagate up to the matching
		// call-with-continuation-prompt handler.
		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			return nil, err
		}

		// Check if this is already a Scheme exception - propagate as-is
		var excErr *ErrExceptionEscape
		if errors.As(err, &excErr) {
			return nil, err
		}

		// Convert Go error to Scheme exception so guard/with-exception-handler can catch it.
		// Create an error object that wraps the original Go error for debugging.
		// Use errors.As to detect ForeignFileError/ForeignReadError and set the appropriate kind.
		return nil, goErrorToSchemeException(mc, err)
	}
	mc.pc++
	return mc, nil
}

func (p *OperationForeignFunctionCall) SchemeString() string {
	return "#<machine-operation-foreign-function-call>"
}

func (p *OperationForeignFunctionCall) IsVoid() bool {
	return p == nil
}

func (p *OperationForeignFunctionCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationForeignFunctionCall)
	return sameType(p, v, ok)
}
