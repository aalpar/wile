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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimCurrentErrorContext implements (current-error-context).
// Returns the ErrorContext from the nearest continuation mark, or #f if
// not currently inside an exception handler dispatch.
func PrimCurrentErrorContext(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	cms := mc.CollectContinuationMarks(machine.DefaultPromptTag)
	mc.SetValue(cms.First(machine.ErrorContextKey(), values.FalseValue))
	return nil
}

// PrimErrorContextQ implements (error-context? obj).
// Returns #t if obj is an ErrorContext value.
var PrimErrorContextQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*machine.ErrorContext)
	return ok
})

// PrimErrorContextSource implements (error-context-source ctx).
// Returns the source location string from the error context, or #f if
// no source location was captured.
func PrimErrorContextSource(mc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](mc, 0, werr.ErrNotAnErrorContext, "error-context-source")
	if err != nil {
		return err
	}
	loc := ctx.SourceLocation()
	if loc == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(loc))
	return nil
}

// PrimErrorContextStackTrace implements (error-context-stack-trace ctx).
// Returns the stack trace as a list of alists, where each alist has keys
// name, file, line, and column. Returns the empty list if no stack trace
// was captured.
func PrimErrorContextStackTrace(mc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](mc, 0, werr.ErrNotAnErrorContext, "error-context-stack-trace")
	if err != nil {
		return err
	}
	mc.SetValue(stackTraceToSchemeList(ctx.StackTraceFrames()))
	return nil
}

// PrimErrorContextMarks implements (error-context-marks ctx).
// Returns the continuation mark set snapshot from the raise site, or #f
// if marks were not captured.
func PrimErrorContextMarks(mc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](mc, 0, werr.ErrNotAnErrorContext, "error-context-marks")
	if err != nil {
		return err
	}
	marks := ctx.Marks()
	if marks == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(marks)
	return nil
}

// PrimErrorObjectSource implements (error-object-source err).
// Returns the source location string from a NativeError, or #f if empty.
func PrimErrorObjectSource(mc machine.CallContext) error {
	errObj, err := helpers.RequireArg[*values.NativeError](mc, 0, werr.ErrNotANativeError, "error-object-source")
	if err != nil {
		return err
	}
	loc := errObj.SourceLocation()
	if loc == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(loc))
	return nil
}

// PrimErrorObjectStackTrace implements (error-object-stack-trace err).
// Returns the stack trace from a NativeError as a list of alists, or ()
// if no stack trace has been captured.
func PrimErrorObjectStackTrace(mc machine.CallContext) error {
	errObj, err := helpers.RequireArg[*values.NativeError](mc, 0, werr.ErrNotANativeError, "error-object-stack-trace")
	if err != nil {
		return err
	}
	stv := errObj.StackTraceValue()
	if stv == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	mc.SetValue(stv)
	return nil
}

// stackTraceToSchemeList converts a machine.StackTrace to a Scheme list of alists.
func stackTraceToSchemeList(st machine.StackTrace) values.Tuple {
	if len(st) == 0 {
		return values.EmptyList
	}
	frames := make([]values.Value, len(st))
	for i, frame := range st {
		frames[i] = stackFrameToAlist(frame)
	}
	return values.List(frames...)
}

// stackFrameToAlist converts a single machine.StackFrame to a Scheme alist.
// The alist contains keys: name, file, line, column.
// Example: ((name . "f") (file . "test.scm") (line . 10) (column . 5))
func stackFrameToAlist(frame machine.StackFrame) values.Tuple {
	name := frame.FunctionName
	if name == "" {
		name = "<anonymous>"
	}

	// Prefer CurrentLoc over CallSite for source info.
	src := frame.CurrentLoc
	if src == nil {
		src = frame.CallSite
	}

	nameEntry := values.NewCons(values.NewSymbol("name"), values.NewString(name))
	if src == nil {
		return values.List(nameEntry)
	}

	fileEntry := values.NewCons(values.NewSymbol("file"), values.NewString(src.File))
	lineEntry := values.NewCons(values.NewSymbol("line"), values.NewInteger(int64(src.Start.Line())))
	colEntry := values.NewCons(values.NewSymbol("column"), values.NewInteger(int64(src.Start.Column())))

	return values.List(nameEntry, fileEntry, lineEntry, colEntry)
}
