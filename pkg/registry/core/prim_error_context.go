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
	"math"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCurrentErrorContext implements (current-error-context).
// Returns the ErrorContext from the nearest continuation mark, or #f if
// not currently inside an exception handler dispatch.
func PrimCurrentErrorContext(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "current-error-context")
	if err != nil {
		return err
	}
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

// PrimCurrentStackTrace implements (current-stack-trace [max-depth]).
// Returns the live Scheme stack at the call site in the same list-of-alists
// shape as error-context-stack-trace, without needing an error to carry it.
//
// The walk starts at the calling context: a foreign primitive runs inline in
// its caller's MachineContext, so no frame is synthesized for
// current-stack-trace itself and the innermost frame is the procedure that
// called it.
//
// max-depth is an exact non-negative integer bounding the number of frames
// walked; omitting it uses machine.DefaultBacktraceDepth. As with an error
// trace, a walk that stops short appends a "... N more frames ..." frame
// rather than silently truncating.
func PrimCurrentStackTrace(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "current-stack-trace")
	if err != nil {
		return err
	}
	depth := int64(machine.DefaultBacktraceDepth)
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(0), "current-stack-trace")
	if err != nil {
		return err
	}
	if ok {
		exact := false
		depth, exact = values.ExactInteger(v)
		if !exact {
			// ExactInteger also refuses an exact integer too large for int64,
			// hence "in machine-word range" rather than a bare type phrase.
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
				"current-stack-trace: max-depth must be an exact integer in machine-word range but got %s", v.SchemeString())
		}
		if depth < 0 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"current-stack-trace: max-depth must be non-negative")
		}
		// A budget past the chain's own length just stops at the chain, so
		// saturating here costs nothing and keeps the int conversion exact on
		// a 32-bit host.
		if depth > math.MaxInt {
			depth = math.MaxInt
		}
	}
	mc.SetValue(machine.StackTraceToSchemeList(mc.CaptureStackTrace(int(depth))))
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
	mc.SetValue(machine.StackTraceToSchemeList(ctx.StackTraceFrames()))
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
