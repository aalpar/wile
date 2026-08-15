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

package wile

import (
	"context"
	"errors"
	"fmt"
	"reflect"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// makeWrapper generates the ForeignFunction closure that bridges between
// the VM calling convention and the Go function.
func (p *ffiSpec) makeWrapper() ForeignFunction {
	return func(cc CallContext) (returnErr error) {
		mc := cc.(*MachineContext)
		defer func() {
			r := recover()
			if r == nil {
				// No panic in flight. Leave returnErr alone: the function body may
				// have set it on a normal error return.
				return
			}
			// A fault raised by the callback protocol is deliberate, not a host bug.
			// callbackErrorResult and callbackSuccessResult (ffi_arg_converters.go)
			// panic when the Go signature has no error slot to return through, so
			// the panic IS the return path, and converting it back to an error here
			// is what the protocol is for. The VM's foreign-call dispatcher then
			// routes it through bridgeForeignError exactly as it routes any returned
			// error — the ErrPanicRecovery wrap is transparent to that routing,
			// because bridgeForeignError matches VM signal types (prompt abort,
			// exception escape, timer interrupt, continuation resume) with errors.As,
			// which traverses the cause chain. Scheme guard catches the result.
			//
			// Match on the two protocol sentinels, NOT on "is a *werr.ForeignError".
			// The VM's own invariant guards panic with a *ForeignError too, so a type
			// test would convert one of those into a catchable condition whenever it
			// crossed a host function that had called back into Scheme.
			err, isErr := r.(error)
			if isErr &&
				(errors.Is(err, werr.ErrFFICallbackError) ||
					errors.Is(err, werr.ErrCallbackResultConversion)) {
				returnErr = werr.RecoverAsError(r, werr.ErrPanicRecovery,
					fmt.Sprintf("FFI %q", p.name))
				return
			}

			// Anything else is a Go-level bug in the host's function — a
			// runtime.Error, a nil map write, a bare panic("..."). Re-raise it
			// unchanged so it reaches the VM boundary recover
			// (MachineContext.RunResumable) and surfaces to the embedder, rather
			// than becoming a Scheme condition that guard can swallow. That boundary
			// names the primitive itself (MachineContext.foreignCallName), so
			// re-wrapping here would only say it twice.
			//
			// This is the one rule both registration routes now follow:
			// RegisterPrimitive never had a recover here, and RegisterFunc no longer
			// has one that catches more than the protocol above.
			panic(r)
		}()

		var args []reflect.Value

		// Forward context if needed.
		if p.hasContext {
			args = append(args, reflect.ValueOf(mc.Context()))
		}

		if p.isVariadic {
			// Fixed args: mc.Arg(0) .. mc.Arg(paramCount-2)
			// Variadic list: mc.Arg(paramCount-1)
			fixedCount := p.paramCount - 1

			for i := range fixedCount {
				converted, err := p.argConvs[i](mc, mc.Arg(i))
				if err != nil {
					return err
				}
				args = append(args, converted)
			}

			// Walk the Scheme list for variadic args.
			variadicConv := p.argConvs[p.paramCount-1]
			varList := mc.Arg(fixedCount)

			_, isTuple := varList.(values.Tuple)
			if !isTuple {
				return fmtArgError(p.name, fixedCount+1, "proper list", varList)
			}

			tail, err := values.ForEach(mc.Context(), varList, func(_ context.Context, _ int, _ bool, v values.Value) error {
				converted, convErr := variadicConv(mc, v)
				if convErr != nil {
					return convErr
				}
				args = append(args, converted)
				return nil
			})
			if err != nil {
				return err
			}
			if !values.IsEmptyList(tail) {
				return fmtArgError(p.name, fixedCount+1, "proper list", varList)
			}
		} else {
			for i := range p.paramCount {
				converted, err := p.argConvs[i](mc, mc.Arg(i))
				if err != nil {
					return err
				}
				args = append(args, converted)
			}
		}

		// Call the Go function. Use Call (not CallSlice) since we've
		// already expanded variadic args into individual reflect.Values.
		results := p.fnValue.Call(args)

		// Process return values.
		switch {
		case p.retConv != nil && p.hasError:
			// (T, error)
			errVal := results[1]
			if !errVal.IsNil() {
				return errVal.Interface().(error)
			}
			mc.SetValue(p.retConv(results[0]))

		case p.retConv != nil:
			// T (no error)
			mc.SetValue(p.retConv(results[0]))

		case p.hasError:
			// error only (void or error)
			errVal := results[0]
			if !errVal.IsNil() {
				return errVal.Interface().(error)
			}
			mc.SetValue(values.Void)

		default:
			// void
			mc.SetValue(values.Void)
		}

		return nil
	}
}
