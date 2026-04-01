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
	"fmt"
	"reflect"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// Pre-computed reflect types for interface detection.
var (
	valueInterfaceType = reflect.TypeFor[Value]()
	contextType        = reflect.TypeFor[context.Context]()
	errorType          = reflect.TypeFor[error]()
)

// argConverter converts a Scheme value to a Go reflect.Value.
// mc is provided for composite types (slices, callbacks) that need
// VM access; scalar converters ignore it.
type argConverter func(mc *MachineContext, v values.Value) (reflect.Value, error)

// retConverter converts a Go reflect.Value to a Scheme value.
type retConverter func(v reflect.Value) values.Value

// returnShape describes the return convention of a Go function type.
// Go functions return 0–2 values in one of four shapes:
//
//   - 0 returns: void (no value, no error)
//   - 1 return, error interface: error-only (success = nil error)
//   - 1 return, non-error: value-only (always succeeds from Go's perspective)
//   - 2 returns, (T, error): value + error (idiomatic Go error return)
//
// The error return must be the exact `error` interface type, not a concrete
// type implementing error. Concrete error types are non-nilable, and the
// wrapper calls IsNil() which would panic on non-interface reflect.Values.
type returnShape struct {
	hasError  bool         // true if the last return is the error interface
	valueType reflect.Type // non-nil if there's a non-error return value
}

// analyzeReturnShape inspects a function type's return values and classifies
// them into one of the four supported shapes. Used by both top-level
// buildFFISpec and nested makeCallbackArgConverter to share the validation
// logic for return conventions.
func analyzeReturnShape(fnType reflect.Type, errPrefix string) (returnShape, error) {
	numOut := fnType.NumOut()
	switch numOut {
	case 0:
		return returnShape{}, nil
	case 1:
		if fnType.Out(0) == errorType {
			return returnShape{hasError: true}, nil
		}
		return returnShape{valueType: fnType.Out(0)}, nil
	case 2:
		if fnType.Out(1) != errorType {
			return returnShape{}, werr.WrapForeignErrorf(
				werr.ErrFFIRegistration,
				"%s: second return value must be error, got %s", errPrefix, fnType.Out(1),
			)
		}
		return returnShape{hasError: true, valueType: fnType.Out(0)}, nil
	default:
		return returnShape{}, werr.WrapForeignErrorf(
			werr.ErrFFIRegistration,
			"%s: too many return values (%d), maximum is 2", errPrefix, numOut,
		)
	}
}

// ffiSpec holds pre-computed reflection data for a registered Go function.
type ffiSpec struct {
	name       string
	fnValue    reflect.Value
	fnType     reflect.Type
	argConvs   []argConverter
	retConv    retConverter // nil for void
	hasError   bool
	hasContext bool
	isVariadic bool
	paramCount int // Scheme-visible parameter count
}

// RegisterFunc registers a Go function as a Scheme primitive using
// natural Go signatures.
//
// Reflection-based FFI bridging: pre-computes argument and return converters
// at registration time using Go's reflect package. Each call uses the cached
// converters to translate between Scheme values and Go types, avoiding
// per-call reflection overhead.
// See BIBLIOGRAPHY.md "Reflection-Based FFI Bridging".
//
// # Supported Types
//
// Parameter types: int64, int, float64, string, bool, []byte, []T (typed
// slices), map[K]V, structs (exported fields), func(...) (callbacks),
// [Value], and [context.Context] (first param only).
//
// Return types: int64, int, float64, string, bool, []byte, []T, map[K]V,
// structs, [Value], error (last return only), and void.
//
// # Variadic Functions
//
// Variadic Go functions are supported. The variadic parameter receives
// all excess arguments from Scheme, converted element-by-element.
//
// # Context Forwarding
//
// If the first parameter is [context.Context], the VM's context is
// forwarded automatically and does not count toward the Scheme
// parameter count.
//
// # Callbacks
//
// Callback parameters (func types) receive a Go closure that invokes a
// Scheme procedure through a VM sub-context. Callbacks must be called
// synchronously during the registered function's execution. Storing a
// callback for later invocation or calling it from another goroutine is
// unsafe — the closure captures VM state that is not goroutine-safe.
//
// Returns an error wrapping [werr.ErrFFIRegistration] if fn is not a function or uses unsupported types.
func (p *Engine) RegisterFunc(name string, fn any) error {
	spec, err := buildFFISpec(name, fn)
	if err != nil {
		return err
	}

	wrapper := spec.makeWrapper()

	return p.RegisterPrimitive(PrimitiveSpec{
		Name:       name,
		ParamCount: spec.paramCount,
		IsVariadic: spec.isVariadic,
		Impl:       wrapper,
	})
}

// RegisterFuncs registers multiple Go functions as Scheme primitives.
// Each key in the map is the Scheme name; each value must be a Go function
// with a signature supported by [RegisterFunc].
//
// Registration stops on the first error. The error message includes the
// binding name that failed first. When multiple functions are invalid, the
// particular binding that fails first is non-deterministic because Go map
// iteration order is unspecified. Functions registered before the failure
// remain registered.
func (p *Engine) RegisterFuncs(funcs map[string]any) error {
	for name, fn := range funcs {
		err := p.RegisterFunc(name, fn)
		if err != nil {
			return err
		}
	}
	return nil
}

// buildFFISpec reflects on fn to produce an ffiSpec with pre-computed converters.
func buildFFISpec(name string, fn any) (*ffiSpec, error) {
	fnType := reflect.TypeOf(fn)
	if fnType == nil || fnType.Kind() != reflect.Func {
		return nil, werr.WrapForeignErrorf(werr.ErrFFIRegistration, "RegisterFunc %q: not a function", name)
	}

	spec := &ffiSpec{
		name:    name,
		fnValue: reflect.ValueOf(fn),
		fnType:  fnType,
	}

	// Detect context.Context as first parameter.
	paramStart := 0
	if fnType.NumIn() > 0 && fnType.In(0) == contextType {
		spec.hasContext = true
		paramStart = 1
	}

	// Validate no context.Context in non-first position.
	for i := paramStart; i < fnType.NumIn(); i++ {
		if fnType.In(i) == contextType {
			return nil, werr.WrapForeignErrorf(
				werr.ErrFFIRegistration,
				"RegisterFunc %q: context.Context must be first parameter", name,
			)
		}
	}

	// Detect Go variadic.
	spec.isVariadic = fnType.IsVariadic()

	// Build argument converters.
	numSchemeParams := fnType.NumIn() - paramStart
	spec.argConvs = make([]argConverter, numSchemeParams)

	for i := paramStart; i < fnType.NumIn(); i++ {
		paramType := fnType.In(i)
		idx := i - paramStart

		// For variadic functions, the last Go parameter is a slice.
		// Build a converter for the element type.
		convType := paramType
		if spec.isVariadic && i == fnType.NumIn()-1 {
			convType = paramType.Elem()
		}
		conv, err := makeArgConverter(name, idx+1, convType)
		if err != nil {
			return nil, err
		}
		spec.argConvs[idx] = conv
	}

	spec.paramCount = numSchemeParams

	// Analyze return types.
	shape, err := analyzeReturnShape(fnType, fmt.Sprintf("RegisterFunc %q", name))
	if err != nil {
		return nil, err
	}
	spec.hasError = shape.hasError
	if shape.valueType != nil {
		conv, err := makeRetConverter(name, shape.valueType)
		if err != nil {
			return nil, err
		}
		spec.retConv = conv
	}

	return spec, nil
}

// isSupportedMapKeyType returns whether a Go type can serve as a map key
// in FFI conversions. Only string, int64, int, and bool are allowed.
//
// Although float64 produces a Hashable Scheme value (*values.Float), it is
// excluded because IEEE 754 NaN != NaN breaks hashtable lookup invariants,
// and exact/inexact conversion can silently change keys during round-trips.
func isSupportedMapKeyType(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.String, reflect.Int64, reflect.Int, reflect.Bool:
		return true
	default:
		return false
	}
}

// fmtArgError creates a type conversion error for argument mismatches.
func fmtArgError(name string, pos int, expected string, got values.Value) error {
	return werr.WrapForeignErrorf(
		werr.ErrTypeConversion,
		"%s: argument %d: expected %s, got %s",
		name, pos, expected, got.SchemeString(),
	)
}
