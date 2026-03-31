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
	"math"
	"reflect"

	"github.com/aalpar/wile/machine"
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

// makeArgConverter creates a converter for a single Go parameter type.
// Converters are recursive: composite types (slices, maps, structs) build
// inner converters for their element/field types at registration time.
func makeArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	// Only accept the exact wile.Value interface type. Concrete Value
	// implementers (e.g., *values.Integer) would cause reflect.Call to panic
	// since the converter produces a *wrappedValue, not the concrete type.
	if t == valueInterfaceType {
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			return reflect.ValueOf(wrapValue(v)), nil
		}, nil
	}

	switch t.Kind() {
	case reflect.Int64:
		targetType := t
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				// Also accept floats that are exact integers.
				f, fok := v.(*values.Float)
				if fok {
					fi := int64(f.Value)
					if float64(fi) == f.Value {
						return reflect.ValueOf(fi).Convert(targetType), nil
					}
				}
				return reflect.Value{}, fmtArgError(name, pos, "integer", v)
			}
			return reflect.ValueOf(n).Convert(targetType), nil
		}, nil

	case reflect.Int:
		targetType := t
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "integer", v)
			}
			if n < math.MinInt || n > math.MaxInt {
				return reflect.Value{}, werr.WrapForeignErrorf(
					werr.ErrTypeConversion,
					"%s: argument %d: integer %d overflows int", name, pos, n,
				)
			}
			return reflect.ValueOf(int(n)).Convert(targetType), nil
		}, nil

	case reflect.Float64:
		targetType := t
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			switch n := v.(type) {
			case *values.Float:
				return reflect.ValueOf(n.Value).Convert(targetType), nil
			case *values.Integer:
				return reflect.ValueOf(float64(n.Value)).Convert(targetType), nil
			case *values.BigInteger:
				if n.BigInt().IsInt64() {
					return reflect.ValueOf(float64(n.Int64())).Convert(targetType), nil
				}
				f, _ := n.BigInt().Float64()
				return reflect.ValueOf(f).Convert(targetType), nil
			case *values.Rational:
				f, _ := n.Rat().Float64()
				return reflect.ValueOf(f).Convert(targetType), nil
			default:
				return reflect.Value{}, fmtArgError(name, pos, "number", v)
			}
		}, nil

	case reflect.String:
		targetType := t
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			s, ok := v.(*values.String)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "string", v)
			}
			return reflect.ValueOf(s.Value).Convert(targetType), nil
		}, nil

	case reflect.Bool:
		targetType := t
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			b, ok := v.(*values.Boolean)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "boolean", v)
			}
			return reflect.ValueOf(b.Value).Convert(targetType), nil
		}, nil

	case reflect.Slice:
		return makeSliceArgConverter(name, pos, t)

	case reflect.Map:
		return makeMapArgConverter(name, pos, t)

	case reflect.Struct:
		return makeStructArgConverter(name, pos, t)

	case reflect.Func:
		return makeCallbackArgConverter(name, pos, t)

	default:
		return nil, werr.WrapForeignErrorf(
			werr.ErrFFIRegistration,
			"RegisterFunc %q: unsupported parameter type at position %d: %s", name, pos, t,
		)
	}
}

// makeSliceArgConverter creates a converter for Go slice types.
// []byte is special-cased to ByteVector; all other element types use
// recursive inner converters that walk Scheme proper lists.
func makeSliceArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	elemType := t.Elem()

	// []byte special case: ByteVector.
	if elemType.Kind() == reflect.Uint8 {
		return func(_ *MachineContext, v values.Value) (reflect.Value, error) {
			bv, ok := v.(*values.ByteVector)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "bytevector", v)
			}
			return reflect.ValueOf(bv.AsBytes()), nil
		}, nil
	}

	// Typed slice: build inner converter for element type.
	elemConv, err := makeArgConverter(name, pos, elemType)
	if err != nil {
		return nil, err
	}

	sliceType := t
	return func(mc *MachineContext, v values.Value) (reflect.Value, error) {
		_, isTuple := v.(values.Tuple)
		if !isTuple {
			return reflect.Value{}, fmtArgError(name, pos, "proper list", v)
		}
		result := reflect.MakeSlice(sliceType, 0, 0)
		_, walkErr := values.ForEach(mc.Context(), v, func(_ context.Context, _ int, _ bool, elem values.Value) error {
			converted, convErr := elemConv(mc, elem)
			if convErr != nil {
				return convErr
			}
			result = reflect.Append(result, converted)
			return nil
		})
		if walkErr != nil {
			return reflect.Value{}, walkErr
		}
		return result, nil
	}, nil
}

// makeMapArgConverter creates a converter for Go map types.
// Key types are restricted to Go types that produce Hashable Scheme values.
func makeMapArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	keyType := t.Key()
	valType := t.Elem()

	// Validate key type at registration time.
	if !isSupportedMapKeyType(keyType) {
		return nil, werr.WrapForeignErrorf(
			werr.ErrFFIRegistration,
			"RegisterFunc %q: unsupported map key type at position %d: %s (must be string, int64, int, or bool)", name, pos, keyType,
		)
	}

	keyConv, err := makeArgConverter(name, pos, keyType)
	if err != nil {
		return nil, err
	}
	valConv, err := makeArgConverter(name, pos, valType)
	if err != nil {
		return nil, err
	}

	mapType := t
	return func(mc *MachineContext, v values.Value) (reflect.Value, error) {
		ht, ok := v.(*values.Hashtable)
		if !ok {
			return reflect.Value{}, fmtArgError(name, pos, "hashtable", v)
		}
		result := reflect.MakeMap(mapType)
		walkErr := ht.Entries(func(key values.Hashable, val values.Value) error {
			goKey, keyErr := keyConv(mc, key)
			if keyErr != nil {
				return keyErr
			}
			goVal, valErr := valConv(mc, val)
			if valErr != nil {
				return valErr
			}
			result.SetMapIndex(goKey, goVal)
			return nil
		})
		if walkErr != nil {
			return reflect.Value{}, walkErr
		}
		return result, nil
	}, nil
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

// makeStructArgConverter creates a converter for Go struct types.
// Scheme alists ((FieldName . value) ...) are mapped to struct fields by
// matching the car symbol against exported field names.
func makeStructArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	type fieldInfo struct {
		index int
		conv  argConverter
	}

	fieldMap := make(map[string]fieldInfo)
	for i := range t.NumField() {
		f := t.Field(i)
		if !f.IsExported() {
			continue
		}
		conv, err := makeArgConverter(name, pos, f.Type)
		if err != nil {
			return nil, err
		}
		fieldMap[f.Name] = fieldInfo{index: i, conv: conv}
	}

	structType := t
	return func(mc *MachineContext, v values.Value) (reflect.Value, error) {
		_, isTuple := v.(values.Tuple)
		if !isTuple {
			return reflect.Value{}, fmtArgError(name, pos, "proper list", v)
		}
		result := reflect.New(structType).Elem()
		_, walkErr := values.ForEach(mc.Context(), v, func(_ context.Context, _ int, _ bool, elem values.Value) error {
			entry, ok := elem.(values.Tuple)
			if !ok {
				return werr.WrapForeignErrorf(
					werr.ErrTypeConversion,
					"%s: argument %d: expected alist pair, got %s", name, pos, elem.SchemeString(),
				)
			}
			sym, ok := entry.Car().(*values.Symbol)
			if !ok {
				return werr.WrapForeignErrorf(
					werr.ErrTypeConversion,
					"%s: argument %d: alist key must be a symbol, got %s", name, pos, entry.Car().SchemeString(),
				)
			}
			fi, found := fieldMap[sym.Key]
			if !found {
				// Extra keys are silently ignored.
				return nil
			}
			converted, convErr := fi.conv(mc, entry.Cdr())
			if convErr != nil {
				return convErr
			}
			result.Field(fi.index).Set(converted)
			return nil
		})
		if walkErr != nil {
			return reflect.Value{}, walkErr
		}
		return result, nil
	}, nil
}

// makeCallbackArgConverter creates a converter for Go function types used as
// callbacks. The Scheme procedure (lambda) is wrapped in a Go function via
// reflect.MakeFunc that invokes it through a VM sub-context.
//
// The returned Go closure captures the *MachineContext from the conversion
// call. Because MachineContext is single-goroutine VM state, the callback
// must be invoked synchronously within the same goroutine that called the
// registered function. Storing the callback or invoking it from another
// goroutine will race on VM internals and corrupt state.
//
// The direction of inner converters is inverted relative to the outer function:
// callback parameters use retConverters (Go→Scheme) and callback returns use
// argConverters (Scheme→Go), since data flows in the opposite direction.
func makeCallbackArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	// Build Go→Scheme converters for callback parameters.
	numIn := t.NumIn()
	paramConvs := make([]retConverter, numIn)
	for i := range numIn {
		conv, err := makeRetConverter(name, t.In(i))
		if err != nil {
			return nil, werr.WrapForeignErrorf(
				werr.ErrFFIRegistration,
				"RegisterFunc %q: unsupported callback parameter type at position %d: %s", name, pos, t.In(i),
			)
		}
		paramConvs[i] = conv
	}

	// Determine callback return shape. Callback returns use argConverters
	// (Scheme→Go direction) because data flows back from the Scheme procedure
	// into the Go caller — the inverse of the outer function's returns.
	shape, err := analyzeReturnShape(t, fmt.Sprintf("RegisterFunc %q: callback at position %d", name, pos))
	if err != nil {
		return nil, err
	}
	hasErrorReturn := shape.hasError
	var resultConv argConverter
	if shape.valueType != nil {
		conv, err := makeArgConverter(name, pos, shape.valueType)
		if err != nil {
			return nil, err
		}
		resultConv = conv
	}

	funcType := t
	return func(mc *MachineContext, v values.Value) (reflect.Value, error) {
		// Validate that the value is a supported callback procedure type.
		// Note: *machine.ComposableContinuation is callable via ApplyCallable, but is
		// intentionally not accepted here as a Go callback target because it represents
		// a captured continuation rather than a standalone procedure.
		switch v.(type) {
		case *machine.MachineClosure, *machine.CaseLambdaClosure, *machine.Parameter:
			// valid callback procedure
		default:
			return reflect.Value{}, werr.WrapForeignErrorf(
				werr.ErrNotAProcedure,
				"%s: argument %d: expected procedure, got %s", name, pos, v.SchemeString(),
			)
		}

		// Parameter objects are callable with 0 args (get) or 1 arg (set).
		// Handle directly without VM sub-context for efficiency.
		param, isParam := v.(*machine.Parameter)

		goFunc := reflect.MakeFunc(funcType, func(goArgs []reflect.Value) []reflect.Value {
			// Convert Go args → Scheme values.
			schemeArgs := make([]values.Value, len(goArgs))
			for i, arg := range goArgs {
				schemeArgs[i] = paramConvs[i](arg)
			}

			if isParam {
				return callbackParameterResult(mc, funcType, resultConv, hasErrorReturn, param, schemeArgs)
			}

			// Invoke the Scheme procedure in a sub-context.
			sub := mc.NewSubContext()
			defer machine.ReleaseSubContext(sub)
			sub.SetContext(mc.Context())

			_, applyErr := sub.ApplyCallable(v, schemeArgs...)
			if applyErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, applyErr)
			}

			runErr := sub.Run()
			if runErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, runErr)
			}

			// Build Go return values.
			return callbackSuccessResult(mc, funcType, resultConv, hasErrorReturn, sub.GetValue())
		})

		return goFunc, nil
	}, nil
}

// makeCallbackReturnWithError builds a reflect return value slice with all
// positions zero-valued except the last, which holds err.
func makeCallbackReturnWithError(funcType reflect.Type, err error) []reflect.Value {
	numOut := funcType.NumOut()
	out := make([]reflect.Value, numOut)
	for i := range out[:numOut-1] {
		out[i] = reflect.Zero(funcType.Out(i))
	}
	out[numOut-1] = reflect.ValueOf(&err).Elem()
	return out
}

// callbackErrorResult builds reflect return values when a callback encounters an error.
// If the Go func type includes an error return, the error is returned normally.
// Otherwise, the error is panicked (standard Go pattern for unrecoverable callback failures).
func callbackErrorResult(funcType reflect.Type, hasErrorReturn bool, err error) []reflect.Value {
	wrapped := werr.WrapForeignErrorWithCause(
		werr.ErrFFICallbackError, err,
		"callback invocation failed",
	)
	if hasErrorReturn {
		return makeCallbackReturnWithError(funcType, wrapped)
	}
	panic(wrapped)
}

// callbackSuccessResult builds reflect return values from a successful callback invocation.
func callbackSuccessResult(
	mc *MachineContext,
	funcType reflect.Type,
	resultConv argConverter,
	hasErrorReturn bool,
	schemeResult values.Value,
) []reflect.Value {
	numOut := funcType.NumOut()
	out := make([]reflect.Value, numOut)

	if resultConv != nil {
		converted, convErr := resultConv(mc, schemeResult)
		if convErr != nil {
			wrapped := werr.WrapForeignErrorWithCause(
				werr.ErrCallbackResultConversion, convErr,
				"callback result conversion failed",
			)
			if hasErrorReturn {
				return makeCallbackReturnWithError(funcType, wrapped)
			}
			panic(wrapped)
		}
		out[0] = converted
	}

	// Fill unset slots with zero values (nil error for hasErrorReturn,
	// zero value for void callbacks).
	for i := range out {
		if !out[i].IsValid() {
			out[i] = reflect.Zero(funcType.Out(i))
		}
	}

	return out
}

// callbackParameterResult handles invoking a Parameter object as a callback.
// Parameters accept 0 args (get current value) or 1 arg (set new value).
// Converter parameters are supported: when setting a value on a parameter
// that has a converter, the converter closure is invoked via a VM sub-context.
func callbackParameterResult(
	mc *MachineContext,
	funcType reflect.Type,
	resultConv argConverter,
	hasErrorReturn bool,
	param *machine.Parameter,
	args []values.Value,
) []reflect.Value {
	switch len(args) {
	case 0:
		return callbackSuccessResult(mc, funcType, resultConv, hasErrorReturn, param.Value())
	case 1:
		newVal := args[0]
		if param.HasConverter() {
			sub := mc.NewSubContext()
			defer machine.ReleaseSubContext(sub)
			sub.SetContext(mc.Context())
			_, applyErr := sub.ApplyCallable(param.Converter(), newVal)
			if applyErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, applyErr)
			}
			runErr := sub.Run()
			if runErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, runErr)
			}
			newVal = sub.GetValue()
		}
		param.SetValue(newVal)
		return callbackSuccessResult(mc, funcType, resultConv, hasErrorReturn, values.Void)
	default:
		paramErr := werr.WrapForeignErrorf(
			werr.ErrWrongNumberOfArguments,
			"parameter callback: expected 0 or 1 arguments, got %d", len(args),
		)
		return callbackErrorResult(funcType, hasErrorReturn, paramErr)
	}
}

// makeRetConverter creates a converter for a single Go return type.
// Converters are recursive for composite types (slices, maps, structs).
func makeRetConverter(name string, t reflect.Type) (retConverter, error) {
	// Only accept the exact wile.Value interface type. This avoids panics
	// from typed-nil returns and keeps the API surface predictable.
	if t == valueInterfaceType {
		return func(v reflect.Value) values.Value {
			if v.IsNil() {
				return values.Void
			}
			val := v.Interface().(Value)
			return unwrapValue(val)
		}, nil
	}

	switch t.Kind() {
	case reflect.Int64, reflect.Int:
		return func(v reflect.Value) values.Value {
			return values.NewInteger(v.Int())
		}, nil

	case reflect.Float64:
		return func(v reflect.Value) values.Value {
			return values.NewFloat(v.Float())
		}, nil

	case reflect.String:
		return func(v reflect.Value) values.Value {
			return values.NewString(v.String())
		}, nil

	case reflect.Bool:
		return func(v reflect.Value) values.Value {
			if v.Bool() {
				return values.TrueValue
			}
			return values.FalseValue
		}, nil

	case reflect.Slice:
		return makeSliceRetConverter(name, t)

	case reflect.Map:
		return makeMapRetConverter(name, t)

	case reflect.Struct:
		return makeStructRetConverter(name, t)

	default:
		return nil, werr.WrapForeignErrorf(
			werr.ErrFFIRegistration,
			"RegisterFunc %q: unsupported return type: %s", name, t,
		)
	}
}

// makeSliceRetConverter creates a return converter for Go slice types.
// []byte is special-cased to ByteVector; all other element types build
// Scheme proper lists from converted elements.
func makeSliceRetConverter(name string, t reflect.Type) (retConverter, error) {
	elemType := t.Elem()

	// []byte special case: ByteVector.
	if elemType.Kind() == reflect.Uint8 {
		return func(v reflect.Value) values.Value {
			return values.NewByteVectorFromBytes(v.Bytes()...)
		}, nil
	}

	elemConv, err := makeRetConverter(name, elemType)
	if err != nil {
		return nil, err
	}

	return func(v reflect.Value) values.Value {
		if v.IsNil() || v.Len() == 0 {
			return values.EmptyList
		}
		elems := make([]values.Value, v.Len())
		for i := range v.Len() {
			elems[i] = elemConv(v.Index(i))
		}
		return values.List(elems...)
	}, nil
}

// makeMapRetConverter creates a return converter for Go map types.
// Key types are validated at registration time using the same restrictions
// as makeMapArgConverter to ensure bidirectional consistency.
func makeMapRetConverter(name string, t reflect.Type) (retConverter, error) {
	keyType := t.Key()
	if !isSupportedMapKeyType(keyType) {
		return nil, werr.WrapForeignErrorf(
			werr.ErrFFIRegistration,
			"RegisterFunc %q: unsupported map key type in return: %s (must be string, int64, int, or bool)", name, keyType,
		)
	}

	keyConv, err := makeRetConverter(name, t.Key())
	if err != nil {
		return nil, err
	}
	valConv, err := makeRetConverter(name, t.Elem())
	if err != nil {
		return nil, err
	}

	return func(v reflect.Value) values.Value {
		ht := values.NewEmptyHashtable()
		if v.IsNil() {
			return ht
		}
		iter := v.MapRange()
		for iter.Next() {
			schemeKey := keyConv(iter.Key())
			schemeVal := valConv(iter.Value())
			setErr := ht.Set(schemeKey, schemeVal)
			if setErr != nil {
				panic(werr.WrapForeignErrorWithCause(
					werr.ErrHashtableInsertionFailed, setErr,
					"RegisterFunc %q: map return conversion failed inserting key %v",
					name, iter.Key(),
				))
			}
		}
		return ht
	}, nil
}

// makeStructRetConverter creates a return converter for Go struct types.
// Exported fields are converted to a Scheme alist ((FieldName . value) ...).
func makeStructRetConverter(name string, t reflect.Type) (retConverter, error) {
	type fieldConvInfo struct {
		name  string
		index int
		conv  retConverter
	}

	var fields []fieldConvInfo
	for i := range t.NumField() {
		f := t.Field(i)
		if !f.IsExported() {
			continue
		}
		conv, err := makeRetConverter(name, f.Type)
		if err != nil {
			return nil, err
		}
		fields = append(fields, fieldConvInfo{name: f.Name, index: i, conv: conv})
	}

	return func(v reflect.Value) values.Value {
		pairs := make([]values.Value, len(fields))
		for i, f := range fields {
			pairs[i] = values.NewCons(
				values.NewSymbol(f.name),
				f.conv(v.Field(f.index)),
			)
		}
		return values.List(pairs...)
	}, nil
}

// makeWrapper generates the ForeignFunction closure that bridges between
// the VM calling convention and the Go function.
func (s *ffiSpec) makeWrapper() ForeignFunction {
	return func(mc *MachineContext) (returnErr error) {
		defer func() {
			r := recover()
			if r == nil {
				return
			}
			err, ok := r.(error)
			if ok {
				var fe *werr.ForeignError
				if errors.As(err, &fe) {
					returnErr = err
					return
				}
			}
			panic(fmt.Sprintf("FFI %q: %v", s.name, r))
		}()

		var args []reflect.Value

		// Forward context if needed.
		if s.hasContext {
			args = append(args, reflect.ValueOf(mc.Context()))
		}

		if s.isVariadic {
			// Fixed args: mc.Arg(0) .. mc.Arg(paramCount-2)
			// Variadic list: mc.Arg(paramCount-1)
			fixedCount := s.paramCount - 1

			for i := range fixedCount {
				converted, err := s.argConvs[i](mc, mc.Arg(i))
				if err != nil {
					return err
				}
				args = append(args, converted)
			}

			// Walk the Scheme list for variadic args.
			variadicConv := s.argConvs[s.paramCount-1]
			varList := mc.Arg(fixedCount)

			_, isTuple := varList.(values.Tuple)
			if !isTuple {
				return fmtArgError(s.name, fixedCount+1, "proper list", varList)
			}

			_, err := values.ForEach(mc.Context(), varList, func(_ context.Context, _ int, _ bool, v values.Value) error {
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
		} else {
			for i := range s.paramCount {
				converted, err := s.argConvs[i](mc, mc.Arg(i))
				if err != nil {
					return err
				}
				args = append(args, converted)
			}
		}

		// Call the Go function. Use Call (not CallSlice) since we've
		// already expanded variadic args into individual reflect.Values.
		results := s.fnValue.Call(args)

		// Process return values.
		switch {
		case s.retConv != nil && s.hasError:
			// (T, error)
			errVal := results[1]
			if !errVal.IsNil() {
				return errVal.Interface().(error)
			}
			mc.SetValue(s.retConv(results[0]))

		case s.retConv != nil:
			// T (no error)
			mc.SetValue(s.retConv(results[0]))

		case s.hasError:
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

// fmtArgError creates a type conversion error for argument mismatches.
func fmtArgError(name string, pos int, expected string, got values.Value) error {
	return werr.WrapForeignErrorf(
		werr.ErrTypeConversion,
		"%s: argument %d: expected %s, got %s",
		name, pos, expected, got.SchemeString(),
	)
}
