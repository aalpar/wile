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
)

// Pre-computed reflect types for interface detection.
var (
	valueInterfaceType = reflect.TypeFor[Value]()
	contextType        = reflect.TypeFor[context.Context]()
	errorType          = reflect.TypeFor[error]()
)

// argConverter converts a Scheme value to a Go reflect.Value.
// ctx and mc are provided for composite types (slices, callbacks) that need
// VM access; scalar converters ignore both.
type argConverter func(ctx context.Context, mc *MachineContext, v values.Value) (reflect.Value, error)

// retConverter converts a Go reflect.Value to a Scheme value.
type retConverter func(v reflect.Value) values.Value

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
// natural Go signatures. Supported parameter types: int64, int, float64,
// string, bool, []byte, []T (typed slices), map[K]V, structs (exported fields),
// func(...) (callbacks), wile.Value, and context.Context (first param only).
// Supported return types: int64, int, float64, string, bool, []byte,
// []T, map[K]V, structs, wile.Value, error (last return only), and void.
//
// Variadic Go functions are supported. The variadic parameter receives
// all excess arguments from Scheme, converted element-by-element.
//
// If the first parameter is context.Context, the VM's context is forwarded
// automatically and does not count toward the Scheme parameter count.
//
// Callback parameters (func types) receive a Go closure that invokes a
// Scheme procedure through a VM sub-context. Callbacks must be called
// synchronously during the registered function's execution. Storing a
// callback for later invocation or calling it from another goroutine is
// unsafe — the closure captures VM state that is not goroutine-safe.
//
// Returns a *wile.Error if fn is not a function or uses unsupported types.
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

// buildFFISpec reflects on fn to produce an ffiSpec with pre-computed converters.
func buildFFISpec(name string, fn any) (*ffiSpec, error) {
	fnType := reflect.TypeOf(fn)
	if fnType == nil || fnType.Kind() != reflect.Func {
		return nil, &Error{Message: fmt.Sprintf("RegisterFunc %q: not a function", name)}
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
			return nil, &Error{
				Message: fmt.Sprintf("RegisterFunc %q: context.Context must be first parameter", name),
			}
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
		if spec.isVariadic && i == fnType.NumIn()-1 {
			elemType := paramType.Elem()
			conv, err := makeArgConverter(name, idx+1, elemType)
			if err != nil {
				return nil, err
			}
			spec.argConvs[idx] = conv
		} else {
			conv, err := makeArgConverter(name, idx+1, paramType)
			if err != nil {
				return nil, err
			}
			spec.argConvs[idx] = conv
		}
	}

	spec.paramCount = numSchemeParams

	// Analyze return types.
	numOut := fnType.NumOut()
	switch numOut {
	case 0:
		// void
	case 1:
		outType := fnType.Out(0)
		// Require the exact error interface, not concrete types that implement
		// error. Concrete error types are non-nilable, and the wrapper calls
		// IsNil() which would panic on non-interface kinds.
		if outType == errorType {
			spec.hasError = true
		} else {
			conv, err := makeRetConverter(name, outType)
			if err != nil {
				return nil, err
			}
			spec.retConv = conv
		}
	case 2:
		// Must be (T, error) with the exact error interface type.
		if fnType.Out(1) != errorType {
			return nil, &Error{
				Message: fmt.Sprintf("RegisterFunc %q: second return value must be error, got %s", name, fnType.Out(1)),
			}
		}
		spec.hasError = true
		conv, err := makeRetConverter(name, fnType.Out(0))
		if err != nil {
			return nil, err
		}
		spec.retConv = conv
	default:
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: too many return values (%d), maximum is 2", name, numOut),
		}
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
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
			return reflect.ValueOf(wrapValue(v)), nil
		}, nil
	}

	switch t.Kind() {
	case reflect.Int64:
		targetType := t
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				// Also accept floats that are exact integers.
				if f, fok := v.(*values.Float); fok { //nolint:gocritic
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
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "integer", v)
			}
			if n < math.MinInt || n > math.MaxInt {
				return reflect.Value{}, values.WrapForeignErrorf(
					values.ErrTypeConversion,
					"%s: argument %d: integer %d overflows int", name, pos, n,
				)
			}
			return reflect.ValueOf(int(n)).Convert(targetType), nil
		}, nil

	case reflect.Float64:
		targetType := t
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
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
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
			s, ok := v.(*values.String)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "string", v)
			}
			return reflect.ValueOf(s.Value).Convert(targetType), nil
		}, nil

	case reflect.Bool:
		targetType := t
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
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
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported parameter type at position %d: %s", name, pos, t),
		}
	}
}

// makeSliceArgConverter creates a converter for Go slice types.
// []byte is special-cased to ByteVector; all other element types use
// recursive inner converters that walk Scheme proper lists.
func makeSliceArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	elemType := t.Elem()

	// []byte special case: ByteVector.
	if elemType.Kind() == reflect.Uint8 {
		return func(_ context.Context, _ *MachineContext, v values.Value) (reflect.Value, error) {
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
	return func(ctx context.Context, mc *MachineContext, v values.Value) (reflect.Value, error) {
		_, isTuple := v.(values.Tuple)
		if !isTuple {
			return reflect.Value{}, fmtArgError(name, pos, "proper list", v)
		}
		result := reflect.MakeSlice(sliceType, 0, 0)
		_, walkErr := values.ForEach(ctx, v, func(innerCtx context.Context, _ int, _ bool, elem values.Value) error {
			converted, convErr := elemConv(innerCtx, mc, elem)
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
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported map key type at position %d: %s (must be string, int64, int, or bool)", name, pos, keyType),
		}
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
	return func(ctx context.Context, mc *MachineContext, v values.Value) (reflect.Value, error) {
		ht, ok := v.(*values.Hashtable)
		if !ok {
			return reflect.Value{}, fmtArgError(name, pos, "hashtable", v)
		}
		result := reflect.MakeMap(mapType)
		walkErr := ht.Entries(func(key values.Hashable, val values.Value) error {
			goKey, keyErr := keyConv(ctx, mc, key)
			if keyErr != nil {
				return keyErr
			}
			goVal, valErr := valConv(ctx, mc, val)
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
	return func(ctx context.Context, mc *MachineContext, v values.Value) (reflect.Value, error) {
		_, isTuple := v.(values.Tuple)
		if !isTuple {
			return reflect.Value{}, fmtArgError(name, pos, "proper list", v)
		}
		result := reflect.New(structType).Elem()
		_, walkErr := values.ForEach(ctx, v, func(innerCtx context.Context, _ int, _ bool, elem values.Value) error {
			pair, ok := elem.(*values.Pair)
			if !ok {
				return values.WrapForeignErrorf(
					values.ErrTypeConversion,
					"%s: argument %d: expected alist pair, got %s", name, pos, elem.SchemeString(),
				)
			}
			sym, ok := pair.Car().(*values.Symbol)
			if !ok {
				return values.WrapForeignErrorf(
					values.ErrTypeConversion,
					"%s: argument %d: alist key must be a symbol, got %s", name, pos, pair.Car().SchemeString(),
				)
			}
			fi, found := fieldMap[sym.Key]
			if !found {
				// Extra keys are silently ignored.
				return nil
			}
			converted, convErr := fi.conv(innerCtx, mc, pair.Cdr())
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
			return nil, &Error{
				Message: fmt.Sprintf("RegisterFunc %q: unsupported callback parameter type at position %d: %s", name, pos, t.In(i)),
			}
		}
		paramConvs[i] = conv
	}

	// Determine callback return shape.
	numOut := t.NumOut()
	var resultConv argConverter
	hasErrorReturn := false

	switch numOut {
	case 0:
		// void callback
	case 1:
		if t.Out(0) == errorType {
			hasErrorReturn = true
		} else {
			conv, err := makeArgConverter(name, pos, t.Out(0))
			if err != nil {
				return nil, &Error{
					Message: fmt.Sprintf("RegisterFunc %q: unsupported callback return type at position %d: %s", name, pos, t.Out(0)),
				}
			}
			resultConv = conv
		}
	case 2:
		if t.Out(1) != errorType {
			return nil, &Error{
				Message: fmt.Sprintf("RegisterFunc %q: callback second return must be error at position %d, got %s", name, pos, t.Out(1)),
			}
		}
		hasErrorReturn = true
		conv, err := makeArgConverter(name, pos, t.Out(0))
		if err != nil {
			return nil, &Error{
				Message: fmt.Sprintf("RegisterFunc %q: unsupported callback return type at position %d: %s", name, pos, t.Out(0)),
			}
		}
		resultConv = conv
	default:
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: callback at position %d has too many return values (%d)", name, pos, numOut),
		}
	}

	funcType := t
	return func(ctx context.Context, mc *MachineContext, v values.Value) (reflect.Value, error) {
		// Determine the callable type.
		var mcls *machine.MachineClosure
		var clcls *machine.CaseLambdaClosure
		var param *machine.Parameter

		switch proc := v.(type) {
		case *machine.MachineClosure:
			mcls = proc
		case *machine.CaseLambdaClosure:
			clcls = proc
		case *machine.Parameter:
			param = proc
		default:
			return reflect.Value{}, values.WrapForeignErrorf(
				values.ErrNotAProcedure,
				"%s: argument %d: expected procedure, got %s", name, pos, v.SchemeString(),
			)
		}

		goFunc := reflect.MakeFunc(funcType, func(goArgs []reflect.Value) []reflect.Value {
			// Convert Go args → Scheme values.
			schemeArgs := make([]values.Value, len(goArgs))
			for i, arg := range goArgs {
				schemeArgs[i] = paramConvs[i](arg)
			}

			// Parameter objects are callable with 0 args (get) or 1 arg (set).
			// Handle directly without VM sub-context.
			if param != nil {
				return callbackParameterResult(ctx, mc, funcType, resultConv, hasErrorReturn, param, schemeArgs)
			}

			// Invoke the Scheme procedure in a sub-context.
			sub := mc.NewSubContext()
			sub.SetContext(ctx)

			var applyErr error
			if mcls != nil {
				_, applyErr = sub.Apply(mcls, schemeArgs...)
			} else {
				_, applyErr = sub.ApplyCaseLambda(clcls, schemeArgs...)
			}
			if applyErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, applyErr)
			}

			runErr := sub.Run()
			if runErr != nil {
				var escapeErr *machine.ErrContinuationEscape
				if errors.As(runErr, &escapeErr) {
					return callbackErrorResult(funcType, hasErrorReturn, runErr)
				}
				if !errors.Is(runErr, machine.ErrMachineHalt) {
					return callbackErrorResult(funcType, hasErrorReturn, runErr)
				}
			}

			// Build Go return values.
			return callbackSuccessResult(ctx, mc, funcType, resultConv, hasErrorReturn, sub.GetValue())
		})

		return goFunc, nil
	}, nil
}

// callbackErrorResult builds reflect return values when a callback encounters an error.
// If the Go func type includes an error return, the error is returned normally.
// Otherwise, the error is panicked (standard Go pattern for unrecoverable callback failures).
func callbackErrorResult(funcType reflect.Type, hasErrorReturn bool, err error) []reflect.Value {
	if hasErrorReturn {
		out := make([]reflect.Value, funcType.NumOut())
		for i := range out {
			if i == funcType.NumOut()-1 {
				out[i] = reflect.ValueOf(&err).Elem()
			} else {
				out[i] = reflect.Zero(funcType.Out(i))
			}
		}
		return out
	}
	panic(err)
}

// callbackSuccessResult builds reflect return values from a successful callback invocation.
func callbackSuccessResult(
	ctx context.Context,
	mc *MachineContext,
	funcType reflect.Type,
	resultConv argConverter,
	hasErrorReturn bool,
	schemeResult values.Value,
) []reflect.Value {
	numOut := funcType.NumOut()
	out := make([]reflect.Value, numOut)

	if resultConv != nil {
		converted, convErr := resultConv(ctx, mc, schemeResult)
		if convErr != nil {
			if hasErrorReturn {
				for i := range out {
					if i == numOut-1 {
						out[i] = reflect.ValueOf(&convErr).Elem()
					} else {
						out[i] = reflect.Zero(funcType.Out(i))
					}
				}
				return out
			}
			panic(convErr)
		}
		out[0] = converted
	}

	if hasErrorReturn {
		// Set error return to nil.
		out[numOut-1] = reflect.Zero(errorType)
	}

	// Fill any unset slots with zero values (for void callbacks with error return).
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
	ctx context.Context,
	mc *MachineContext,
	funcType reflect.Type,
	resultConv argConverter,
	hasErrorReturn bool,
	param *machine.Parameter,
	args []values.Value,
) []reflect.Value {
	switch len(args) {
	case 0:
		return callbackSuccessResult(ctx, mc, funcType, resultConv, hasErrorReturn, param.Value())
	case 1:
		newVal := args[0]
		if param.HasConverter() {
			sub := mc.NewSubContext()
			sub.SetContext(ctx)
			_, applyErr := sub.Apply(param.Converter(), newVal)
			if applyErr != nil {
				return callbackErrorResult(funcType, hasErrorReturn, applyErr)
			}
			runErr := sub.Run()
			if runErr != nil && !errors.Is(runErr, machine.ErrMachineHalt) {
				return callbackErrorResult(funcType, hasErrorReturn, runErr)
			}
			newVal = sub.GetValue()
		}
		param.SetValue(newVal)
		return callbackSuccessResult(ctx, mc, funcType, resultConv, hasErrorReturn, values.Void)
	default:
		paramErr := values.WrapForeignErrorf(
			values.ErrWrongNumberOfArguments,
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
	case reflect.Int64:
		return func(v reflect.Value) values.Value {
			return values.NewInteger(v.Int())
		}, nil

	case reflect.Int:
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
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported return type: %s", name, t),
		}
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
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported map key type in return: %s (must be string, int64, int, or bool)", name, keyType),
		}
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
				panic(fmt.Sprintf("wile: RegisterFunc %q: map return conversion failed: %v", name, setErr))
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
	return func(ctx context.Context, mc *MachineContext) error {
		var args []reflect.Value

		// Forward context if needed.
		if s.hasContext {
			args = append(args, reflect.ValueOf(ctx))
		}

		if s.isVariadic {
			// Fixed args: mc.Arg(0) .. mc.Arg(paramCount-2)
			// Variadic list: mc.Arg(paramCount-1)
			fixedCount := s.paramCount - 1

			for i := range fixedCount {
				converted, err := s.argConvs[i](ctx, mc, mc.Arg(i))
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

			_, err := values.ForEach(ctx, varList, func(_ context.Context, _ int, _ bool, v values.Value) error {
				converted, convErr := variadicConv(ctx, mc, v)
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
				converted, err := s.argConvs[i](ctx, mc, mc.Arg(i))
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
	return values.WrapForeignErrorf(
		values.ErrTypeConversion,
		"%s: argument %d: expected %s, got %s",
		name, pos, expected, got.SchemeString(),
	)
}
