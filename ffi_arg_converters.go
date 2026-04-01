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
	"math"
	"reflect"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

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
