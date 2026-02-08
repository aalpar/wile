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
)

// Pre-computed reflect types for interface detection.
var (
	valueInterfaceType = reflect.TypeFor[Value]()
	contextType        = reflect.TypeFor[context.Context]()
	errorType          = reflect.TypeFor[error]()
)

// argConverter converts a Scheme value to a Go reflect.Value.
type argConverter func(v values.Value) (reflect.Value, error)

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
// string, bool, []byte, wile.Value, and context.Context (first param only).
// Supported return types: int64, int, float64, string, bool, []byte,
// wile.Value, error (last return only), and void.
//
// Variadic Go functions are supported. The variadic parameter receives
// all excess arguments from Scheme, converted element-by-element.
//
// If the first parameter is context.Context, the VM's context is forwarded
// automatically and does not count toward the Scheme parameter count.
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
		if outType.Implements(errorType) {
			spec.hasError = true
		} else {
			conv, err := makeRetConverter(name, outType)
			if err != nil {
				return nil, err
			}
			spec.retConv = conv
		}
	case 2:
		// Must be (T, error).
		if !fnType.Out(1).Implements(errorType) {
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
func makeArgConverter(name string, pos int, t reflect.Type) (argConverter, error) {
	// Check Value interface first.
	if t.Implements(valueInterfaceType) {
		return func(v values.Value) (reflect.Value, error) {
			return reflect.ValueOf(wrapValue(v)), nil
		}, nil
	}

	switch t.Kind() {
	case reflect.Int64:
		return func(v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				// Also accept floats that are exact integers.
				if f, fok := v.(*values.Float); fok {
					fi := int64(f.Value)
					if float64(fi) == f.Value {
						return reflect.ValueOf(fi), nil
					}
				}
				return reflect.Value{}, fmtArgError(name, pos, "integer", v)
			}
			return reflect.ValueOf(n), nil
		}, nil

	case reflect.Int:
		return func(v values.Value) (reflect.Value, error) {
			n, ok := values.ExactInteger(v)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "integer", v)
			}
			return reflect.ValueOf(int(n)), nil
		}, nil

	case reflect.Float64:
		return func(v values.Value) (reflect.Value, error) {
			switch n := v.(type) {
			case *values.Float:
				return reflect.ValueOf(n.Value), nil
			case *values.Integer:
				return reflect.ValueOf(float64(n.Value)), nil
			case *values.BigInteger:
				if n.BigInt().IsInt64() {
					return reflect.ValueOf(float64(n.Int64())), nil
				}
				f, _ := n.BigInt().Float64()
				return reflect.ValueOf(f), nil
			case *values.Rational:
				f, _ := n.Rat().Float64()
				return reflect.ValueOf(f), nil
			default:
				return reflect.Value{}, fmtArgError(name, pos, "number", v)
			}
		}, nil

	case reflect.String:
		return func(v values.Value) (reflect.Value, error) {
			s, ok := v.(*values.String)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "string", v)
			}
			return reflect.ValueOf(s.Value), nil
		}, nil

	case reflect.Bool:
		return func(v values.Value) (reflect.Value, error) {
			b, ok := v.(*values.Boolean)
			if !ok {
				return reflect.Value{}, fmtArgError(name, pos, "boolean", v)
			}
			return reflect.ValueOf(b.Value), nil
		}, nil

	case reflect.Slice:
		if t.Elem().Kind() == reflect.Uint8 {
			return func(v values.Value) (reflect.Value, error) {
				bv, ok := v.(*values.ByteVector)
				if !ok {
					return reflect.Value{}, fmtArgError(name, pos, "bytevector", v)
				}
				return reflect.ValueOf(bv.AsBytes()), nil
			}, nil
		}
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported parameter type at position %d: %s", name, pos, t),
		}

	default:
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported parameter type at position %d: %s", name, pos, t),
		}
	}
}

// makeRetConverter creates a converter for a single Go return type.
func makeRetConverter(name string, t reflect.Type) (retConverter, error) {
	// Check Value interface first.
	if t.Implements(valueInterfaceType) {
		return func(v reflect.Value) values.Value {
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
		if t.Elem().Kind() == reflect.Uint8 {
			return func(v reflect.Value) values.Value {
				return values.NewByteVectorFromBytes(v.Bytes()...)
			}, nil
		}
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported return type: %s", name, t),
		}

	default:
		return nil, &Error{
			Message: fmt.Sprintf("RegisterFunc %q: unsupported return type: %s", name, t),
		}
	}
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
				converted, err := s.argConvs[i](mc.Arg(i))
				if err != nil {
					return err
				}
				args = append(args, converted)
			}

			// Walk the Scheme list for variadic args.
			variadicConv := s.argConvs[s.paramCount-1]
			varList := mc.Arg(fixedCount)

			_, err := values.ForEach(ctx, varList, func(_ context.Context, _ int, _ bool, v values.Value) error {
				converted, convErr := variadicConv(v)
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
				converted, err := s.argConvs[i](mc.Arg(i))
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
