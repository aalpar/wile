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
	"reflect"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

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
