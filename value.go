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
	"math/big"

	"github.com/aalpar/wile/values"
)

// Value represents a Scheme value in the public API.
type Value interface {
	// SchemeString returns the Scheme representation.
	SchemeString() string
	// IsVoid returns true if this is the void value.
	IsVoid() bool
	// Internal returns the underlying values.Value for advanced use.
	// This is exported for use by testing packages and advanced embedding scenarios.
	Internal() values.Value
	// internal returns the underlying values.Value (unexported alias for Internal)
	internal() values.Value
}

type wrappedValue struct {
	v values.Value
}

func (p *wrappedValue) SchemeString() string {
	return p.v.SchemeString()
}

func (p *wrappedValue) IsVoid() bool {
	return values.IsVoid(p.v)
}

func (p *wrappedValue) Internal() values.Value {
	return p.v
}

func (p *wrappedValue) internal() values.Value {
	return p.v
}

func wrapValue(v values.Value) Value {
	if v == nil {
		return nil
	}
	return &wrappedValue{v: v}
}

func unwrapValue(v Value) values.Value {
	if v == nil {
		return nil
	}
	return v.internal()
}

// Helper constructors for creating Scheme values from Go values

// NewInteger creates a Scheme integer.
func NewInteger(n int64) Value {
	return wrapValue(values.NewInteger(n))
}

// NewFloat creates a Scheme inexact real.
func NewFloat(f float64) Value {
	return wrapValue(values.NewFloat(f))
}

// NewString creates a Scheme string.
func NewString(s string) Value {
	return wrapValue(values.NewString(s))
}

// NewSymbol creates a Scheme symbol.
func NewSymbol(s string) Value {
	return wrapValue(values.NewSymbol(s))
}

// NewBoolean creates a Scheme boolean.
func NewBoolean(b bool) Value {
	if b {
		return wrapValue(values.TrueValue)
	}
	return wrapValue(values.FalseValue)
}

// NewBigInteger creates a big integer value from a big.Int.
func NewBigInteger(n *big.Int) Value {
	return wrapValue(values.NewBigInteger(n))
}

// NewBigIntegerFromInt64 creates a big integer value from an int64.
func NewBigIntegerFromInt64(n int64) Value {
	return wrapValue(values.NewBigIntegerFromInt64(n))
}

// NewBigIntegerFromString creates a big integer from a string in the given base.
// Returns nil if the string is not a valid integer.
func NewBigIntegerFromString(s string, base int) Value {
	v := values.NewBigIntegerFromString(s, base)
	if v == nil {
		return nil
	}
	return wrapValue(v)
}

// NewBigFloat creates a big float value from a big.Float.
func NewBigFloat(f *big.Float) Value {
	return wrapValue(values.NewBigFloat(f))
}

// NewBigFloatFromFloat64 creates a big float value from a float64.
func NewBigFloatFromFloat64(f float64) Value {
	return wrapValue(values.NewBigFloatFromFloat64(f))
}

// NewBigFloatFromString creates a big float from a string.
// Returns nil if the string is not a valid float.
func NewBigFloatFromString(s string) Value {
	v := values.NewBigFloatFromString(s)
	if v == nil {
		return nil
	}
	return wrapValue(v)
}

// NewRational creates a Scheme exact rational number.
func NewRational(num, denom int64) Value {
	return wrapValue(values.NewRational(num, denom))
}

// NewRationalFromBigInt creates a Scheme exact rational from big.Int numerator and denominator.
func NewRationalFromBigInt(num, denom *big.Int) Value {
	return wrapValue(values.NewRationalFromBigInt(num, denom))
}

// NewComplex creates a Scheme complex number from a Go complex128.
func NewComplex(v complex128) Value {
	return wrapValue(values.NewComplex(v))
}

// NewComplexFromParts creates a Scheme complex number from real and imaginary parts.
func NewComplexFromParts(realPart, imagPart float64) Value {
	return wrapValue(values.NewComplexFromParts(realPart, imagPart))
}

// NewVector creates a Scheme vector from values.
func NewVector(vals ...Value) Value {
	unwrapped := make([]values.Value, len(vals))
	for i, v := range vals {
		unwrapped[i] = unwrapValue(v)
	}
	return wrapValue(values.NewVector(unwrapped...))
}

// NewList creates a Scheme list from values.
func NewList(vals ...Value) Value {
	if len(vals) == 0 {
		return wrapValue(values.EmptyList)
	}
	// Build list from back to front
	var result values.Value = values.EmptyList
	for i := len(vals) - 1; i >= 0; i-- {
		result = values.NewCons(unwrapValue(vals[i]), result)
	}
	return wrapValue(result)
}

// Null is the empty list.
var Null = wrapValue(values.EmptyList)

// Void is the void value.
var Void = wrapValue(values.Void)

// True is the #t value.
var True = wrapValue(values.TrueValue)

// False is the #f value.
var False = wrapValue(values.FalseValue)
