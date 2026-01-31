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
	"wile/values"
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
	return p.v == values.Void
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
