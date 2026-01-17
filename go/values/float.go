// Copyright 2025 Aaron Alpar
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

package values

import (
	"strconv"
)

var (
	_ Value  = (*Float)(nil)
	_ Number = (*Float)(nil)
	// _ Comparable = (*Float)(nil)
)

// Float represents a Scheme floating-point number.
type Float struct {
	Value float64
}

// NewFloat creates a new float value.
func NewFloat(v float64) *Float {
	q := &Float{Value: v}
	return q
}

// Datum returns the underlying float64 value.
func (p *Float) Datum() float64 {
	return p.Value
}

// Add returns the sum of two numbers.
func (p *Float) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value + float64(v.Value))
	case *Float:
		return NewFloat(p.Value + v.Value)
	case *Rational:
		return NewFloat(p.Value + v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) + v.Value)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of two numbers.
func (p *Float) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value - float64(v.Value))
	case *Float:
		return NewFloat(p.Value - v.Value)
	case *Rational:
		return NewFloat(p.Value - v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) - v.Value)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of two numbers.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
func (p *Float) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value * float64(v.Value))
	case *Float:
		return NewFloat(p.Value * v.Value)
	case *Rational:
		return NewFloat(p.Value * v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) * v.Value)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this float and another number.
func (p *Float) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		return NewFloat(p.Value / float64(v.Value))
	case *Float:
		return NewFloat(p.Value / v.Value)
	case *Rational:
		return NewFloat(p.Value / v.Float64())
	case *Complex:
		return NewComplex(complex(p.Value, 0) / v.Value)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if this float is zero.
func (p *Float) IsZero() bool {
	return p.Value == 0.0
}

// LessThan returns true if this float is less than another number.
func (p *Float) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < float64(v.Value)
	case *Float:
		return p.Value < v.Value
	case *Rational:
		return p.Value < v.Float64()
	case *Complex:
		return p.Value < real(v.Value)
	}
	panic(ErrNotANumber)
}

// IsVoid returns true if the float is nil.
func (p *Float) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both floats have the same value.
func (p *Float) EqualTo(v Value) bool {
	if other, ok := v.(*Float); ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of the float.
func (p *Float) SchemeString() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}

func (p *Float) String() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}
