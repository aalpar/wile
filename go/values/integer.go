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
	"math/big"
	"strconv"
)

var (
	_ Value  = (*Integer)(nil)
	_ Number = (*Integer)(nil)
	// _ Comparable = (*Integer)(nil)
)

// Integer cache for small integers (-32768 to 32767).
// This avoids allocations for commonly used integer values.
// Uses 16-bit range to cover most practical small integers.
const (
	intCacheMin = -32768
	intCacheMax = 32767
)

var intCache [intCacheMax - intCacheMin + 1]*Integer

func init() {
	for i := int64(intCacheMin); i <= intCacheMax; i++ {
		intCache[i-intCacheMin] = &Integer{Value: i}
	}
}

// Integer represents a Scheme integer value.
type Integer struct {
	Value int64
}

// NewInteger returns an Integer value. Small integers in the range
// -256 to 255 are cached and return the same pointer for the same value.
func NewInteger(v int64) *Integer {
	if v >= intCacheMin && v <= intCacheMax {
		return intCache[v-intCacheMin]
	}
	return &Integer{Value: v}
}

// Datum returns the underlying int64 value.
func (p *Integer) Datum() int64 {
	return p.Value
}

// Add returns the sum of this integer and another number.
func (p *Integer) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value + v.Value)
	case *Float:
		return NewFloat(float64(p.Value) + float64(v.Value))
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Add(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) + v.Value)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of this integer and another number.
func (p *Integer) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value - v.Value)
	case *Float:
		return NewFloat(float64(p.Value) - v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Sub(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) - v.Value)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of this integer and another number.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
// Exception: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
func (p *Integer) Multiply(o Number) Number {
	if o.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Integer:
		return NewInteger(p.Value * v.Value)
	case *Float:
		return NewFloat(float64(p.Value) * v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Mul(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) * v.Value)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this integer and another number.
func (p *Integer) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Integer:
		result := NewRational(p.Value, v.Value)
		if result.IsInteger() {
			return NewInteger(result.NumInt64())
		}
		return result
	case *Float:
		return NewFloat(float64(p.Value) / v.Value)
	case *Rational:
		self := big.NewRat(p.Value, 1)
		result := new(big.Rat).Quo(self, v.Rat())
		return &Rational{value: result}
	case *Complex:
		return NewComplex(complex(float64(p.Value), 0) / v.Value)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if this integer is zero.
func (p *Integer) IsZero() bool {
	return p.Value == 0
}

// LessThan returns true if this integer is less than another number.
func (p *Integer) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Integer:
		return p.Value < v.Value
	case *Float:
		return float64(p.Value) < v.Value
	case *Rational:
		self := big.NewRat(p.Value, 1)
		return self.Cmp(v.Rat()) < 0
	case *Complex:
		return float64(p.Value) < real(v.Value)
	}
	panic(ErrNotANumber)
}

// IsVoid returns true if this integer is nil.
func (p *Integer) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both integers have the same value.
func (p *Integer) EqualTo(v Value) bool {
	other, ok := v.(*Integer)
	if ok {
		return p.Value == other.Value
	}
	return false
}

// SchemeString returns the Scheme representation of this integer.
func (p *Integer) SchemeString() string {
	return strconv.FormatInt(p.Value, 10)
}
