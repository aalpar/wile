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
)

var (
	_ Value  = (*BigInteger)(nil)
	_ Number = (*BigInteger)(nil)
)

// BigInteger represents an arbitrary-precision integer.
// Created with the #z prefix in Scheme (e.g., #z12345678901234567890).
//
// R7RS §6.2.1: Integers are exact numbers in the numeric tower hierarchy:
//   number ⊃ complex ⊃ real ⊃ rational ⊃ integer
//
// R7RS §6.2.2: BigInteger is always exact. Operations on exact numbers
// produce exact results when mathematically well-defined.
//
// R7RS §6.2.3: Implementations may support arbitrarily large exact integers.
// BigInteger provides this capability using Go's math/big.Int.
type BigInteger struct {
	value *big.Int
}

// NewBigInteger creates a new BigInteger from a big.Int.
func NewBigInteger(v *big.Int) *BigInteger {
	return &BigInteger{value: new(big.Int).Set(v)}
}

// NewBigIntegerFromInt64 creates a new BigInteger from an int64.
func NewBigIntegerFromInt64(v int64) *BigInteger {
	return &BigInteger{value: big.NewInt(v)}
}

// NewBigIntegerFromString creates a new BigInteger from a string.
// Returns nil if the string is not a valid integer.
func NewBigIntegerFromString(s string, base int) *BigInteger {
	v := new(big.Int)
	_, ok := v.SetString(s, base)
	if !ok {
		return nil
	}
	return &BigInteger{value: v}
}

func newBigIntFromOp(fn func(p0, v0, v1 *big.Int) *big.Int, v0, v1 *big.Int) *big.Int {
	return fn(new(big.Int), v0, v1)
}

func float64FromBigInt(bi *big.Int) float64 {
	f, _ := new(big.Float).SetInt(bi).Float64()
	return f
}

// BigInt returns the underlying big.Int value.
func (p *BigInteger) BigInt() *big.Int {
	return p.value
}

// Int64 returns the value as int64 (may overflow for large values).
func (p *BigInteger) Int64() int64 {
	return p.value.Int64()
}

// Add returns the sum of this BigInteger and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact (BigInteger),
// exact + inexact = inexact (Float/Complex).
func (p *BigInteger) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, v.value)}
	case *Integer:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, big.NewInt(v.Value))}
	case *Float:
		// no constructor for big.Float from big.Int, so convert via float64
		f := float64FromBigInt(p.value)
		return NewFloat(f + v.Value)
	case *Rational:
		// Convert BigInteger to Rational and add
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Add(pRat, v.Rat()))
	case *Complex:
		f := float64FromBigInt(p.value)
		// no constructor for big.Float from big.Int, so convert via float64
		return NewComplex(complex(f, 0) + v.Datum())
	}
	return nil
}

// Subtract returns the difference of this BigInteger and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *BigInteger) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, v.value)}
	case *Integer:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, big.NewInt(v.Value))}
	case *Float:
		f := float64FromBigInt(p.value)
		return NewFloat(f - v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Sub(pRat, v.Rat()))
	case *Complex:
		f := float64FromBigInt(p.value)
		// no constructor for big.Float from big.Int, so convert via float64
		return NewComplex(complex(f, 0) - v.Datum())
	}
	return nil
}

// Multiply returns the product of this BigInteger and another number.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
func (p *BigInteger) Multiply(o Number) Number {
	if o.IsZero() {
		return NewBigIntegerFromInt64(0)
	}
	if p.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *BigInteger:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, v.value)}
	case *Integer:
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, big.NewInt(v.Value))}
	case *Float:
		f := float64FromBigInt(p.value)
		return NewFloat(f * v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Mul(pRat, v.Rat()))
	case *Complex:
		f := float64FromBigInt(p.value)
		return NewComplex(complex(f, 0) * v.Datum())
	}
	return nil
}

// Divide returns the quotient of this BigInteger and another number.
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// For exact arguments, / may return a non-integer (Rational) when the
// mathematical result is not an integer. Returns BigInteger only when
// the division is exact (remainder is zero).
//
// R7RS §6.2.2 Exactness: exact / exact = exact (BigInteger or Rational),
// exact / inexact = inexact (Float or Complex).
func (p *BigInteger) Divide(o Number) Number {
	if o.IsZero() {
		return nil // Division by zero
	}
	switch v := o.(type) {
	case *BigInteger:
		// Check if division is exact
		quo, rem := new(big.Int).QuoRem(p.value, v.value, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}
		}
		return NewRationalFromBigInt(p.value, v.value)
	case *Integer:
		// Check if division is exact
		divisor := big.NewInt(v.Value)
		quo, rem := new(big.Int).QuoRem(p.value, divisor, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}
		}
		return NewRationalFromBigInt(p.value, divisor)
	case *Float:
		f := float64FromBigInt(p.value)
		return NewFloat(f / v.Value)
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return NewRationalFromRat(new(big.Rat).Quo(pRat, v.Rat()))
	case *Complex:
		f := float64FromBigInt(p.value)
		return NewComplex(complex(f, 0) / v.Datum())
	}
	return nil
}

// Negate returns the negation of this BigInteger.
func (p *BigInteger) Negate() Number {
	return &BigInteger{value: new(big.Int).Neg(p.value)}
}

// IsZero returns true if this BigInteger is zero.
func (p *BigInteger) IsZero() bool {
	return p.value.Sign() == 0
}

// LessThan returns true if this BigInteger is less than another number.
//
// R7RS §6.2.6: The < procedure returns #t if its arguments are monotonically
// increasing. Comparison across numeric types uses mathematical value.
func (p *BigInteger) LessThan(o Number) bool {
	return p.Compare(o) < 0
}

// IsNegative returns true if this BigInteger is negative.
func (p *BigInteger) IsNegative() bool {
	return p.value.Sign() < 0
}

// IsPositive returns true if this BigInteger is positive.
func (p *BigInteger) IsPositive() bool {
	return p.value.Sign() > 0
}

// IsExact returns true as BigInteger is always exact.
//
// R7RS §6.2.2: Integers (including BigInteger) are always exact.
func (p *BigInteger) IsExact() bool {
	return true
}

// ToExact returns this BigInteger as an exact number.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
// Since BigInteger is already exact, it returns itself.
func (p *BigInteger) ToExact() Number {
	return p
}

// ToInexact returns this BigInteger converted to an inexact float.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
// Converts to Float (float64), which may lose precision for large values.
//
// R7RS §6.2.3: The inexact representation may have limited precision,
// but the conversion should be as close as practical.
func (p *BigInteger) ToInexact() Number {
	f := float64FromBigInt(p.value)
	return NewFloat(f)
}

// Compare compares this BigInteger with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of
// exactness. Returns -1, 0, or 1 for less than, equal, or greater than.
func (p *BigInteger) Compare(o Number) int {
	switch v := o.(type) {
	case *BigInteger:
		return p.value.Cmp(v.value)
	case *Integer:
		return p.value.Cmp(big.NewInt(v.Value))
	case *Float:
		f := float64FromBigInt(p.value)
		if f < v.Value {
			return -1
		} else if f > v.Value {
			return 1
		}
		return 0
	case *Rational:
		pRat := new(big.Rat).SetInt(p.value)
		return pRat.Cmp(v.Rat())
	}
	return 0
}

// SchemeString returns the Scheme representation of this BigInteger.
func (p *BigInteger) SchemeString() string {
	return p.value.String()
}

// IsVoid returns true if this BigInteger is nil.
func (p *BigInteger) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this BigInteger equals another value.
//
// R7RS §6.2.6: The = procedure compares numerical values for equality.
// BigInteger also compares equal to Integer when values match.
func (p *BigInteger) EqualTo(o Value) bool {
	v, ok := o.(*BigInteger)
	if !ok {
		// Also check if equal to regular Integer
		if i, ok := o.(*Integer); ok {
			return p.value.Cmp(big.NewInt(i.Value)) == 0
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	return p.value.Cmp(v.value) == 0
}
