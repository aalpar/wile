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

package values

import (
	"math/big"
)

var (
	_ Value      = (*BigFloat)(nil)
	_ Number     = (*BigFloat)(nil)
	_ RealNumber = (*BigFloat)(nil)
	_ Hashable   = (*BigFloat)(nil)
)

// DefaultBigFloatPrecision is the default precision for BigFloat values.
const DefaultBigFloatPrecision = 256

// BigFloat represents an arbitrary-precision floating-point number.
// Created with the #m prefix in Scheme (e.g., #m3.14159265358979323846).
type BigFloat struct {
	value *big.Float
}

// NewBigFloat creates a new BigFloat from a big.Float.
func NewBigFloat(v *big.Float) *BigFloat {
	return &BigFloat{value: new(big.Float).Copy(v)}
}

// NewBigFloatFromFloat64 creates a new BigFloat from a float64.
func NewBigFloatFromFloat64(v float64) *BigFloat {
	return &BigFloat{value: big.NewFloat(v).SetPrec(DefaultBigFloatPrecision)}
}

// NewBigFloatFromString creates a new BigFloat from a string.
// Returns nil if the string is not a valid number.
func NewBigFloatFromString(s string) *BigFloat {
	v, _, err := big.ParseFloat(s, 10, DefaultBigFloatPrecision, big.ToNearestEven)
	if err != nil {
		return nil
	}
	return &BigFloat{value: v}
}

// BigFloatValue returns the underlying big.Float value.
func (p *BigFloat) BigFloatValue() *big.Float {
	return p.value
}

// Float64 returns the value as float64 (may lose precision).
func (p *BigFloat) Float64() float64 {
	f, _ := p.value.Float64()
	return f
}

// HashCode returns a hash code for this BigFloat.
// Uses the canonical inexact-family hash so that Float and BigFloat
// produce identical hashes for equal values.
func (p *BigFloat) HashCode() uint64 {
	return hashInexactNumeric(p.value)
}

// Add returns the sum of this BigFloat and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: inexact + inexact = inexact, exact + inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigFloat) Add(o Number) Number {
	// R7RS §6.2.2: Inexactness is contagious. For addition, 0 + x = x,
	// so the result's exactness MUST match the other operand.
	// No zero short-circuit allowed (unlike multiplication).
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
	case *BigInteger:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Integer:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Float:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Add(p.value, vf)}
	case *Complex:
		// Convert both to BigComplex to preserve BigFloat precision
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		vc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return bc.Add(vc)
	case *BigComplex:
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of this BigFloat and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: inexact - inexact = inexact, exact - inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigFloat) Subtract(o Number) Number {
	// R7RS §6.2.2: Inexactness is contagious. For subtraction, x - 0 = x,
	// so the result's exactness MUST match the minuend.
	// No zero short-circuit allowed (unlike multiplication).
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Sub(p.value, v.value)}
	case *BigInteger:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Integer:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Float:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Sub(p.value, vf)}
	case *Complex:
		// Convert both to BigComplex to preserve BigFloat precision
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		vc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return bc.Subtract(vc)
	case *BigComplex:
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		return bc.Subtract(v)
	}
	panic(ErrNotANumber)
}

// Multiply returns the product of this BigFloat and another number.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigFloat) Multiply(o Number) Number {
	if o.IsZero() {
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
	case *BigInteger:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Integer:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Float:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Mul(p.value, vf)}
	case *Complex:
		// Convert both to BigComplex to preserve BigFloat precision
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		vc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return bc.Multiply(vc)
	case *BigComplex:
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		return bc.Multiply(v)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of this BigFloat and another number.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigFloat) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *BigFloat:
		return &BigFloat{value: new(big.Float).Quo(p.value, v.value)}
	case *BigInteger:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Integer:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Float:
		vf := v.bigFloat()
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return &BigFloat{value: new(big.Float).Quo(p.value, vf)}
	case *Complex:
		// Convert both to BigComplex to preserve BigFloat precision
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		vc := NewBigComplexFromBigFloats(
			NewBigFloatFromFloat64(real(v.Value)),
			NewBigFloatFromFloat64(imag(v.Value)),
		)
		return bc.Divide(vc)
	case *BigComplex:
		bc := NewBigComplex(p, NewBigFloatFromFloat64(0))
		return bc.Divide(v)
	}
	panic(ErrNotANumber)
}

// Negate returns the negation of this BigFloat.
func (p *BigFloat) Negate() Number {
	return &BigFloat{value: new(big.Float).Neg(p.value)}
}

// IsZero returns true if this BigFloat is zero.
func (p *BigFloat) IsZero() bool {
	return p.value.Sign() == 0
}

// LessThan returns true if this BigFloat is less than another number.
func (p *BigFloat) LessThan(o Number) bool {
	return p.Compare(o) < 0
}

// IsNegative returns true if this BigFloat is negative.
func (p *BigFloat) IsNegative() bool {
	return p.value.Sign() < 0
}

// IsPositive returns true if this BigFloat is positive.
func (p *BigFloat) IsPositive() bool {
	return p.value.Sign() > 0
}

// IsExact returns false since BigFloat is always inexact.
func (p *BigFloat) IsExact() bool {
	return false // BigFloat is inexact
}

// IsInteger returns true if this BigFloat represents an integer value.
//
// R7RS §6.2.6: integer? returns #t for inexact integers.
func (p *BigFloat) IsInteger() bool {
	return p.value.IsInt()
}

// IsRational returns true since BigFloat is always finite (big.Float has no Inf/NaN).
//
// R7RS §6.2.6: rational? returns #t for all finite real numbers.
func (p *BigFloat) IsRational() bool {
	return true
}

// IsFinite returns true since big.Float has no Inf or NaN representation.
//
// R7RS §6.2.6: finite? returns #t for finite numbers.
func (p *BigFloat) IsFinite() bool {
	return true
}

// IsNaN returns false since big.Float has no NaN representation.
//
// R7RS §6.2.6: nan? returns #f for big.Float values.
func (p *BigFloat) IsNaN() bool {
	return false
}

// ToExact converts this BigFloat to an exact Rational.
func (p *BigFloat) ToExact() Number {
	// Convert to Rational for exact representation
	r, _ := p.value.Rat(nil)
	if r == nil {
		return NewRational(0, 1)
	}
	return NewRationalFromRat(r)
}

// ToInexact returns this BigFloat unchanged since it's already inexact.
func (p *BigFloat) ToInexact() Number {
	return p
}

// Abs returns the absolute value of this BigFloat.
func (p *BigFloat) Abs() Number {
	return NewBigFloat(new(big.Float).Abs(p.value))
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
func (p *BigFloat) Sign() int {
	return p.value.Sign()
}

// Compare compares this BigFloat with another number.
func (p *BigFloat) Compare(o Number) int {
	switch v := o.(type) {
	case *BigFloat:
		return p.value.Cmp(v.value)
	case *BigInteger:
		vf := v.bigFloat()
		return p.value.Cmp(vf)
	case *Integer:
		vf := v.bigFloat()
		return p.value.Cmp(vf)
	case *Float:
		vf := v.bigFloat()
		return p.value.Cmp(vf)
	case *Rational:
		vf := new(big.Float).SetRat(v.Rat())
		return p.value.Cmp(vf)
	case *Complex:
		f, _ := p.value.Float64()
		if f < real(v.Value) {
			return -1
		} else if f > real(v.Value) {
			return 1
		}
		return 0
	case *BigComplex:
		return p.value.Cmp(toBigFloat(v.Real()).BigFloatValue())
	}
	panic(ErrNotANumber)
}

// SchemeString returns the Scheme representation of this BigFloat.
func (p *BigFloat) SchemeString() string {
	return p.value.Text('g', -1)
}

// IsVoid returns true if this BigFloat is nil.
func (p *BigFloat) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this BigFloat equals another value.
func (p *BigFloat) EqualTo(o Value) bool {
	v, ok := o.(*BigFloat)
	if !ok {
		// Also check if equal to regular Float
		f, ok := o.(*Float)
		if ok {
			vf := new(big.Float).SetFloat64(f.Value)
			return p.value.Cmp(vf) == 0
		}
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	return p.value.Cmp(v.value) == 0
}
