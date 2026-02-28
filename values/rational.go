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
	_ Value      = (*Rational)(nil)
	_ Number     = (*Rational)(nil)
	_ RealNumber = (*Rational)(nil)
	_ Hashable   = (*Rational)(nil)
)

// Rational represents a Scheme rational number (exact fraction).
type Rational struct {
	value *big.Rat
}

// HashCode returns a hash of the rational value.
// Uses the canonical exact-family hash so that Integer, BigInteger,
// and Rational produce identical hashes for equal values.
func (p *Rational) HashCode() uint64 {
	return hashExactNumeric(p.value)
}

// NewRational creates a new Rational from numerator and denominator.
// The fraction is automatically normalized (reduced to lowest terms).
func NewRational(num, denom int64) *Rational {
	return &Rational{
		value: big.NewRat(num, denom),
	}
}

// NewRationalFromBigInt creates a new Rational from big.Int numerator and denominator.
func NewRationalFromBigInt(num, denom *big.Int) *Rational {
	r := new(big.Rat)
	r.SetFrac(num, denom)
	return &Rational{value: r}
}

// NewRationalFromRat creates a Rational from an existing big.Rat.
func NewRationalFromRat(r *big.Rat) *Rational {
	return &Rational{value: new(big.Rat).Set(r)}
}

// Rat returns the underlying big.Rat value.
func (p *Rational) Rat() *big.Rat {
	return p.value
}

// Num returns the numerator as a big.Int.
func (p *Rational) Num() *big.Int {
	return p.value.Num()
}

// Denom returns the denominator as a big.Int.
func (p *Rational) Denom() *big.Int {
	return p.value.Denom()
}

// NumInt64 returns the numerator as int64 (may overflow for large values).
func (p *Rational) NumInt64() int64 {
	return p.value.Num().Int64()
}

// DenomInt64 returns the denominator as int64 (may overflow for large values).
func (p *Rational) DenomInt64() int64 {
	return p.value.Denom().Int64()
}

// Float64 returns the rational as a float64 approximation.
func (p *Rational) Float64() float64 {
	f, _ := p.value.Float64()
	return f
}

// IsInteger returns true if the rational represents an integer (denominator is 1).
func (p *Rational) IsInteger() bool {
	return p.value.IsInt()
}

// Add returns the sum of two numbers.
//
// Kind returns the numeric kind for dispatch table indexing.
func (p *Rational) Kind() NumericKind {
	return KindRational
}

var rationalAdd [numKinds]func(*Rational, Number) Number
var rationalSubtract [numKinds]func(*Rational, Number) Number
var rationalLessThan [numKinds]func(*Rational, Number) bool
var rationalCompare [numKinds]func(*Rational, Number) int
var rationalMultiply [numKinds]func(*Rational, Number) Number
var rationalDivide [numKinds]func(*Rational, Number) Number

func init() {
	rationalAdd = makeAddDispatch(KindRational, func(p *Rational, o Number) Number {
		result := new(big.Rat).Add(p.value, o.(*Rational).value)
		return &Rational{value: result}
	})

	rationalSubtract = makeSubtractDispatch(KindRational, func(p *Rational, o Number) Number {
		result := new(big.Rat).Sub(p.value, o.(*Rational).value)
		return &Rational{value: result}
	})

	rationalLessThan = makeLessThanDispatch(KindRational, func(p *Rational, o Number) bool {
		return p.value.Cmp(o.(*Rational).value) < 0
	})

	rationalCompare = makeCompareDispatch(KindRational, func(p *Rational, o Number) int {
		return p.value.Cmp(o.(*Rational).value)
	})

	rationalMultiply = makeMultiplyDispatch(KindRational, func(p *Rational, o Number) Number {
		result := new(big.Rat).Mul(p.value, o.(*Rational).value)
		return &Rational{value: result}
	})

	rationalDivide = makeDivideDispatch(KindRational, func(p *Rational, o Number) Number {
		result := new(big.Rat).Quo(p.value, o.(*Rational).value)
		return &Rational{value: result}
	})
}

// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
// Inexactness is contagious per R7RS §6.2.2.
func (p *Rational) Add(o Number) Number {
	v, ok := o.(*Rational)
	if ok {
		return &Rational{value: new(big.Rat).Add(p.value, v.value)}
	}
	return rationalAdd[o.Kind()](p, o)
}

// Subtract returns the difference of two numbers.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *Rational) Subtract(o Number) Number {
	v, ok := o.(*Rational)
	if ok {
		return &Rational{value: new(big.Rat).Sub(p.value, v.value)}
	}
	return rationalSubtract[o.Kind()](p, o)
}

// Multiply returns the product of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Multiply(o Number) Number {
	if o.IsZero() {
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	v, ok := o.(*Rational)
	if ok {
		result := new(big.Rat).Mul(p.value, v.value)
		return &Rational{value: result}
	}
	return rationalMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of two numbers.
func (p *Rational) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	v, ok := o.(*Rational)
	if ok {
		result := new(big.Rat).Quo(p.value, v.value)
		return &Rational{value: result}
	}
	return rationalDivide[o.Kind()](p, o)
}

// IsZero returns true if the rational equals zero.
func (p *Rational) IsZero() bool {
	return p.value.Sign() == 0
}

// LessThan returns true if this rational is less than another number.
func (p *Rational) LessThan(o Number) bool {
	v, ok := o.(*Rational)
	if ok {
		return p.value.Cmp(v.value) < 0
	}
	return rationalLessThan[o.Kind()](p, o)
}

// Negate returns the negation of this rational.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Rational) Negate() Number {
	return &Rational{value: new(big.Rat).Neg(p.value)}
}

// Abs returns the absolute value of this rational.
func (p *Rational) Abs() Number {
	return NewRationalFromRat(new(big.Rat).Abs(p.value))
}

// ToExact returns this Rational unchanged since it is already exact.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
func (p *Rational) ToExact() Number {
	return p
}

// ToInexact converts this Rational to an inexact BigFloat.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
// L18: Use big.Float.SetRat to preserve precision for large rationals.
func (p *Rational) ToInexact() Number {
	f := new(big.Float).SetRat(p.value)
	return NewBigFloat(f)
}

// IsPositive returns true if this rational is positive.
func (p *Rational) IsPositive() bool {
	return p.value.Sign() > 0
}

// IsNegative returns true if this rational is negative.
func (p *Rational) IsNegative() bool {
	return p.value.Sign() < 0
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
func (p *Rational) Sign() int {
	return p.value.Sign()
}

// Compare compares this rational with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Rational) Compare(o Number) int {
	v, ok := o.(*Rational)
	if ok {
		return p.value.Cmp(v.value)
	}
	return rationalCompare[o.Kind()](p, o)
}

// IsExact returns true since Rational is always exact.
//
// R7RS §6.2.2: Rationals are always exact numbers.
func (p *Rational) IsExact() bool {
	return true
}

// IsRational returns true since Rational is always a rational number.
//
// R7RS §6.2.6: rational? returns #t for exact rationals.
func (p *Rational) IsRational() bool {
	return true
}

// IsFinite returns true since exact rationals are always finite.
//
// R7RS §6.2.6: finite? returns #t for all exact numbers.
func (p *Rational) IsFinite() bool {
	return true
}

// IsNaN returns false since exact rationals are never NaN.
//
// R7RS §6.2.6: nan? returns #f for exact numbers.
func (p *Rational) IsNaN() bool {
	return false
}

// IsVoid returns true if the rational is nil.
func (p *Rational) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the rationals have equal values.
// Handles comparison with Integer and BigInteger for symmetry with
// whole-valued rationals (e.g., 5/1 == 5).
func (p *Rational) EqualTo(v Value) bool {
	switch other := v.(type) {
	case *Rational:
		return p.value.Cmp(other.value) == 0
	case *Integer:
		return p.value.Cmp(new(big.Rat).SetInt64(other.Value)) == 0
	case *BigInteger:
		return p.value.Cmp(new(big.Rat).SetInt(other.BigInt())) == 0
	}
	return false
}

// SchemeString returns the Scheme representation of the rational.
func (p *Rational) SchemeString() string {
	return p.value.RatString()
}
