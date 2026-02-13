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
// Uses the canonical string representation of the reduced fraction.
func (p *Rational) HashCode() uint64 {
	return hashString(0x6, p.value.RatString())
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

// Per-type conversion helpers for Rational.
// These eliminate repeated conversion expressions in the type-switch
// dispatch methods below. Each produces the representation needed by
// the target type's native arithmetic.

func (p *Rational) bigFloat() *big.Float {
	return new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
}
func (p *Rational) toComplex() complex128 {
	return complex(p.Float64(), 0)
}

// IsInteger returns true if the rational represents an integer (denominator is 1).
func (p *Rational) IsInteger() bool {
	return p.value.IsInt()
}

// Add returns the sum of two numbers.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Add(o Number) Number {
	// R7RS §6.2.2: Inexactness is contagious. For addition, 0 + x = x,
	// so the result's exactness MUST match the other operand.
	// No zero short-circuit allowed (unlike multiplication).
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Add(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := v.bigRat()
		result := new(big.Rat).Add(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := v.bigRat()
		result := new(big.Rat).Add(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() + v.Value)
	case *BigFloat:
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Add(self, v.value)}
	case *Complex:
		return NewComplex(p.toComplex() + v.Value)
	case *BigComplex:
		bf := p.bigFloat()
		bc := NewBigComplex(&BigFloat{value: bf}, NewBigFloatFromFloat64(0))
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of two numbers.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Subtract(o Number) Number {
	// R7RS §6.2.2: Inexactness is contagious. For subtraction, x - 0 = x,
	// so the result's exactness MUST match the minuend.
	// No zero short-circuit allowed (unlike multiplication).
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Sub(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := v.bigRat()
		result := new(big.Rat).Sub(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := v.bigRat()
		result := new(big.Rat).Sub(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() - v.Value)
	case *BigFloat:
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Sub(self, v.value)}
	case *Complex:
		return NewComplex(p.toComplex() - v.Value)
	case *BigComplex:
		bf := p.bigFloat()
		bc := NewBigComplex(&BigFloat{value: bf}, NewBigFloatFromFloat64(0))
		return bc.Subtract(v)
	}
	panic(ErrNotANumber)
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
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Mul(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := v.bigRat()
		result := new(big.Rat).Mul(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := v.bigRat()
		result := new(big.Rat).Mul(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() * v.Value)
	case *BigFloat:
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Mul(self, v.value)}
	case *Complex:
		return NewComplex(p.toComplex() * v.Value)
	case *BigComplex:
		bf := p.bigFloat()
		bc := NewBigComplex(&BigFloat{value: bf}, NewBigFloatFromFloat64(0))
		return bc.Multiply(v)
	}
	panic(ErrNotANumber)
}

// Divide returns the quotient of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Divide(o Number) Number {
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Quo(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := v.bigRat()
		result := new(big.Rat).Quo(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := v.bigRat()
		result := new(big.Rat).Quo(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() / v.Value)
	case *BigFloat:
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Quo(self, v.value)}
	case *Complex:
		return NewComplex(p.toComplex() / v.Value)
	case *BigComplex:
		bf := p.bigFloat()
		bc := NewBigComplex(&BigFloat{value: bf}, NewBigFloatFromFloat64(0))
		return bc.Divide(v)
	}
	panic(ErrNotANumber)
}

// IsZero returns true if the rational equals zero.
func (p *Rational) IsZero() bool {
	return p.value.Sign() == 0
}

// LessThan returns true if this rational is less than another number.
func (p *Rational) LessThan(o Number) bool {
	switch v := o.(type) {
	case *Rational:
		return p.value.Cmp(v.value) < 0
	case *Integer:
		other := v.bigRat()
		return p.value.Cmp(other) < 0
	case *BigInteger:
		other := v.bigRat()
		return p.value.Cmp(other) < 0
	case *Float:
		return p.Float64() < v.Value
	case *BigFloat:
		self := new(big.Float).SetRat(p.value)
		return self.Cmp(v.BigFloatValue()) < 0
	case *Complex:
		return p.Float64() < real(v.Value)
	case *BigComplex:
		self := p.bigFloat()
		return self.Cmp(toBigFloat(v.Real()).BigFloatValue()) < 0
	}
	panic(ErrNotANumber)
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

// ToInexact converts this Rational to an inexact Float.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
func (p *Rational) ToInexact() Number {
	return NewFloat(p.Float64())
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
	switch v := o.(type) {
	case *Rational:
		return p.value.Cmp(v.value)
	case *Integer:
		other := v.bigRat()
		return p.value.Cmp(other)
	case *BigInteger:
		other := v.bigRat()
		return p.value.Cmp(other)
	case *Float:
		pf := p.Float64()
		if pf < v.Value {
			return -1
		} else if pf > v.Value {
			return 1
		}
		return 0
	case *BigFloat:
		self := new(big.Float).SetRat(p.value)
		return self.Cmp(v.BigFloatValue())
	case *Complex:
		pf := p.Float64()
		r := real(v.Value)
		if pf < r {
			return -1
		} else if pf > r {
			return 1
		}
		return 0
	case *BigComplex:
		self := p.bigFloat()
		return self.Cmp(toBigFloat(v.Real()).BigFloatValue())
	}
	panic(ErrNotANumber)
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
