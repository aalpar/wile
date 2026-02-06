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
	_ Value    = (*Rational)(nil)
	_ Number   = (*Rational)(nil)
	_ Hashable = (*Rational)(nil)
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

// IsInteger returns true if the rational represents an integer (denominator is 1).
func (p *Rational) IsInteger() bool {
	return p.value.IsInt()
}

// Add returns the sum of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Add(o Number) Number {
	if o.IsZero() {
		return p
	}
	if p.IsZero() {
		return o
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Add(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Add(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := new(big.Rat).SetInt(v.value)
		result := new(big.Rat).Add(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() + v.Value)
	case *BigFloat:
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
		return &BigFloat{value: new(big.Float).Add(self, v.value)}
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) + v.Value)
	case *BigComplex:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
		bc := NewBigComplex(&BigFloat{value: bf}, NewBigFloatFromFloat64(0))
		return bc.Add(v)
	}
	panic(ErrNotANumber)
}

// Subtract returns the difference of two numbers.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *Rational) Subtract(o Number) Number {
	if o.IsZero() {
		return p
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Sub(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Sub(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := new(big.Rat).SetInt(v.value)
		result := new(big.Rat).Sub(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() - v.Value)
	case *BigFloat:
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
		return &BigFloat{value: new(big.Float).Sub(self, v.value)}
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) - v.Value)
	case *BigComplex:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
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
		return o
	}
	switch v := o.(type) {
	case *Rational:
		result := new(big.Rat).Mul(p.value, v.value)
		return &Rational{value: result}
	case *Integer:
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Mul(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := new(big.Rat).SetInt(v.value)
		result := new(big.Rat).Mul(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() * v.Value)
	case *BigFloat:
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
		return &BigFloat{value: new(big.Float).Mul(self, v.value)}
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) * v.Value)
	case *BigComplex:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
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
		other := big.NewRat(v.Value, 1)
		result := new(big.Rat).Quo(p.value, other)
		return &Rational{value: result}
	case *BigInteger:
		other := new(big.Rat).SetInt(v.value)
		result := new(big.Rat).Quo(p.value, other)
		return &Rational{value: result}
	case *Float:
		return NewFloat(p.Float64() / v.Value)
	case *BigFloat:
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
		return &BigFloat{value: new(big.Float).Quo(self, v.value)}
	case *Complex:
		return NewComplex(complex(p.Float64(), 0) / v.Value)
	case *BigComplex:
		bf := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
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
		other := big.NewRat(v.Value, 1)
		return p.value.Cmp(other) < 0
	case *BigInteger:
		other := new(big.Rat).SetInt(v.BigInt())
		return p.value.Cmp(other) < 0
	case *Float:
		return p.Float64() < v.Value
	case *BigFloat:
		self := new(big.Float).SetRat(p.value)
		return self.Cmp(v.BigFloatValue()) < 0
	case *Complex:
		return p.Float64() < real(v.Value)
	case *BigComplex:
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
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

// Compare compares this rational with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Rational) Compare(o Number) int {
	switch v := o.(type) {
	case *Rational:
		return p.value.Cmp(v.value)
	case *Integer:
		other := big.NewRat(v.Value, 1)
		return p.value.Cmp(other)
	case *BigInteger:
		other := new(big.Rat).SetInt(v.BigInt())
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
		self := new(big.Float).SetPrec(DefaultBigFloatPrecision).SetRat(p.value)
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

// IsVoid returns true if the rational is nil.
func (p *Rational) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the rationals have equal values.
func (p *Rational) EqualTo(v Value) bool {
	if other, ok := v.(*Rational); ok {
		return p.value.Cmp(other.value) == 0
	}
	return false
}

// SchemeString returns the Scheme representation of the rational.
func (p *Rational) SchemeString() string {
	return p.value.RatString()
}
