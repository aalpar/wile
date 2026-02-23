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
	_ Value      = (*BigInteger)(nil)
	_ Number     = (*BigInteger)(nil)
	_ RealNumber = (*BigInteger)(nil)
	_ Hashable   = (*BigInteger)(nil)
)

// BigInteger represents an arbitrary-precision integer.
// Created with the #z prefix in Scheme (e.g., #z12345678901234567890).
//
// R7RS §6.2.1: Integers are exact numbers in the numeric tower hierarchy:
//
//	number ⊃ complex ⊃ real ⊃ rational ⊃ integer
//
// R7RS §6.2.2: BigInteger is always exact. Operations on exact numbers
// produce exact results when mathematically well-defined.
//
// R7RS §6.2.3: Implementations may support arbitrarily large exact integers.
// BigInteger provides this capability using Go's math/big.Int.
//
// # Precision Preservation
//
// BigInteger operations with Float preserve precision by promoting to BigFloat
// for comparison and arithmetic. This avoids precision loss from converting
// large BigIntegers to float64 (which has only 53 bits of mantissa precision).
//
// Prior to M5 fix, BigIntegers with >53 significant bits would be truncated
// when compared with Float, causing incorrect comparison results. For example,
// comparing 2^53+1 with 2^53.0 would incorrectly report equality after both
// were converted to the same float64 value.
//
// The fix promotes both operands to BigFloat (arbitrary precision), ensuring
// correct comparisons and arithmetic while preserving R7RS exactness contagion
// (exact + inexact → inexact).
type BigInteger struct {
	value *big.Int
}

// HashCode returns a hash of the big integer value.
// Uses the canonical exact-family hash so that Integer, BigInteger,
// and Rational produce identical hashes for equal values.
func (p *BigInteger) HashCode() uint64 {
	return hashExactNumeric(new(big.Rat).SetInt(p.value))
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

// Per-type conversion helpers for BigInteger.
// These eliminate repeated conversion expressions in the type-switch
// dispatch methods below. Each produces the representation needed by
// the target type's native arithmetic.

func (p *BigInteger) float64Val() float64 {
	return float64FromBigInt(p.value)
}

func (p *BigInteger) bigRat() *big.Rat {
	return new(big.Rat).SetInt(p.value)
}

func (p *BigInteger) bigFloat() *big.Float {
	return new(big.Float).SetInt(p.value)
}
func (p *BigInteger) toBigComplex() *BigComplex {
	return NewBigComplex(p, NewBigIntegerFromInt64(0))
}

// Add returns the sum of this BigInteger and another number.
//
// Kind returns the numeric kind for dispatch table indexing.
func (p *BigInteger) Kind() NumericKind {
	return KindBigInteger
}

var bigIntegerAdd [numKinds]func(*BigInteger, Number) Number
var bigIntegerSubtract [numKinds]func(*BigInteger, Number) Number
var bigIntegerLessThan [numKinds]func(*BigInteger, Number) bool
var bigIntegerCompare [numKinds]func(*BigInteger, Number) int
var bigIntegerMultiply [numKinds]func(*BigInteger, Number) Number
var bigIntegerDivide [numKinds]func(*BigInteger, Number) Number

func init() {
	bigIntegerAdd[KindInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, o.(*Integer).bigInt())}
	}
	bigIntegerAdd[KindBigInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, o.(*BigInteger).value)}
	}
	bigIntegerAdd[KindFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		other := o.(*Float).bigFloat()
		result := new(big.Float).Add(self, other)
		return NewBigFloat(result)
	}
	bigIntegerAdd[KindBigFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Add(self, o.(*BigFloat).value)}
	}
	bigIntegerAdd[KindRational] = func(p *BigInteger, o Number) Number {
		pRat := p.bigRat()
		return NewRationalFromRat(new(big.Rat).Add(pRat, o.(*Rational).Rat()))
	}
	bigIntegerAdd[KindComplex] = func(p *BigInteger, o Number) Number {
		f := p.float64Val()
		return NewComplex(complex(f, 0) + o.(*Complex).Datum())
	}
	bigIntegerAdd[KindBigComplex] = func(p *BigInteger, o Number) Number {
		bc := p.toBigComplex()
		return bc.Add(o)
	}

	bigIntegerSubtract[KindInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, o.(*Integer).bigInt())}
	}
	bigIntegerSubtract[KindBigInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, o.(*BigInteger).value)}
	}
	bigIntegerSubtract[KindFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		other := o.(*Float).bigFloat()
		result := new(big.Float).Sub(self, other)
		return NewBigFloat(result)
	}
	bigIntegerSubtract[KindBigFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Sub(self, o.(*BigFloat).value)}
	}
	bigIntegerSubtract[KindRational] = func(p *BigInteger, o Number) Number {
		pRat := p.bigRat()
		return NewRationalFromRat(new(big.Rat).Sub(pRat, o.(*Rational).Rat()))
	}
	bigIntegerSubtract[KindComplex] = func(p *BigInteger, o Number) Number {
		f := p.float64Val()
		return NewComplex(complex(f, 0) - o.(*Complex).Datum())
	}
	bigIntegerSubtract[KindBigComplex] = func(p *BigInteger, o Number) Number {
		bc := p.toBigComplex()
		return bc.Subtract(o)
	}

	bigIntegerLessThan[KindInteger] = func(p *BigInteger, o Number) bool {
		return p.value.Cmp(o.(*Integer).bigInt()) < 0
	}
	bigIntegerLessThan[KindBigInteger] = func(p *BigInteger, o Number) bool {
		return p.value.Cmp(o.(*BigInteger).value) < 0
	}
	bigIntegerLessThan[KindFloat] = func(p *BigInteger, o Number) bool {
		return p.bigFloat().Cmp(o.(*Float).bigFloat()) < 0
	}
	bigIntegerLessThan[KindBigFloat] = func(p *BigInteger, o Number) bool {
		return p.bigFloat().Cmp(o.(*BigFloat).BigFloatValue()) < 0
	}
	bigIntegerLessThan[KindRational] = func(p *BigInteger, o Number) bool {
		return p.bigRat().Cmp(o.(*Rational).Rat()) < 0
	}
	bigIntegerLessThan[KindComplex] = func(p *BigInteger, o Number) bool {
		self := p.bigFloat()
		realPart := NewFloat(real(o.(*Complex).Value)).bigFloat()
		return self.Cmp(realPart) < 0
	}
	bigIntegerLessThan[KindBigComplex] = func(p *BigInteger, o Number) bool {
		return p.bigFloat().Cmp(toBigFloat(o.(*BigComplex).Real()).BigFloatValue()) < 0
	}

	bigIntegerCompare[KindInteger] = func(p *BigInteger, o Number) int {
		return p.value.Cmp(o.(*Integer).bigInt())
	}
	bigIntegerCompare[KindBigInteger] = func(p *BigInteger, o Number) int {
		return p.value.Cmp(o.(*BigInteger).value)
	}
	bigIntegerCompare[KindFloat] = func(p *BigInteger, o Number) int {
		// Convert both to BigFloat to preserve precision.
		// Don't convert BigInteger to float64 (loses precision for >53-bit integers).
		self := p.bigFloat()
		other := o.(*Float).bigFloat()
		return self.Cmp(other)
	}
	bigIntegerCompare[KindBigFloat] = func(p *BigInteger, o Number) int {
		return p.bigFloat().Cmp(o.(*BigFloat).BigFloatValue())
	}
	bigIntegerCompare[KindRational] = func(p *BigInteger, o Number) int {
		return p.bigRat().Cmp(o.(*Rational).Rat())
	}
	bigIntegerCompare[KindComplex] = func(p *BigInteger, o Number) int {
		// Compare real parts at BigFloat precision.
		// Don't convert BigInteger to float64 (loses precision for >53-bit integers).
		self := p.bigFloat()
		realPart := NewFloat(real(o.(*Complex).Value)).bigFloat()
		return self.Cmp(realPart)
	}
	bigIntegerCompare[KindBigComplex] = func(p *BigInteger, o Number) int {
		return p.bigFloat().Cmp(toBigFloat(o.(*BigComplex).Real()).BigFloatValue())
	}

	bigIntegerMultiply[KindInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, o.(*Integer).bigInt())}
	}
	bigIntegerMultiply[KindBigInteger] = func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, o.(*BigInteger).value)}
	}
	bigIntegerMultiply[KindFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		other := o.(*Float).bigFloat()
		result := new(big.Float).Mul(self, other)
		return NewBigFloat(result)
	}
	bigIntegerMultiply[KindBigFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Mul(self, o.(*BigFloat).value)}
	}
	bigIntegerMultiply[KindRational] = func(p *BigInteger, o Number) Number {
		pRat := p.bigRat()
		return NewRationalFromRat(new(big.Rat).Mul(pRat, o.(*Rational).Rat()))
	}
	bigIntegerMultiply[KindComplex] = func(p *BigInteger, o Number) Number {
		f := p.float64Val()
		return NewComplex(complex(f, 0) * o.(*Complex).Datum())
	}
	bigIntegerMultiply[KindBigComplex] = func(p *BigInteger, o Number) Number {
		bc := p.toBigComplex()
		return bc.Multiply(o)
	}

	bigIntegerDivide[KindInteger] = func(p *BigInteger, o Number) Number {
		v := o.(*Integer)
		divisor := v.bigInt()
		quo, rem := new(big.Int).QuoRem(p.value, divisor, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}
		}
		return NewRationalFromBigInt(p.value, divisor)
	}
	bigIntegerDivide[KindBigInteger] = func(p *BigInteger, o Number) Number {
		v := o.(*BigInteger)
		quo, rem := new(big.Int).QuoRem(p.value, v.value, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}
		}
		return NewRationalFromBigInt(p.value, v.value)
	}
	bigIntegerDivide[KindFloat] = func(p *BigInteger, o Number) Number {
		v := o.(*Float)
		self := p.bigFloat()
		other := v.bigFloat()
		result := new(big.Float).Quo(self, other)
		return NewBigFloat(result)
	}
	bigIntegerDivide[KindBigFloat] = func(p *BigInteger, o Number) Number {
		self := p.bigFloat()
		return &BigFloat{value: new(big.Float).Quo(self, o.(*BigFloat).value)}
	}
	bigIntegerDivide[KindRational] = func(p *BigInteger, o Number) Number {
		pRat := p.bigRat()
		return NewRationalFromRat(new(big.Rat).Quo(pRat, o.(*Rational).Rat()))
	}
	bigIntegerDivide[KindComplex] = func(p *BigInteger, o Number) Number {
		f := p.float64Val()
		return NewComplex(complex(f, 0) / o.(*Complex).Datum())
	}
	bigIntegerDivide[KindBigComplex] = func(p *BigInteger, o Number) Number {
		bc := p.toBigComplex()
		return bc.Divide(o)
	}
}

// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact (BigInteger),
// exact + inexact = inexact (Float/Complex).
// Inexactness is contagious per R7RS §6.2.2.
func (p *BigInteger) Add(o Number) Number {
	v, ok := o.(*BigInteger)
	if ok {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, v.value)}
	}
	return bigIntegerAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this BigInteger and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *BigInteger) Subtract(o Number) Number {
	v, ok := o.(*BigInteger)
	if ok {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, v.value)}
	}
	return bigIntegerSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this BigInteger and another number.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2 Exactness: exact * exact = exact, exact * inexact = inexact.
// Exception: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigInteger) Multiply(o Number) Number {
	if o.IsZero() {
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	v, ok := o.(*BigInteger)
	if ok {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, v.value)}
	}
	return bigIntegerMultiply[o.Kind()](p, o)
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
		panic(ErrDivisionByZero)
	}
	v, ok := o.(*BigInteger)
	if ok {
		quo, rem := new(big.Int).QuoRem(p.value, v.value, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}
		}
		return NewRationalFromBigInt(p.value, v.value)
	}
	return bigIntegerDivide[o.Kind()](p, o)
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
	v, ok := o.(*BigInteger)
	if ok {
		return p.value.Cmp(v.value) < 0
	}
	return bigIntegerLessThan[o.Kind()](p, o)
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

// IsInteger returns true since BigInteger is always an integer.
//
// R7RS §6.2.6: integer? returns #t for exact integers.
func (p *BigInteger) IsInteger() bool {
	return true
}

// IsRational returns true since integers are a subset of rationals.
//
// R7RS §6.2.6: rational? returns #t for all real finite numbers.
func (p *BigInteger) IsRational() bool {
	return true
}

// IsFinite returns true since integers are always finite.
//
// R7RS §6.2.6: finite? returns #t for all exact numbers.
func (p *BigInteger) IsFinite() bool {
	return true
}

// IsNaN returns false since integers are never NaN.
//
// R7RS §6.2.6: nan? returns #f for exact numbers.
func (p *BigInteger) IsNaN() bool {
	return false
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
//
// PRECISION NOTE: For BigIntegers with more than 53 significant bits,
// precision is lost when converting to float64 (IEEE 754 binary64 has
// only 53 bits of mantissa precision). This is compliant with R7RS
// which allows inexact to be approximate.
func (p *BigInteger) ToInexact() Number {
	f := p.float64Val()
	return NewFloat(f)
}

// Abs returns the absolute value of this BigInteger.
func (p *BigInteger) Abs() Number {
	return NewBigInteger(new(big.Int).Abs(p.value))
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
func (p *BigInteger) Sign() int {
	return p.value.Sign()
}

// Compare compares this BigInteger with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of
// exactness. Returns -1, 0, or 1 for less than, equal, or greater than.
func (p *BigInteger) Compare(o Number) int {
	v, ok := o.(*BigInteger)
	if ok {
		return p.value.Cmp(v.value)
	}
	return bigIntegerCompare[o.Kind()](p, o)
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
	switch other := o.(type) {
	case *BigInteger:
		if other == nil || p == nil {
			return p == other
		}
		return p.value.Cmp(other.value) == 0
	case *Integer:
		return p.value.Cmp(other.bigInt()) == 0
	case *Rational:
		return other.value.Cmp(new(big.Rat).SetInt(p.BigInt())) == 0
	}
	return false
}
