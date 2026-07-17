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

	"github.com/aalpar/wile/pkg/werr"
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

// BigInt returns p's storage without copying. The result aliases p, and a
// BigInteger is reachable from every Scheme binding it flowed to, so mutating
// the result (Set, Neg, an Add writing into it) retroactively changes the value
// of an exact integer Scheme treats as immutable — at every one of those
// bindings, not just the one the caller has in hand.
//
// The accessor does not copy because it sits on the numeric hot path and nearly
// every caller only reads (Cmp, Sign, Text, IsInt64, SetInt); a defensive copy
// here would allocate for all of them to protect against the rare mutator. A
// caller that intends to mutate owns the copy: new(big.Int).Set(v.BigInt()).
//
// The in-place scratch operations in numeric_scratch.go are the mutators this
// contract is aimed at. They are safe only on a big.Int the caller allocated;
// none may be pointed at a *BigInteger's storage obtained here.
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

// Kind returns the numeric kind for dispatch table indexing.
func (p *BigInteger) Kind() NumericKind {
	return KindBigInteger
}

// bigIntegerSimplifyDown demotes a BigInteger to Integer when its value fits
// in int64; otherwise returns it unchanged.
func bigIntegerSimplifyDown(n Number) Number {
	v := n.(*BigInteger)
	if v.value.IsInt64() {
		return NewInteger(v.value.Int64())
	}
	return n
}

// bigIntegerToFloat64WithAccuracy converts a BigInteger to float64 using
// native big.Float to get Go's stdlib accuracy signal directly.
func bigIntegerToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	p := n.(*BigInteger)
	f, acc := new(big.Float).SetInt(p.value).Float64()
	return f, acc, true
}

var bigIntegerAdd [numKinds]func(*BigInteger, Number) Number
var bigIntegerSubtract [numKinds]func(*BigInteger, Number) Number
var bigIntegerLessThan [numKinds]func(*BigInteger, Number) bool
var bigIntegerMultiply [numKinds]func(*BigInteger, Number) Number
var bigIntegerDivide [numKinds]func(*BigInteger, Number) (Number, error)

func init() {
	bigIntegerAdd = makeAddDispatch(KindBigInteger, func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, o.(*BigInteger).value)}
	})

	bigIntegerSubtract = makeSubtractDispatch(KindBigInteger, func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Sub, p.value, o.(*BigInteger).value)}
	})

	bigIntegerLessThan = makeLessThanDispatch(KindBigInteger, func(p *BigInteger, o Number) bool {
		return p.value.Cmp(o.(*BigInteger).value) < 0
	})

	bigIntegerMultiply = makeMultiplyDispatch(KindBigInteger, func(p *BigInteger, o Number) Number {
		return &BigInteger{value: newBigIntFromOp((*big.Int).Mul, p.value, o.(*BigInteger).value)}
	})

	bigIntegerDivide = makeDivideDispatch(KindBigInteger, func(p *BigInteger, o Number) (Number, error) {
		v := o.(*BigInteger)
		quo, rem := new(big.Int).QuoRem(p.value, v.value, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}, nil
		}
		return NewRationalFromBigInt(p.value, v.value), nil
	})

	registerNumericSpec(KindBigInteger, NumericTypeSpec{
		schemeName:            "integer",
		simplifyDown:          bigIntegerSimplifyDown,
		toFloat64WithAccuracy: bigIntegerToFloat64WithAccuracy,
		isAlwaysExact:         true,
		isAlwaysReal:          true,
	})
}

// Add returns the sum of this BigInteger and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: exact + exact = exact (BigInteger),
// exact + inexact = inexact (Float/Complex).
func (p *BigInteger) Add(o Number) Number {
	if isExactZero(o) {
		return p
	}
	if isExactZero(p) {
		return o
	}
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
	if isExactZero(o) {
		return p
	}
	if isExactZero(p) {
		return o.Negate()
	}
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
	if exactZeroEither(p, o) {
		return NewInteger(0)
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
func (p *BigInteger) Divide(o Number) (Number, error) {
	// The exact-zero rule for division; exactZeroTable[zeroDiv] in exact_zero.go.
	switch exactZeroDivideAction(p, o) {
	case zeroRaise:
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "BigInteger.Divide: division by exact zero")
	case zeroYieldExactZero:
		return NewInteger(0), nil
	}
	v, ok := o.(*BigInteger)
	if ok {
		quo, rem := new(big.Int).QuoRem(p.value, v.value, new(big.Int))
		if rem.Sign() == 0 {
			return &BigInteger{value: quo}, nil
		}
		return NewRationalFromBigInt(p.value, v.value), nil
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

// SignBit reports whether this big integer carries a negative sign bit.
//
// BigInteger is exact, so it has no signed zero and this coincides with IsNegative.
func (p *BigInteger) SignBit() bool {
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
func (p *BigInteger) ToExact() (Number, error) {
	return p, nil
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

// SchemeString returns the Scheme representation of this BigInteger.
func (p *BigInteger) SchemeString() string {
	return p.value.String()
}

// IsVoid returns true if this BigInteger is nil.
func (p *BigInteger) IsVoid() bool {
	return p == nil
}

// EqualTo implements R7RS equal? for BigInteger.
//
// R7RS §6.1: equal? "returns the same as eqv? when applied to … numbers" — no
// latitude. So this delegates to EqvNumber (eqv.go), the single authority on
// numeric equivalence, rather than restating the rules. Restating them is what
// let equal? and eqv? drift apart on signed zero and on cross-representation
// inexacts.
func (p *BigInteger) EqualTo(v Value) bool {
	return eqvNumberValue(p, v)
}
