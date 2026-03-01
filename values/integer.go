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
	"math"
	"math/big"
	"strconv"
)

var (
	_ Value      = (*Integer)(nil)
	_ Number     = (*Integer)(nil)
	_ RealNumber = (*Integer)(nil)
	_ Hashable   = (*Integer)(nil)
)

// Small integer cache: same flyweight technique as character caching. The
// range [-32768, 32767] covers most loop counters and small constants.
// See BIBLIOGRAPHY.md "Flyweight Pattern / Value Caching".
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
//
// R7RS §6.2.1: Integers are exact numbers in the numeric tower hierarchy:
//
//	number ⊃ complex ⊃ real ⊃ rational ⊃ integer
//
// R7RS §6.2.2: Integer is always exact. Operations on exact numbers
// produce exact results when mathematically well-defined.
type Integer struct {
	Value int64
}

// NewInteger returns an Integer value. Small integers in the range
// -32768 to 32767 are cached and return the same pointer for the same value.
func NewInteger(v int64) *Integer {
	if v >= intCacheMin && v <= intCacheMax {
		return intCache[v-intCacheMin]
	}
	return &Integer{Value: v}
}

// HashCode returns a hash of the integer value.
// Uses the canonical exact-family hash so that Integer, BigInteger,
// and Rational produce identical hashes for equal values.
func (p *Integer) HashCode() uint64 {
	return hashExactNumeric(new(big.Rat).SetInt64(p.Value))
}

// Datum returns the underlying int64 value.
func (p *Integer) Datum() int64 {
	return p.Value
}

// Per-type conversion helpers for Integer.
// These eliminate repeated conversion expressions in the type-switch
// dispatch methods below. Each produces the representation needed by
// the target type's native arithmetic.

func (p *Integer) bigInt() *big.Int {
	return big.NewInt(p.Value)
}

// Overflow-detecting arithmetic helpers for int64.
//
// R7RS §6.2.3 allows implementations to support arbitrarily large exact
// integers. These helpers ensure that int64 arithmetic silently promotes
// to BigInteger instead of wrapping on overflow. Each uses a standard
// overflow-detection idiom for its operation, falling back to math/big
// when the result would exceed int64 range. The existing Simplify()
// function handles demotion back to Integer when BigInteger results
// fit in int64.
//
// Overflow detection techniques (Warren, Hacker's Delight §2-12, §2-13):
//   - Addition: XOR sign-bit test — same-sign operands overflow when
//     the result sign differs from the operands.
//   - Subtraction: different-sign operands overflow when the result
//     sign differs from the first operand.
//   - Multiplication: after computing prod = a * b, verify prod/a == b.
//   - Negation: only math.MinInt64 overflows (its absolute value is
//     2^63, which exceeds math.MaxInt64 by 1).
//
// See BIBLIOGRAPHY.md "Hacker's Delight Overflow Detection".

// addInt64 adds two int64 values, promoting to BigInteger on overflow.
func addInt64(a, b int64) Number {
	sum := a + b
	if (a^b) >= 0 && (a^sum) < 0 {
		result := new(big.Int).Add(big.NewInt(a), big.NewInt(b))
		return &BigInteger{value: result}
	}
	return NewInteger(sum)
}

// subInt64 subtracts two int64 values, promoting to BigInteger on overflow.
func subInt64(a, b int64) Number {
	diff := a - b
	if (a^b) < 0 && (a^diff) < 0 {
		result := new(big.Int).Sub(big.NewInt(a), big.NewInt(b))
		return &BigInteger{value: result}
	}
	return NewInteger(diff)
}

// mulInt64 multiplies two int64 values, promoting to BigInteger on overflow.
func mulInt64(a, b int64) Number {
	if a == 0 || b == 0 {
		return NewInteger(0)
	}
	prod := a * b
	if prod/a != b {
		result := new(big.Int).Mul(big.NewInt(a), big.NewInt(b))
		return &BigInteger{value: result}
	}
	return NewInteger(prod)
}

// negateInt64 negates an int64 value, promoting to BigInteger for MinInt64.
func negateInt64(v int64) Number {
	if v == math.MinInt64 {
		result := new(big.Int).Neg(big.NewInt(v))
		return &BigInteger{value: result}
	}
	return NewInteger(-v)
}

// Add returns the sum of this integer and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// Kind returns the numeric kind for dispatch table indexing.
func (p *Integer) Kind() NumericKind {
	return KindInteger
}

var integerAdd [numKinds]func(*Integer, Number) Number
var integerSubtract [numKinds]func(*Integer, Number) Number
var integerLessThan [numKinds]func(*Integer, Number) bool
var integerCompare [numKinds]func(*Integer, Number) int
var integerMultiply [numKinds]func(*Integer, Number) Number
var integerDivide [numKinds]func(*Integer, Number) Number

func init() {
	integerAdd = makeAddDispatch(KindInteger, func(p *Integer, o Number) Number {
		return addInt64(p.Value, o.(*Integer).Value)
	})

	integerSubtract = makeSubtractDispatch(KindInteger, func(p *Integer, o Number) Number {
		return subInt64(p.Value, o.(*Integer).Value)
	})

	integerLessThan = makeLessThanDispatch(KindInteger, func(p *Integer, o Number) bool {
		return p.Value < o.(*Integer).Value
	})

	integerCompare = makeCompareDispatch(KindInteger, func(p *Integer, o Number) int {
		v := o.(*Integer)
		if p.Value < v.Value {
			return -1
		} else if p.Value > v.Value {
			return 1
		}
		return 0
	})

	integerMultiply = makeMultiplyDispatch(KindInteger, func(p *Integer, o Number) Number {
		return mulInt64(p.Value, o.(*Integer).Value)
	})

	integerDivide = makeDivideDispatch(KindInteger, func(p *Integer, o Number) Number {
		v := o.(*Integer)
		result := NewRational(p.Value, v.Value)
		if result.IsInteger() {
			return NewInteger(result.NumInt64())
		}
		return result
	})
}

// R7RS §6.2.2 Exactness: exact + exact = exact, exact + inexact = inexact.
// When adding Integer + BigInteger, result is BigInteger (exact).
// When adding Integer + Float/Complex, result is Float/Complex (inexact).
func (p *Integer) Add(o Number) Number {
	v, ok := o.(*Integer)
	if ok {
		return addInt64(p.Value, v.Value)
	}
	return integerAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this integer and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: exact - exact = exact, exact - inexact = inexact.
func (p *Integer) Subtract(o Number) Number {
	v, ok := o.(*Integer)
	if ok {
		return subInt64(p.Value, v.Value)
	}
	return integerSubtract[o.Kind()](p, o)
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
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	v, ok := o.(*Integer)
	if ok {
		return mulInt64(p.Value, v.Value)
	}
	return integerMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this integer and another number.
//
// R7RS §6.2.6: The / procedure returns the quotient of its arguments.
// For exact arguments, / may return a non-integer (Rational) when the
// mathematical result is not an integer. Returns Integer only when
// the division is exact.
//
// R7RS §6.2.2 Exactness: exact / exact = exact (Integer or Rational),
// exact / inexact = inexact (Float or Complex).
func (p *Integer) Divide(o Number) Number {
	if o.IsZero() && o.IsExact() {
		panic(ErrDivisionByZero)
	}
	v, ok := o.(*Integer)
	if ok {
		result := NewRational(p.Value, v.Value)
		if result.IsInteger() {
			return NewInteger(result.NumInt64())
		}
		return result
	}
	return integerDivide[o.Kind()](p, o)
}

// IsZero returns true if this integer is zero.
func (p *Integer) IsZero() bool {
	return p.Value == 0
}

// LessThan returns true if this integer is less than another number.
//
// R7RS §6.2.6: The < procedure returns #t if its arguments are monotonically
// increasing. Comparison across numeric types uses mathematical value.
func (p *Integer) LessThan(o Number) bool {
	v, ok := o.(*Integer)
	if ok {
		return p.Value < v.Value
	}
	return integerLessThan[o.Kind()](p, o)
}

func (p *Integer) Abs() Number {
	if p.Value < 0 {
		return negateInt64(p.Value)
	}
	return NewInteger(p.Value)
}

// Negate returns the negation of this integer.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Integer) Negate() Number {
	return negateInt64(p.Value)
}

// ToExact returns this Integer unchanged since it is already exact.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
func (p *Integer) ToExact() Number {
	return p
}

// ToInexact converts this Integer to an inexact Float.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
func (p *Integer) ToInexact() Number {
	return NewFloat(float64(p.Value))
}

// IsPositive returns true if this integer is positive.
//
// R7RS §6.2.6: positive? returns #t if the real number is positive.
func (p *Integer) IsPositive() bool {
	return p.Value > 0
}

// IsNegative returns true if this integer is negative.
//
// R7RS §6.2.6: negative? returns #t if the real number is negative.
func (p *Integer) IsNegative() bool {
	return p.Value < 0
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
func (p *Integer) Sign() int {
	if p.Value < 0 {
		return -1
	}
	if p.Value > 0 {
		return 1
	}
	return 0
}

// Compare compares this integer with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Integer) Compare(o Number) int {
	v, ok := o.(*Integer)
	if ok {
		if p.Value < v.Value {
			return -1
		} else if p.Value > v.Value {
			return 1
		}
		return 0
	}
	return integerCompare[o.Kind()](p, o)
}

// IsExact returns true since Integer is always exact.
//
// R7RS §6.2.2: Integers are always exact numbers.
func (p *Integer) IsExact() bool {
	return true
}

// IsInteger returns true since Integer is always an integer.
//
// R7RS §6.2.6: integer? returns #t for exact integers.
func (p *Integer) IsInteger() bool {
	return true
}

// IsRational returns true since integers are a subset of rationals.
//
// R7RS §6.2.6: rational? returns #t for all real finite numbers.
func (p *Integer) IsRational() bool {
	return true
}

// IsFinite returns true since integers are always finite.
//
// R7RS §6.2.6: finite? returns #t for all exact numbers.
func (p *Integer) IsFinite() bool {
	return true
}

// IsNaN returns false since integers are never NaN.
//
// R7RS §6.2.6: nan? returns #f for exact numbers.
func (p *Integer) IsNaN() bool {
	return false
}

// IsVoid returns true if this integer is nil.
func (p *Integer) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both integers have the same value.
//
// R7RS §6.2.6: The = procedure compares numerical values for equality.
// This implements structural equality for the Integer type specifically.
// Handles comparison with Integer, BigInteger, and Rational types for symmetry.
func (p *Integer) EqualTo(v Value) bool {
	switch other := v.(type) {
	case *Integer:
		return p.Value == other.Value
	case *BigInteger:
		return other.BigInt().Cmp(p.bigInt()) == 0
	case *Rational:
		return other.value.Cmp(new(big.Rat).SetInt64(p.Value)) == 0
	}
	return false
}

// SchemeString returns the Scheme representation of this integer.
func (p *Integer) SchemeString() string {
	return strconv.FormatInt(p.Value, 10)
}
