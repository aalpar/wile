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

	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ Value      = (*Float)(nil)
	_ Number     = (*Float)(nil)
	_ RealNumber = (*Float)(nil)
	_ Hashable   = (*Float)(nil)
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

// HashCode returns a hash of the float value.
//
// The Hashable contract is one-directional — equal implies same hash — and after the
// R7RS §6.1 alignment a Float is NEVER eqv? to a BigFloat (representation is observable
// for inexacts, so they are distinct numbers). The contract therefore says nothing about
// the two, and this hash owes cross-type agreement to nothing. It used to claim it
// "produce[d] identical hashes for equal values" across Float and BigFloat, which is now
// a promise about a relation that cannot hold.
//
// What it DOES owe: every NaN hashes alike (eqv? identifies all NaNs, so the contract
// binds), and ±Inf stay bit-exact (+inf.0 and -inf.0 are NOT eqv?, so they must be able
// to differ).
func (p *Float) HashCode() uint64 {
	// Every NaN hashes alike, because eqv? identifies every NaN. Hashing the raw
	// bits would give (/ 0.0 0.0) and +nan.0 different hashes despite their being
	// equal?. See hashNaN.
	if math.IsNaN(p.Value) {
		return hashNaN()
	}
	// Inf keeps its bits: +inf.0 and -inf.0 are NOT eqv?, so they must be free to
	// hash differently.
	if math.IsInf(p.Value, 0) {
		return hashUint64(0x5, math.Float64bits(p.Value))
	}
	return hashInexactNumeric(new(big.Float).SetFloat64(p.Value))
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *Float) Kind() NumericKind {
	return KindFloat
}

// floatSimplifyDown is the identity. Simplify descends WITHIN an exactness class
// (R7RS §6.2.2), and *Float is the bottom of the inexact tier — there is nothing
// below it to descend to.
//
// It used to demote an integer-valued Float to an exact *Integer, which crosses
// the exactness class and is a bug, not an optimization: it would make
// (exact? 2.0) answer #t. It survived only because nothing called Simplify on a
// float — parse-time Simplify runs on exact literals. That "no live caller" is a
// thin guarantee: wiring Simplify into an arithmetic path is a two-line change (it
// is exactly what rational.go now does to canonicalize denominator-1 results), and
// the next person to do it for floats would have silently made every whole-valued
// float exact.
//
// The same demotion was at bigFloatSimplifyDown; both are identities now.
func floatSimplifyDown(n Number) Number {
	return n
}

// floatToFloat64WithAccuracy returns the underlying float64; always Exact
// since *Float IS a float64 (identity conversion, no precision change).
// NaN and Inf are also Exact per Q-6: identity is bit-pattern identity.
func floatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	return n.(*Float).Value, big.Exact, true
}

var floatAdd [numKinds]func(*Float, Number) Number
var floatSubtract [numKinds]func(*Float, Number) Number
var floatLessThan [numKinds]func(*Float, Number) bool
var floatMultiply [numKinds]func(*Float, Number) Number
var floatDivide [numKinds]func(*Float, Number) (Number, error)

func init() {
	floatAdd = makeAddDispatch(KindFloat, func(p *Float, o Number) Number {
		return NewFloat(p.Value + o.(*Float).Value)
	})

	floatSubtract = makeSubtractDispatch(KindFloat, func(p *Float, o Number) Number {
		return NewFloat(p.Value - o.(*Float).Value)
	})

	floatLessThan = makeLessThanDispatch(KindFloat, func(p *Float, o Number) bool {
		return p.Value < o.(*Float).Value
	})

	floatMultiply = makeMultiplyDispatch(KindFloat, func(p *Float, o Number) Number {
		return NewFloat(p.Value * o.(*Float).Value)
	})

	floatDivide = makeDivideDispatch(KindFloat, func(p *Float, o Number) (Number, error) {
		return NewFloat(p.Value / o.(*Float).Value), nil
	})

	registerNumericSpec(KindFloat, NumericTypeSpec{
		schemeName:            "real",
		simplifyDown:          floatSimplifyDown,
		toFloat64WithAccuracy: floatToFloat64WithAccuracy,
		isAlwaysExact:         false,
		isAlwaysReal:          true,
	})
}

// Add returns the sum of this Float and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: inexact + inexact = inexact, exact + inexact = inexact.
func (p *Float) Add(o Number) Number {
	if isExactZero(o) {
		return p
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value + v.Value)
	}
	// A *Complex operand falls through to the dispatch table, which computes
	// real ⊕ complex part-wise and never manufactures an imaginary component.
	// Float shares that path with the exact kinds; see makeArithmeticDispatch.
	return floatAdd[o.Kind()](p, o)
}

// Subtract returns the difference of two numbers.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: inexact - inexact = inexact, exact - inexact = inexact.
func (p *Float) Subtract(o Number) Number {
	if isExactZero(o) {
		return p
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value - v.Value)
	}
	// A *Complex operand falls through to the dispatch table; see Add.
	return floatSubtract[o.Kind()](p, o)
}

// Multiply returns the product of two numbers.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
//
// Exactness: an exact zero annihilates the product to exact 0, regardless of the
// other operand (R7RS §6.2.2; matches Chez and Racket, including (* +inf.0 0) => 0).
// An inexact zero does not short-circuit: IEEE 754 governs, so (* 5 0.0) => 0.0 and
// (* -1.0 0.0) => -0.0.
func (p *Float) Multiply(o Number) Number {
	if exactZeroEither(p, o) {
		return NewInteger(0)
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value * v.Value)
	}
	// A *Complex operand falls through to the dispatch table; see Add.
	return floatMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this float and another number.
func (p *Float) Divide(o Number) (Number, error) {
	// The exact-zero rule for division; exactZeroTable[zeroDiv] in exact_zero.go.
	switch exactZeroDivideAction(p, o) {
	case zeroRaise:
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Float.Divide: division by exact zero")
	case zeroYieldExactZero:
		return NewInteger(0), nil
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value / v.Value), nil
	}
	// A *Complex divisor falls through to the dispatch table; see Add.
	return floatDivide[o.Kind()](p, o)
}

// IsZero returns true if this float is zero.
func (p *Float) IsZero() bool {
	return p.Value == 0.0
}

// LessThan returns true if this float is less than another number.
func (p *Float) LessThan(o Number) bool {
	v, ok := o.(*Float)
	if ok {
		return p.Value < v.Value
	}
	return floatLessThan[o.Kind()](p, o)
}

// Abs returns the absolute value of this float.
func (p *Float) Abs() Number {
	return NewFloat(math.Abs(p.Value))
}

// ToExact converts this Float to an exact Number.
//
// R7RS §6.2.6: exact returns an exact representation of its argument.
// Returns Integer if the float is integral, Rational otherwise.
func (p *Float) ToExact() (Number, error) {
	return floatToExact(p.Value)
}

// ToInexact returns this Float unchanged since it is already inexact.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
func (p *Float) ToInexact() Number {
	return p
}

// IsPositive returns true if this float is positive.
func (p *Float) IsPositive() bool {
	return p.Value > 0
}

// IsNegative returns true if this float is negative.
func (p *Float) IsNegative() bool {
	return p.Value < 0
}

// SignBit reports whether this float carries a negative sign bit, INCLUDING -0.0.
//
// This is the case IsNegative cannot see: -0.0 < 0 is false, so IsNegative reports
// false for a value that is unambiguously on the negative side of the real axis.
// math.Signbit reads the bit itself.
func (p *Float) SignBit() bool {
	return math.Signbit(p.Value)
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
// NaN returns 0.
func (p *Float) Sign() int {
	if p.Value < 0 {
		return -1
	}
	if p.Value > 0 {
		return 1
	}
	return 0
}

// Negate returns the negation of this float.
//
// R7RS §6.2.6: The - procedure with one argument returns the additive inverse.
func (p *Float) Negate() Number {
	return NewFloat(-p.Value)
}

// IsExact returns false since Float is always inexact.
//
// R7RS §6.2.2: Floating-point numbers are inexact.
func (p *Float) IsExact() bool {
	return false
}

// IsInteger returns true if this float represents an integer value.
//
// R7RS §6.2.6: integer? returns #t for inexact integers (e.g., 3.0).
// Uses math.Trunc to correctly handle large floats outside int64 range.
func (p *Float) IsInteger() bool {
	return p.Value == math.Trunc(p.Value) && !math.IsInf(p.Value, 0) && !math.IsNaN(p.Value)
}

// IsRational returns true if this float is finite (not NaN or Inf).
//
// R7RS §6.2.6: rational? returns #t for finite inexact reals.
func (p *Float) IsRational() bool {
	return !math.IsNaN(p.Value) && !math.IsInf(p.Value, 0)
}

// IsFinite returns true if this float is finite (not Inf or NaN).
//
// R7RS §6.2.6: finite? returns #t for finite numbers.
func (p *Float) IsFinite() bool {
	return !math.IsInf(p.Value, 0) && !math.IsNaN(p.Value)
}

// IsNaN returns true if this float is NaN.
//
// R7RS §6.2.6: nan? returns #t for NaN values.
func (p *Float) IsNaN() bool {
	return math.IsNaN(p.Value)
}

// IsVoid returns true if the float is nil.
func (p *Float) IsVoid() bool {
	return p == nil
}

// EqualTo implements R7RS equal? for Float.
//
// R7RS §6.1: equal? "returns the same as eqv? when applied to … numbers" — no
// latitude. So this delegates to EqvNumber (eqv.go), the single authority on
// numeric equivalence, rather than restating the rules. Restating them is what
// let equal? and eqv? drift apart on signed zero and on cross-representation
// inexacts.
func (p *Float) EqualTo(v Value) bool {
	return eqvNumberValue(p, v)
}

// SchemeString returns the Scheme representation of the float.
//
// R7RS §6.2.5: +inf.0, -inf.0, and +nan.0 are the written representations
// for positive infinity, negative infinity, and NaN.
// R7RS §7.1.1: Inexact real numbers must contain a decimal point to distinguish
// them from exact integers.
func (p *Float) SchemeString() string {
	return formatInexactReal(p.Value)
}

// formatInexactReal formats a float64 as its R7RS inexact-real external
// representation: lowercase +inf.0 / -inf.0 / +nan.0 for the IEEE 754 special
// values (R7RS §6.2.5), and a forced decimal point on finite values so they read
// back as inexact rather than exact integers (R7RS §7.1.1). Shared by
// Float.SchemeString and the complex-component formatter in complex.go.
func formatInexactReal(f float64) string {
	if math.IsInf(f, 1) {
		return PositiveInfinityString
	}
	if math.IsInf(f, -1) {
		return NegativeInfinityString
	}
	if math.IsNaN(f) {
		return NaNString
	}
	s := strconv.FormatFloat(f, 'f', -1, 64)
	for i := 0; i < len(s); i++ {
		if s[i] == '.' {
			return s
		}
	}
	return s + ".0"
}

func (p *Float) String() string {
	return strconv.FormatFloat(p.Value, 'f', -1, 64)
}
