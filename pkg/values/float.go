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
// Uses the canonical inexact-family hash so that Float and BigFloat
// produce identical hashes for equal values.
// NaN and Inf use bitwise hashing as a fallback since BigFloat has
// no Inf/NaN, making cross-type equality impossible for those values.
func (p *Float) HashCode() uint64 {
	if math.IsNaN(p.Value) || math.IsInf(p.Value, 0) {
		return hashUint64(0x5, math.Float64bits(p.Value))
	}
	return hashInexactNumeric(new(big.Float).SetFloat64(p.Value))
}

// Per-type conversion helpers for Float.
// These eliminate repeated conversion expressions in the type-switch
// dispatch methods below. Each produces the representation needed by
// the target type's native arithmetic.

func (p *Float) bigFloat() *big.Float {
	return new(big.Float).SetFloat64(p.Value)
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *Float) Kind() NumericKind {
	return KindFloat
}

// floatSimplifyDown demotes an integer-valued Float to Integer. The
// double-cast equality guard rejects |Value| > MaxInt64 by way of Go's
// implementation-defined overflow on the int64 cast — the resulting
// inequality fails the test, so the float is left as-is.
func floatSimplifyDown(n Number) Number {
	v := n.(*Float)
	if v.Value == float64(int64(v.Value)) {
		return NewInteger(int64(v.Value))
	}
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
var floatCompare [numKinds]func(*Float, Number) int
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

	floatCompare = makeCompareDispatch(KindFloat, func(p *Float, o Number) int {
		pf := p.bigFloat()
		vf := new(big.Float).SetFloat64(o.(*Float).Value)
		return pf.Cmp(vf)
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
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value + v.Value)
	}
	return floatAdd[o.Kind()](p, o)
}

// Subtract returns the difference of two numbers.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: inexact - inexact = inexact, exact - inexact = inexact.
func (p *Float) Subtract(o Number) Number {
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value - v.Value)
	}
	return floatSubtract[o.Kind()](p, o)
}

// Multiply returns the product of two numbers.
//
// R7RS §6.2.6: The * procedure returns the product of its arguments.
// R7RS §6.2.2: Exact zero dominates—(* 0 x) may return exact 0 even when
// x is inexact. Zero is an exact value when the result is mathematically
// unambiguous. This implementation follows Chez Scheme's behavior.
func (p *Float) Multiply(o Number) Number {
	z, ok := exactZeroProduct(p, o)
	if ok {
		return z
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value * v.Value)
	}
	return floatMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this float and another number.
func (p *Float) Divide(o Number) (Number, error) {
	if o.IsZero() && o.IsExact() {
		return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Float.Divide: division by exact zero")
	}
	v, ok := o.(*Float)
	if ok {
		return NewFloat(p.Value / v.Value), nil
	}
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

// Compare compares this float with another number.
//
// R7RS §6.2.6: Numeric comparisons use mathematical value regardless of exactness.
// Returns -1 if p < o, 0 if p == o, 1 if p > o.
func (p *Float) Compare(o Number) int {
	v, ok := o.(*Float)
	if !ok {
		return floatCompare[o.Kind()](p, o)
	}
	pf := p.bigFloat()
	vf := new(big.Float).SetFloat64(v.Value)
	return pf.Cmp(vf)
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

// EqualTo returns true if both floats have the same value.
// Handles comparison with both Float and BigFloat types for symmetry.
func (p *Float) EqualTo(v Value) bool {
	switch other := v.(type) {
	case *Float:
		return p.Value == other.Value
	case *BigFloat:
		if math.IsNaN(p.Value) || other.IsNaN() {
			return false
		}
		vf := new(big.Float).SetFloat64(p.Value)
		return vf.Cmp(other.BigFloatValue()) == 0
	}
	return false
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
