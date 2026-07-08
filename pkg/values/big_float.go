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

	"github.com/aalpar/wile/pkg/werr"
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
//
// big.Float natively supports ±Inf via SetInf/IsInf. NaN has no native
// big.Float representation (operations that produce NaN under IEEE 754 panic
// with big.ErrNaN instead), so NaN is tracked via an out-of-band flag.
//
// Invariant: when nan is true, value MUST be a valid (zero-valued) *big.Float,
// never nil, to prevent nil-pointer panics.
type BigFloat struct {
	value *big.Float
	nan   bool
}

// NewBigFloat creates a new BigFloat from a big.Float.
func NewBigFloat(v *big.Float) *BigFloat {
	return &BigFloat{value: new(big.Float).Copy(v)}
}

// NewBigFloatNaN creates a new BigFloat representing NaN.
func NewBigFloatNaN() *BigFloat {
	return &BigFloat{value: new(big.Float), nan: true}
}

// NewBigFloatFromFloat64 creates a new BigFloat from a float64.
func NewBigFloatFromFloat64(v float64) *BigFloat {
	if math.IsNaN(v) {
		return NewBigFloatNaN()
	}
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

// recoverNaN wraps a big.Float operation that may panic with big.ErrNaN.
// If the operation panics with ErrNaN, returns a BigFloat NaN. Non-ErrNaN
// panics are re-panicked. Used to convert IEEE 754 NaN-producing operations
// (Inf+(-Inf), 0*Inf, Inf/Inf) into BigFloat NaN values.
func recoverNaN(op func() *BigFloat) (result *BigFloat) {
	defer func() {
		r := recover()
		if r != nil {
			if _, ok := r.(big.ErrNaN); ok {
				result = NewBigFloatNaN()
			} else {
				panic(r)
			}
		}
	}()
	return op()
}

// BigFloatValue returns the underlying big.Float value.
func (p *BigFloat) BigFloatValue() *big.Float {
	return p.value
}

// Float64Truncated returns the value as float64, silently rounding when the
// magnitude exceeds float64 precision. Use only when downstream code inherently
// cannot use the accuracy bit (math.Sin/Cos inputs, FNV hash seeds,
// transcendental coercions).
//
// For loss-signal-aware conversion, use Float64WithAccuracy() instead.
//
// NaN handling: a BigFloat with the NaN flag set returns math.NaN().
func (p *BigFloat) Float64Truncated() float64 {
	if p.nan {
		return math.NaN()
	}
	// big.Accuracy intentionally discarded: callers of Float64Truncated have
	// opted out of the loss signal by name. Use Float64WithAccuracy when the
	// accuracy bit is actionable.
	f, _ := p.value.Float64()
	return f
}

// Float64WithAccuracy returns the value as float64 along with Go's
// big.Accuracy indicator (Below / Exact / Above). Mirrors the stdlib
// (*big.Float).Float64() signature directly; the NaN flag is surfaced as
// (math.NaN(), big.Exact) since NaN→NaN is bit-pattern identity.
//
// Use this whenever the caller can reasonably act on the accuracy bit
// (precision-aware conversions, the ToFloat64WithAccuracy public helper).
func (p *BigFloat) Float64WithAccuracy() (float64, big.Accuracy) {
	if p.nan {
		return math.NaN(), big.Exact
	}
	return p.value.Float64()
}

// HashCode returns a hash code for this BigFloat.
// For Inf and NaN, delegates to float64 bit pattern matching Float.HashCode,
// ensuring Float and BigFloat produce identical hashes for equal values.
//
// Note: NaN != NaN (IEEE 754), so two NaN BigFloats are never EqualTo each
// other. A NaN key stored in a hashtable is therefore unretrievable — the
// Hashable contract is not violated, but NaN is not a useful hashtable key.
func (p *BigFloat) HashCode() uint64 {
	if p.nan {
		return hashUint64(0x5, math.Float64bits(math.NaN()))
	}
	if p.value.IsInf() {
		return hashUint64(0x5, math.Float64bits(p.Float64Truncated()))
	}
	return hashInexactNumeric(p.value)
}

// Kind returns the numeric kind for dispatch table indexing.
func (p *BigFloat) Kind() NumericKind {
	return KindBigFloat
}

// bigFloatSimplifyDown descends BigFloat → BigInteger → Integer in a single
// call when the value is integer-valued and fits each smaller representation.
//
// The discarded big.Accuracy on Int(nil) is Exact by construction: IsInt()
// returned true, so the value has no fractional part and the integer cast
// is mathematically lossless.
func bigFloatSimplifyDown(n Number) Number {
	v := n.(*BigFloat)
	if !v.value.IsInt() {
		return n
	}
	bi, _ := v.value.Int(nil)
	bigInt := &BigInteger{value: bi}
	if bigInt.value.IsInt64() {
		return NewInteger(bigInt.value.Int64())
	}
	return bigInt
}

// bigFloatToFloat64WithAccuracy converts a BigFloat to float64 with loss-signal.
// Handles the NaN case first (NaN→NaN is identity, reported as Exact per Q-6).
func bigFloatToFloat64WithAccuracy(n Number) (float64, big.Accuracy, bool) {
	p := n.(*BigFloat)
	if p.IsNaN() {
		return math.NaN(), big.Exact, true // NaN→NaN identity
	}
	f, acc := p.value.Float64()
	return f, acc, true
}

var bigFloatAdd [numKinds]func(*BigFloat, Number) Number
var bigFloatSubtract [numKinds]func(*BigFloat, Number) Number
var bigFloatLessThan [numKinds]func(*BigFloat, Number) bool
var bigFloatCompare [numKinds]func(*BigFloat, Number) int
var bigFloatMultiply [numKinds]func(*BigFloat, Number) Number
var bigFloatDivide [numKinds]func(*BigFloat, Number) (Number, error)

func init() {
	bigFloatAdd = makeAddDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
		return p.Add(o)
	})

	bigFloatSubtract = makeSubtractDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
		return p.Subtract(o)
	})

	bigFloatLessThan = makeLessThanDispatch(KindBigFloat, func(p *BigFloat, o Number) bool {
		return p.LessThan(o)
	})

	bigFloatCompare = makeCompareDispatch(KindBigFloat, func(p *BigFloat, o Number) int {
		return p.Compare(o)
	})

	bigFloatMultiply = makeMultiplyDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
		return p.Multiply(o)
	})

	bigFloatDivide = makeDivideDispatch(KindBigFloat, func(p *BigFloat, o Number) (Number, error) {
		return p.Divide(o)
	})

	registerNumericSpec(KindBigFloat, NumericTypeSpec{
		schemeName:            "real",
		simplifyDown:          bigFloatSimplifyDown,
		toFloat64WithAccuracy: bigFloatToFloat64WithAccuracy,
		isAlwaysExact:         false,
		isAlwaysReal:          true,
	})
}

// Add returns the sum of this BigFloat and another number.
//
// R7RS §6.2.6: The + procedure returns the sum of its arguments.
// R7RS §6.2.2 Exactness: inexact + inexact = inexact, exact + inexact = inexact.
func (p *BigFloat) Add(o Number) Number {
	v, ok := o.(*BigFloat)
	if ok {
		if p.nan || v.nan {
			return NewBigFloatNaN()
		}
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
		})
	}
	return bigFloatAdd[o.Kind()](p, o)
}

// Subtract returns the difference of this BigFloat and another number.
//
// R7RS §6.2.6: The - procedure returns the difference of its arguments.
// R7RS §6.2.2 Exactness: inexact - inexact = inexact, exact - inexact = inexact.
func (p *BigFloat) Subtract(o Number) Number {
	v, ok := o.(*BigFloat)
	if ok {
		if p.nan || v.nan {
			return NewBigFloatNaN()
		}
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Sub(p.value, v.value)}
		})
	}
	return bigFloatSubtract[o.Kind()](p, o)
}

// Multiply returns the product of this BigFloat and another number.
//
//nolint:dupl // Type dispatch pattern repeated across numeric tower
func (p *BigFloat) Multiply(o Number) Number {
	if p.nan || o.IsNaN() {
		return NewBigFloatNaN()
	}
	if o.IsZero() {
		if !p.IsFinite() {
			return NewBigFloatNaN() // 0 * Inf = NaN
		}
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	v, ok := o.(*BigFloat)
	if ok {
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
		})
	}
	return bigFloatMultiply[o.Kind()](p, o)
}

// Divide returns the quotient of this BigFloat and another number.
func (p *BigFloat) Divide(o Number) (Number, error) {
	if p.nan || o.IsNaN() {
		return NewBigFloatNaN(), nil
	}
	if o.IsZero() {
		if o.IsExact() {
			return nil, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "BigFloat.Divide: division by exact zero")
		}
		// Inexact zero: IEEE 754 semantics — ±Inf or NaN
		if p.nan || p.IsZero() {
			return NewBigFloatNaN(), nil
		}
		return NewBigFloat(new(big.Float).SetInf(p.value.Sign() < 0)), nil
	}
	v, ok := o.(*BigFloat)
	if ok {
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Quo(p.value, v.value)}
		}), nil
	}
	return bigFloatDivide[o.Kind()](p, o)
}

// Negate returns the negation of this BigFloat.
func (p *BigFloat) Negate() Number {
	if p.nan {
		return NewBigFloatNaN()
	}
	return &BigFloat{value: new(big.Float).Neg(p.value)}
}

// IsZero returns true if this BigFloat is zero.
func (p *BigFloat) IsZero() bool {
	return !p.nan && p.value.Sign() == 0
}

// LessThan returns true if this BigFloat is less than another number.
func (p *BigFloat) LessThan(o Number) bool {
	if p.nan || o.IsNaN() {
		return false
	}
	v, ok := o.(*BigFloat)
	if ok {
		return p.value.Cmp(v.value) < 0
	}
	return bigFloatLessThan[o.Kind()](p, o)
}

// IsNegative returns true if this BigFloat is negative.
// NaN has no sign and returns false.
func (p *BigFloat) IsNegative() bool {
	return !p.nan && p.value.Sign() < 0
}

// IsPositive returns true if this BigFloat is positive.
// NaN has no sign and returns false.
func (p *BigFloat) IsPositive() bool {
	return !p.nan && p.value.Sign() > 0
}

// IsExact returns false since BigFloat is always inexact.
func (p *BigFloat) IsExact() bool {
	return false // BigFloat is inexact
}

// IsInteger returns true if this BigFloat represents an integer value.
//
// R7RS §6.2.6: integer? returns #t for inexact integers.
func (p *BigFloat) IsInteger() bool {
	return !p.nan && p.value.IsInt()
}

// IsRational returns true if this BigFloat holds a finite value.
//
// R7RS §6.2.6: rational? returns #t for all finite real numbers.
// Inf and NaN are not rational.
func (p *BigFloat) IsRational() bool {
	return !p.nan && !p.value.IsInf()
}

// IsFinite returns true if this BigFloat holds a finite value.
//
// R7RS §6.2.6: finite? returns #t for finite numbers.
func (p *BigFloat) IsFinite() bool {
	return !p.nan && !p.value.IsInf()
}

// IsNaN returns true if this BigFloat holds NaN.
//
// R7RS §6.2.6: nan? returns #t for NaN values.
func (p *BigFloat) IsNaN() bool {
	return p.nan
}

// ToExact converts this BigFloat to an exact Rational.
//
// R7RS §6.2.6: (exact +inf.0) and (exact +nan.0) are errors.
func (p *BigFloat) ToExact() (Number, error) {
	if p.nan || p.value.IsInf() {
		return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion,
			"cannot convert non-finite BigFloat to exact")
	}
	r, _ := p.value.Rat(nil)
	if r == nil {
		return NewRational(0, 1), nil
	}
	return NewRationalFromRat(r), nil
}

// ToInexact returns this BigFloat unchanged since it's already inexact.
func (p *BigFloat) ToInexact() Number {
	return p
}

// Abs returns the absolute value of this BigFloat.
func (p *BigFloat) Abs() Number {
	if p.nan {
		return NewBigFloatNaN()
	}
	return NewBigFloat(new(big.Float).Abs(p.value))
}

// Sign returns -1 if negative, 0 if zero, or 1 if positive.
// NaN returns 0 (NaN has no sign).
func (p *BigFloat) Sign() int {
	if p.nan {
		return 0
	}
	return p.value.Sign()
}

// Compare compares this BigFloat with another number.
// Returns 0 for NaN operands (NaN has no valid ordering).
func (p *BigFloat) Compare(o Number) int {
	if p.nan || o.IsNaN() {
		return 0
	}
	v, ok := o.(*BigFloat)
	if ok {
		return p.value.Cmp(v.value)
	}
	return bigFloatCompare[o.Kind()](p, o)
}

// SchemeString returns the Scheme representation of this BigFloat.
//
// R7RS §6.2.6: Inexact integers must include a decimal point to distinguish
// them from exact integers. big.Float.Text('g', -1) drops ".0" for integer
// values, so we append it when neither '.' nor 'e'/'E' is present.
func (p *BigFloat) SchemeString() string {
	if p.nan {
		return NaNString
	}
	if p.value.IsInf() {
		if p.value.Sign() < 0 {
			return NegativeInfinityString
		}
		return PositiveInfinityString
	}
	s := p.value.Text('g', -1)
	for i := 0; i < len(s); i++ {
		if s[i] == '.' || s[i] == 'e' || s[i] == 'E' {
			return s
		}
	}
	return s + ".0"
}

// IsVoid returns true if this BigFloat is nil.
func (p *BigFloat) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this BigFloat equals another value.
// NaN is not equal to anything, including itself (IEEE 754).
func (p *BigFloat) EqualTo(o Value) bool {
	v, ok := o.(*BigFloat)
	if ok {
		if v == nil || p == nil {
			return p == v
		}
		if p.nan || v.nan {
			return false
		}
		return p.value.Cmp(v.value) == 0
	}
	f, ok := o.(*Float)
	if ok {
		if p.nan || math.IsNaN(f.Value) {
			return false
		}
		vf := new(big.Float).SetFloat64(f.Value)
		return p.value.Cmp(vf) == 0
	}
	return false
}
