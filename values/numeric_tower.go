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

	"github.com/aalpar/wile/werr"
)

// multiplyResultForZero returns the correct result when one operand is zero
// in a multiplication, following R7RS §6.2.2 and Chez Scheme behavior.
//
// R7RS permits (* 0 x) to return exact 0 even when x is inexact.
// Rule: if either operand is exact, return exact zero (Integer 0).
// If both operands are inexact, return the zero operand unchanged.
//
// Callers must ensure `other` is finite before calling this function.
// IEEE 754 requires 0 * inf = NaN and 0 * NaN = NaN, so the exact-zero
// rule does not apply when the non-zero operand is infinite or NaN.
func multiplyResultForZero(zero, other Number) Number {
	if zero.IsExact() || other.IsExact() {
		return NewInteger(0)
	}
	return zero
}

// floatToExact converts a float64 to its exact Number representation.
// Returns Integer or BigInteger if the float is integral, Rational otherwise.
//
// R7RS §6.2.6: (exact z) returns an exact representation of z.
// R7RS says (exact +inf.0) and (exact +nan.0) should raise an error.
func floatToExact(f float64) (Number, error) {
	// big.Rat.SetFloat64 returns nil for infinity and NaN (non-finite values).
	// R7RS requires raising an error for (exact +inf.0) and (exact +nan.0).
	r := new(big.Rat).SetFloat64(f)
	if r == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion,
			"cannot convert non-finite float to exact")
	}
	if r.IsInt() {
		num := r.Num()
		if num.IsInt64() {
			return NewBigIntegerFromInt64(num.Int64()), nil
		}
		return NewBigInteger(new(big.Int).Set(num)), nil
	}
	return NewRationalFromRat(r), nil
}

// Simplify attempts to reduce a number to a simpler type without losing information.
//
// Simplification rules:
// - BigComplex with zero imaginary → real part
// - Complex with zero imaginary → Float → possibly Integer
// - BigFloat that is an integer → BigInteger → possibly Integer
// - Float that is an integer → Integer
// - Rational that is an integer → BigInteger → possibly Integer
// - BigInteger that fits int64 → Integer
func Simplify(n Number) Number {
	switch v := n.(type) {
	case *BigComplex:
		if v.Imag().IsZero() {
			return Simplify(v.Real())
		}
	case *Complex:
		if imag(v.Value) == 0 {
			return Simplify(NewFloat(real(v.Value)))
		}
	case *BigFloat:
		if v.value.IsInt() {
			bi, _ := v.value.Int(nil)
			return Simplify(&BigInteger{value: bi})
		}
	case *Float:
		// Check if float is a whole number that fits in int64
		if v.Value == float64(int64(v.Value)) {
			return NewInteger(int64(v.Value))
		}
	case *Rational:
		if v.IsInteger() {
			return Simplify(&BigInteger{value: new(big.Int).Set(v.Num())})
		}
	case *BigInteger:
		if v.value.IsInt64() {
			return NewInteger(v.value.Int64())
		}
	}
	return n
}

// Exactness represents whether a number is exact or inexact.
//
// R7RS §6.2.2: Numbers are either exact or inexact. A number is exact if it
// was written as an exact constant or derived from exact numbers using only
// exact operations. Otherwise, it is inexact.
type Exactness int

// Exactness constants for R7RS exact/inexact classification.
const (
	Exact Exactness = iota
	Inexact
)

// ExactnessOf returns the exactness of a number.
//
// R7RS §6.2.2:
// - Integer, BigInteger, Rational are exact
// - Float, BigFloat, Complex are inexact
// - BigComplex depends on its components
func ExactnessOf(n Number) Exactness {
	switch v := n.(type) {
	case *Integer, *BigInteger, *Rational:
		return Exact
	case *Float, *BigFloat, *Complex:
		return Inexact
	case *BigComplex:
		if v.IsExact() {
			return Exact
		}
		return Inexact
	}
	panic(werr.ErrNotANumber)
}

// IntegerEqualsFloat compares an exact integer to an inexact float.
// Returns true only if the float exactly represents the integer value.
//
// R7RS §6.2.5: Numeric equality must not lose precision. An exact integer
// and an inexact float are equal only if the float exactly represents
// the integer's value.
func IntegerEqualsFloat(i *Integer, f *Float) bool {
	// NaN is not equal to anything
	if math.IsNaN(f.Value) {
		return false
	}
	// Infinity cannot equal any integer
	if math.IsInf(f.Value, 0) {
		return false
	}
	// Non-integer floats cannot equal integers
	if f.Value != math.Trunc(f.Value) {
		return false
	}
	// For integers within float64's exact range (|n| <= 2^53), direct compare
	const maxExactFloat64Int = int64(1) << 53
	if i.Value >= -maxExactFloat64Int && i.Value <= maxExactFloat64Int {
		return float64(i.Value) == f.Value
	}
	// For larger integers, convert float to big.Rat and compare exactly
	r := new(big.Rat).SetFloat64(f.Value)
	if r == nil || !r.IsInt() {
		return false
	}
	return r.Num().Int64() == i.Value
}

// BigIntegerEqualsFloat compares a BigInteger to a Float.
// Returns true only if the float exactly represents the BigInteger value.
func BigIntegerEqualsFloat(bi *BigInteger, f *Float) bool {
	if math.IsNaN(f.Value) || math.IsInf(f.Value, 0) {
		return false
	}
	if f.Value != math.Trunc(f.Value) {
		return false
	}
	r := new(big.Rat).SetFloat64(f.Value)
	if r == nil || !r.IsInt() {
		return false
	}
	return bi.BigInt().Cmp(r.Num()) == 0
}
