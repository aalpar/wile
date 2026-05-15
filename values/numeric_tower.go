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
// Abstract interpretation (Cousot & Cousot 1977). Exactness is tracked
// in the two-point lattice {exact < inexact}.
//
//	α: Number → {exact, inexact}   (abstraction function)
//	Transfer for most ops: α(a op b) = α(a) ⊔ α(b)  (join = inexact wins)
//	Transfer for (* 0 x): α(result) = exact   if α(0)=exact ∨ α(x)=exact
//
//	The zero-absorbs rule is a strong update: the transfer function
//	returns a more precise result than the naive join because the
//	mathematical result (0) is known exactly.
//
//	Invariant: the strong update only applies when other is finite.
//	  IEEE 754 requires 0 * inf = NaN, so the exact-zero rule does
//	  not apply for non-finite operands.
//	Constrains: all arithmetic dispatch closures (must respect
//	  contagion), Simplify (must not change exactness class).
//	Constrained by: promotion lattice (T must be monotone w.r.t.
//	  the exactness ordering).
//
// See BIBLIOGRAPHY.md "Exactness as Abstract Interpretation".
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
//   - BigComplex with zero imaginary → real part (cross-kind; handled here)
//   - Complex with zero imaginary → Float → possibly Integer (cross-kind; handled here)
//   - All other per-kind descents are delegated to the NumericTypeSpec.SimplifyDown
//     function registered for each kind (see values/numeric_registry.go).
//
// Returns nil unchanged (callers may pass nil from generic Value paths).
func Simplify(n Number) Number {
	if n == nil {
		return nil
	}
	bc, ok := n.(*BigComplex)
	if ok && bc.Imag().IsZero() {
		return Simplify(bc.Real())
	}
	c, ok := n.(*Complex)
	if ok && imag(c.Value) == 0 {
		return Simplify(NewFloat(real(c.Value)))
	}
	return LookupNumericSpec(n.Kind()).SimplifyDown(n)
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
// - Integer, BigInteger, Rational are always exact (IsAlwaysExact in spec)
// - Float, BigFloat, Complex are always inexact (IsAlwaysExact == false)
// - BigComplex depends on its components (per-instance check via IsExact)
//
// Panics on nil; nil cannot meaningfully classify as Exact or Inexact and
// indicates a caller bug.
func ExactnessOf(n Number) Exactness {
	if n == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "ExactnessOf: nil Number"))
	}
	bc, ok := n.(*BigComplex)
	if ok {
		if bc.IsExact() {
			return Exact
		}
		return Inexact
	}
	if LookupNumericSpec(n.Kind()).IsAlwaysExact() {
		return Exact
	}
	return Inexact
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
