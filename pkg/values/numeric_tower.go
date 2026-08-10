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

// floatToExact converts a float64 to its exact Number representation.
// Returns BigInteger if the float is integral, Rational otherwise. (Callers
// wanting the canonical smallest exact representation must Simplify the result.)
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
//   - BigComplex with an EXACT zero imaginary → real part (cross-kind; handled here)
//   - All other per-kind descents are delegated to the NumericTypeSpec.SimplifyDown
//     function registered for each kind (see values/numeric_registry.go).
//
// Exactness, not magnitude, licenses the complex→real descent, exactly as in
// maybeSimplify and IsReal. Demoting on an INEXACT zero imaginary part would lose
// information twice over, which is what this function exists not to do: it drops a
// component R7RS §6.2.6 says is still there (real? -2.5+0.0i is #f), and it then
// launders the inexact real into an exact one, breaking the "must not change
// exactness class" constraint stated in the exact-zero rule's contract
// (exact_zero.go).
//
// A *Complex therefore never descends: its parts are float64, so IsExact() is
// false unconditionally (complex.go) and a 0.0 imaginary part is always an inexact
// zero. There is no exact-zero-imag *Complex to descend from.
//
// Returns nil unchanged (callers may pass nil from generic Value paths).
func Simplify(n Number) Number {
	if n == nil {
		return nil
	}
	bc, ok := n.(*BigComplex)
	if ok && bc.IsReal() {
		return Simplify(bc.Real())
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

// NumericEquals implements R7RS `=` semantics for two numbers.
//
// R7RS §6.2.6: `=` returns #t if its arguments are numerically equal. It is one
// of the five predicates derived from CompareNumbers (compare.go) and it accepts
// exactly one of that kernel's four verdicts, so it agrees with `<`, `>`, `<=`
// and `>=` by construction rather than by two implementations happening to
// coincide.
//
// This used to be a hand-written ladder of type pairs -- Integer/Float and
// BigInteger/Float got exact arms, everything else fell through to
// a.Subtract(b).IsZero(). Subtract routes through the CONTAGION table, so
// Rational vs Float rounded the exact operand and answered #t for two distinct
// numbers, and Inf - Inf is NaN so an infinity was not equal to itself.
func NumericEquals(a, b Number) bool {
	return CompareNumbers(a, b) == OrderEqual
}
