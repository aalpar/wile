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

package values_test

import (
	"fmt"
	"math"
	"math/big"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestExactnessContagionAddition validates R7RS §6.2.2 exactness contagion
// for addition across all numeric types.
//
// R7RS §6.2.2: "Inexactness is contagious: operations on inexact numbers
// produce inexact results. Exact 0 is a number, and infinities and NaNs
// are numbers whose use in arithmetic should be restricted to those
// occasions where they are needed."
//
// For addition: (+ 0 x) → x, (+ x 0) → x
// The result's exactness MUST match the other operand. This means:
//
//	(+ 0 0.0) → 0.0 (inexact)
//	(+ 0.0 0) → 0.0 (inexact)
//	(+ 0 0)   → 0   (exact)
//
// This test verifies that zero short-circuits do NOT happen in Add methods,
// because doing so would violate exactness contagion.
func TestExactnessContagionAddition(t *testing.T) {
	// Test case structure: exact zero + inexact zero = inexact zero
	// This is the critical case that detects improper zero short-circuits.
	tcs := []struct {
		name     string
		a        values.Number
		b        values.Number
		wantType string
		isExact  bool
	}{
		// Integer (exact) + Float (inexact) → Float (inexact)
		{"Integer 0 + Float 0.0", values.NewInteger(0), values.NewFloat(0.0), "Float", false},
		{"Float 0.0 + Integer 0", values.NewFloat(0.0), values.NewInteger(0), "Float", false},

		// Integer (exact) + Integer (exact) → Integer (exact)
		{"Integer 0 + Integer 0", values.NewInteger(0), values.NewInteger(0), "Integer", true},

		// BigInteger (exact) + Float (inexact) → Float (inexact).
		// An exact zero is an IDENTITY: it returns the other operand UNCHANGED,
		// so no promotion to the BigFloat LUB happens. (Corrected 2026-07-12: these
		// two rows said BigFloat, contradicting their Integer/Rational siblings
		// above and below, which already said Float. Nothing ever checked the field.)
		{"BigInteger 0 + Float 0.0", values.NewBigIntegerFromInt64(0), values.NewFloat(0.0), "Float", false},
		{"Float 0.0 + BigInteger 0", values.NewFloat(0.0), values.NewBigIntegerFromInt64(0), "Float", false},

		// BigInteger (exact) + BigFloat (inexact) → BigFloat (inexact)
		{"BigInteger 0 + BigFloat 0.0", values.NewBigIntegerFromInt64(0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 + BigInteger 0", values.NewBigFloatFromFloat64(0.0), values.NewBigIntegerFromInt64(0), "BigFloat", false},

		// Rational (exact) + Float (inexact) → Float (inexact)
		{"Rational 0/1 + Float 0.0", values.NewRational(0, 1), values.NewFloat(0.0), "Float", false},
		{"Float 0.0 + Rational 0/1", values.NewFloat(0.0), values.NewRational(0, 1), "Float", false},

		// Rational (exact) + BigFloat (inexact) → BigFloat (inexact)
		{"Rational 0/1 + BigFloat 0.0", values.NewRational(0, 1), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 + Rational 0/1", values.NewBigFloatFromFloat64(0.0), values.NewRational(0, 1), "BigFloat", false},

		// Integer (exact) + Complex (inexact) → Complex (inexact)
		{"Integer 0 + Complex 0+0i", values.NewInteger(0), values.NewComplex(0), "Complex", false},
		{"Complex 0+0i + Integer 0", values.NewComplex(0), values.NewInteger(0), "Complex", false},

		// BigInteger (exact) + BigComplex (can be exact or inexact depending on parts)
		{"BigInteger 0 + BigComplex(inexact)", values.NewBigIntegerFromInt64(0),
			values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(0), values.NewBigFloatFromFloat64(0)),
			"BigComplex", false},

		// Float (inexact) + Float (inexact) → Float (inexact)
		{"Float 0.0 + Float 0.0", values.NewFloat(0.0), values.NewFloat(0.0), "Float", false},

		// BigFloat (inexact) + BigFloat (inexact) → BigFloat (inexact)
		{"BigFloat 0.0 + BigFloat 0.0", values.NewBigFloatFromFloat64(0.0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := tc.a.Add(tc.b)
			c.Assert(result.IsExact(), qt.Equals, tc.isExact,
				qt.Commentf("Expected %s exactness to be %v, got %v", tc.name, tc.isExact, result.IsExact()))
			gotType := strings.TrimPrefix(fmt.Sprintf("%T", result), "*values.")
			c.Assert(gotType, qt.Equals, tc.wantType,
				qt.Commentf("exactness matched but the result type did not"))
		})
	}
}

// TestExactnessContagionSubtraction validates R7RS §6.2.2 exactness contagion
// for subtraction across all numeric types.
//
// The two operands short-circuit differently, because subtraction is not
// commutative: (- x 0) → x (identity), but (- 0 x) → -x (negation).
//
//	(- 0.0 0) →  0.0 (identity: the operand is returned untouched)
//	(- 0 0.0) → -0.0 (negation: an exact zero flips the sign bit)
//	(- 0 0)   →  0   (exact)
//
// Negation preserves the operand's TYPE, so an exact zero on the left yields
// o's kind rather than the promoted LUB. This table pins type and exactness;
// the SIGN of the result is pinned by TestExactZeroSubtractNegates.
func TestExactnessContagionSubtraction(t *testing.T) {
	tcs := []struct {
		name     string
		a        values.Number
		b        values.Number
		wantType string
		isExact  bool
	}{
		// An exact zero on the LEFT of Subtract NEGATES: (- 0 x) is -x. Negation
		// preserves the operand's TYPE, so the result is a Float, not the promoted
		// BigFloat LUB that a fall-through to dispatch would produce.
		{"Integer 0 - Float 0.0", values.NewInteger(0), values.NewFloat(0.0), "Float", false},
		// An exact zero on the RIGHT is an identity: returns the operand unchanged.
		{"Float 0.0 - Integer 0", values.NewFloat(0.0), values.NewInteger(0), "Float", false},

		// Integer (exact) - Integer (exact) → Integer (exact)
		{"Integer 0 - Integer 0", values.NewInteger(0), values.NewInteger(0), "Integer", true},

		// Negation, not promotion: a Float, exactly like its Integer sibling above.
		{"BigInteger 0 - Float 0.0", values.NewBigIntegerFromInt64(0), values.NewFloat(0.0), "Float", false},
		// Identity on the right: no promotion occurs.
		{"Float 0.0 - BigInteger 0", values.NewFloat(0.0), values.NewBigIntegerFromInt64(0), "Float", false},

		// Negating a BigFloat yields a BigFloat, so here the type is unchanged
		// either way; only the sign of the value moves (+0.0 → -0.0).
		{"BigInteger 0 - BigFloat 0.0", values.NewBigIntegerFromInt64(0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 - BigInteger 0", values.NewBigFloatFromFloat64(0.0), values.NewBigIntegerFromInt64(0), "BigFloat", false},

		// Rational (exact) - Float (inexact) → Float, via negation.
		{"Rational 0/1 - Float 0.0", values.NewRational(0, 1), values.NewFloat(0.0), "Float", false},
		{"Float 0.0 - Rational 0/1", values.NewFloat(0.0), values.NewRational(0, 1), "Float", false},

		// Rational (exact) - BigFloat (inexact) → BigFloat (negation preserves type).
		{"Rational 0/1 - BigFloat 0.0", values.NewRational(0, 1), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 - Rational 0/1", values.NewBigFloatFromFloat64(0.0), values.NewRational(0, 1), "BigFloat", false},

		// (- 0 0.0+0.0i) is -0.0-0.0i in Chez and Racket, and (real? -0.0-0.0i) is #f
		// per R7RS §6.2.6 — an INEXACT zero imaginary part does not make a number
		// real. Negating on the left keeps the value complex; it never reaches the
		// dispatch path that would demote it.
		{"Integer 0 - Complex 0+0i", values.NewInteger(0), values.NewComplex(0), "Complex", false},
		{"Complex 0+0i - Integer 0", values.NewComplex(0), values.NewInteger(0), "Complex", false},

		// Float (inexact) - Float (inexact) → Float (inexact)
		{"Float 0.0 - Float 0.0", values.NewFloat(0.0), values.NewFloat(0.0), "Float", false},

		// BigFloat (inexact) - BigFloat (inexact) → BigFloat (inexact)
		{"BigFloat 0.0 - BigFloat 0.0", values.NewBigFloatFromFloat64(0.0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := tc.a.Subtract(tc.b)
			c.Assert(result.IsExact(), qt.Equals, tc.isExact,
				qt.Commentf("Expected %s exactness to be %v, got %v", tc.name, tc.isExact, result.IsExact()))
			gotType := strings.TrimPrefix(fmt.Sprintf("%T", result), "*values.")
			c.Assert(gotType, qt.Equals, tc.wantType,
				qt.Commentf("exactness matched but the result type did not"))
		})
	}
}

// TestExactZeroSubtractNegates pins the VALUE of (- 0 x), which the contagion
// table above cannot see: it asserts only type and exactness, so it would pass
// just as happily on a +0.0 as on the correct -0.0.
//
// (- 0 x) is -x, sign bit and all. An exact 0 is a mathematical zero, not an
// IEEE +0.0, so the result is NOT governed by IEEE's 0.0 - 0.0 = +0.0 rule.
// Every want below is the verbatim output of both oracles:
//
//	petite -q <<< '(display (list (- 0 0.0) (- 0 -0.0) (- 0 0.0+0.0i)))'
//	racket   -e '(display (list (- 0 0.0) (- 0 -0.0) (- 0 0.0+0.0i)))'
//	# both => (-0.0 0.0 -0.0-0.0i)
func TestExactZeroSubtractNegates(t *testing.T) {
	tcs := []struct {
		name string
		a    values.Number
		b    values.Number
		want string
	}{
		// The sign flip is the whole point: +0.0 here would be an IEEE answer to
		// a question that is not an IEEE question.
		{"Integer 0 - Float 0.0", values.NewInteger(0), values.NewFloat(0.0), "-0.0"},
		// Negating a NEGATIVE zero returns to +0.0. Chez and Racket agree, though
		// here they agree with plain IEEE subtraction by coincidence, so this row
		// alone would not have caught the bug.
		{"Integer 0 - Float -0.0", values.NewInteger(0), values.NewFloat(math.Copysign(0, -1)), "0.0"},

		// The negation is carried by every exact kind, not just Integer.
		{"BigInteger 0 - Float 0.0", values.NewBigIntegerFromInt64(0), values.NewFloat(0.0), "-0.0"},
		{"Rational 0/1 - Float 0.0", values.NewRational(0, 1), values.NewFloat(0.0), "-0.0"},

		// Both components of a complex flip.
		{"Integer 0 - Complex 0+0i", values.NewInteger(0), values.NewComplex(0), "-0.0-0.0i"},

		// Non-zero operands: ordinary negation, exactness and type preserved.
		{"Integer 0 - Float 2.5", values.NewInteger(0), values.NewFloat(2.5), "-2.5"},
		{"Integer 0 - Integer 5", values.NewInteger(0), values.NewInteger(5), "-5"},
		{"Integer 0 - Rational 1/2", values.NewInteger(0), values.NewRational(1, 2), "-1/2"},

		// The negation reroutes ±inf and NaN away from dispatch arithmetic and
		// through Negate(), which flips the sign bit. NaN must stay +nan.0 (it has
		// no meaningful sign to report), and an infinity must flip. Both oracles:
		//	(- 0 +inf.0) => -inf.0    (- 0 -inf.0) => +inf.0    (- 0 +nan.0) => +nan.0
		{"Integer 0 - Float +inf.0", values.NewInteger(0), values.NewFloat(math.Inf(1)), "-inf.0"},
		{"Integer 0 - Float -inf.0", values.NewInteger(0), values.NewFloat(math.Inf(-1)), "+inf.0"},
		{"Integer 0 - Float +nan.0", values.NewInteger(0), values.NewFloat(math.NaN()), "+nan.0"},

		// The fourth exact kind: an exact-zero BigComplex receiver. This is the only
		// kind whose Subtract negation guard is otherwise untested, and it is
		// type-changing — it returns the OPERAND's kind, not a BigComplex.
		{
			"BigComplex 0+0i - Float 0.0",
			values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(0)),
			values.NewFloat(0.0),
			"-0.0",
		},
		{
			"BigComplex 0+0i - Integer 5",
			values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(0)),
			values.NewInteger(5),
			"-5",
		},

		// An exact zero on the RIGHT is the identity, NOT a negation: it returns
		// the operand untouched, so a -0.0 stays -0.0 rather than flipping.
		{"Float -0.0 - Integer 0", values.NewFloat(math.Copysign(0, -1)), values.NewInteger(0), "-0.0"},
		{"Float 0.0 - Integer 0", values.NewFloat(0.0), values.NewInteger(0), "0.0"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := tc.a.Subtract(tc.b)
			c.Assert(got.SchemeString(), qt.Equals, tc.want,
				qt.Commentf("(- %s %s): sign or magnitude of the negation is wrong",
					tc.a.SchemeString(), tc.b.SchemeString()))
		})
	}
}

// TestIEEE754SignedZeroPreservation validates that IEEE 754 signed zero
// semantics are preserved in addition and subtraction.
//
// IEEE 754 §6.3: "The sum of two operands with the same sign is the same
// as the sum of their absolute values with the same sign. The sum of two
// operands with opposite signs uses the sign of the operand with the larger
// absolute value."
//
// Key cases:
//
//	(+0.0) + (+0.0) → +0.0
//	(+0.0) + (-0.0) → +0.0
//	(-0.0) + (-0.0) → -0.0
//	(+0.0) - (+0.0) → +0.0
//	(+0.0) - (-0.0) → +0.0
//	(-0.0) - (+0.0) → -0.0
//	(-0.0) - (-0.0) → +0.0
func TestIEEE754SignedZeroPreservation(t *testing.T) {
	c := qt.New(t)

	posZero := values.NewFloat(+0.0)
	negZero := values.NewFloat(math.Copysign(0.0, -1.0)) // -0.0

	// Helper to check sign via math.Signbit
	isNegativeZero := func(n values.Number) bool {
		f, ok := n.(*values.Float)
		if !ok {
			return false
		}
		return f.Value == 0.0 && math.Signbit(f.Value)
	}

	isPositiveZero := func(n values.Number) bool {
		f, ok := n.(*values.Float)
		if !ok {
			return false
		}
		return f.Value == 0.0 && !math.Signbit(f.Value)
	}

	tcs := []struct {
		name    string
		a       values.Number
		b       values.Number
		op      func(values.Number, values.Number) values.Number
		wantPos bool // true = +0.0, false = -0.0
	}{
		// Addition
		{"(+0.0) + (+0.0)", posZero, posZero, func(a, b values.Number) values.Number {
			return a.Add(b)
		}, true},
		{"(+0.0) + (-0.0)", posZero, negZero, func(a, b values.Number) values.Number {
			return a.Add(b)
		}, true},
		{"(-0.0) + (-0.0)", negZero, negZero, func(a, b values.Number) values.Number {
			return a.Add(b)
		}, false},
		{"(-0.0) + (+0.0)", negZero, posZero, func(a, b values.Number) values.Number {
			return a.Add(b)
		}, true},

		// Subtraction
		{"(+0.0) - (+0.0)", posZero, posZero, func(a, b values.Number) values.Number {
			return a.Subtract(b)
		}, true},
		{"(+0.0) - (-0.0)", posZero, negZero, func(a, b values.Number) values.Number {
			return a.Subtract(b)
		}, true},
		{"(-0.0) - (+0.0)", negZero, posZero, func(a, b values.Number) values.Number {
			return a.Subtract(b)
		}, false},
		{"(-0.0) - (-0.0)", negZero, negZero, func(a, b values.Number) values.Number {
			return a.Subtract(b)
		}, true},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.op(tc.a, tc.b)
			if tc.wantPos {
				c.Assert(isPositiveZero(result), qt.IsTrue,
					qt.Commentf("Expected +0.0, got %v (signbit=%v)",
						result.SchemeString(), isNegativeZero(result)))
			} else {
				c.Assert(isNegativeZero(result), qt.IsTrue,
					qt.Commentf("Expected -0.0, got %v (signbit=%v)",
						result.SchemeString(), math.Signbit(result.(*values.Float).Value)))
			}
		})
	}
}

// TestMultiplicationExactZeroOptimization validates the exact-zero rule for
// multiplication: an EXACT zero annihilates the product unconditionally.
//
// R7RS §6.2.2: "Exact 0 is a number, and infinities and NaNs are numbers
// whose use in arithmetic should be restricted to those occasions where
// they are needed."
//
// The governing distinction is exact vs inexact, NOT finite vs infinite. An
// exact 0 is a mathematical zero, not an IEEE +0.0, so IEEE 754's 0*inf = NaN
// rule does not reach it. Chez Scheme and Racket agree on every case below,
// including (* 0 +inf.0) => 0 and (* 0 +nan.0) => 0.
//
// Why multiplication is different from addition/subtraction:
//
//	(* 0 x)   → exact 0 (the exact zero annihilates, whatever x is)
//	(+ 0 x)   → x (identity: preserves x's exactness AND its sign)
//	(- x 0)   → x (identity: preserves x's exactness AND its sign)
//
// An INEXACT zero does not short-circuit at all — IEEE 754 governs, so
// (* 5 0.0) => 0.0 and (* +inf.0 0.0) => +nan.0.
//
// CORRECTED 2026-07-12: cases 3–5 previously asserted (* 0 +inf.0) => +nan.0,
// encoding a false "the exact-zero rule requires a finite operand" invariant
// that Chez, Racket and R7RS all contradict. This test was what kept the bug
// green. See plans/2026-07-12-numeric-zero-and-tier2-fold.local.md.
func TestMultiplicationExactZeroOptimization(t *testing.T) {
	c := qt.New(t)

	// Case 1: Exact zero * finite inexact → exact zero.
	result := values.NewInteger(0).Multiply(values.NewFloat(42.5))
	c.Assert(result.IsZero(), qt.IsTrue)
	c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("(* 0 42.5) should be exact 0"))

	// Case 2: Inexact zero * exact → INEXACT zero. Contagion governs; the
	// inexact zero is an IEEE value and does not annihilate to an exact 0.
	result = values.NewFloat(0.0).Multiply(values.NewInteger(42))
	c.Assert(result.IsZero(), qt.IsTrue)
	c.Assert(result.IsExact(), qt.IsFalse, qt.Commentf("(* 0.0 42) should be inexact 0.0"))

	// Case 3: Exact zero * infinity → exact zero. The exact zero wins.
	result = values.NewInteger(0).Multiply(values.NewFloat(math.Inf(1)))
	c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("(* 0 +inf.0) should be exact 0"))
	c.Assert(result.IsZero(), qt.IsTrue)

	// Case 4: Exact zero * -infinity → exact zero.
	result = values.NewInteger(0).Multiply(values.NewFloat(math.Inf(-1)))
	c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("(* 0 -inf.0) should be exact 0"))
	c.Assert(result.IsZero(), qt.IsTrue)

	// Case 5: Exact zero * NaN → exact zero. The exact-zero rule outranks NaN.
	result = values.NewInteger(0).Multiply(values.NewFloat(math.NaN()))
	c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("(* 0 +nan.0) should be exact 0"))
	c.Assert(result.IsZero(), qt.IsTrue)

	// Case 6: INEXACT zero * infinity → NaN. IEEE 754 governs here, and only here.
	result = values.NewFloat(0.0).Multiply(values.NewFloat(math.Inf(1)))
	c.Assert(result.IsNaN(), qt.IsTrue, qt.Commentf("(* 0.0 +inf.0) should be +nan.0"))
}

// TestExactnessContagionWhyMultiplicationIsDifferent documents the fundamental
// difference between addition/subtraction and multiplication with zero.
//
// Addition/Subtraction: Identity operation
//
//	(+ 0 x) = x → result has same exactness as x
//	(- x 0) = x → result has same exactness as x
//	Zero short-circuit would lose exactness information.
//
// Multiplication: Absorbing element (for an EXACT zero, unconditionally)
//
//	(* 0 x) = 0 → result is exact zero, whatever x is — including inf and NaN.
//	Zero is the mathematical result, not a passthrough.
//	An exact-zero short-circuit therefore returns exact zero.
//
// This is why the architectural review finding (H3) only applies to
// Add/Subtract methods, not Multiply methods.
func TestExactnessContagionWhyMultiplicationIsDifferent(t *testing.T) {
	c := qt.New(t)

	// Demonstrate: addition preserves exactness of non-zero operand
	addResult := values.NewInteger(0).Add(values.NewFloat(0.0))
	c.Assert(addResult.IsExact(), qt.IsFalse,
		qt.Commentf("(+ 0 0.0) must be inexact because 0.0 is inexact"))

	// Demonstrate: subtraction preserves exactness of minuend
	subResult := values.NewFloat(0.0).Subtract(values.NewInteger(0))
	c.Assert(subResult.IsExact(), qt.IsFalse,
		qt.Commentf("(- 0.0 0) must be inexact because 0.0 is inexact"))

	// Demonstrate: multiplication can return exact zero
	mulResult := values.NewInteger(0).Multiply(values.NewFloat(42.5))
	c.Assert(mulResult.IsExact(), qt.IsTrue,
		qt.Commentf("(* 0 42.5) can be exact because result is mathematically 0"))
}

// TestContagionOverflowsToInfinity pins the most user-visible consequence of exactness
// contagion, and the one thing about it that looks like a bug and is not.
//
// Absorbing an exact operand into a float64 means ROUNDING it, and a bignum too large
// for float64 rounds to ±Inf. So a precise exact value, touched by a single inexact
// operand, becomes +inf.0. That is not a defect; it is what "inexact" promises, and it
// is what Chez gives:
//
//	petite -q <<< '(display (list (+ 1.5 (expt 2 2000)) (exact->inexact (expt 2 2000))))'
//	# => (+inf.0 +inf.0)
//
// This was untested. Three tests that used to exercise the lossy path were rewritten to
// route AROUND it (they now take a #m literal, which is the deliberate opt-in to
// arbitrary precision), which left the behavior itself pinned nowhere: contagion could
// have been reverted to the old exact × Float => BigFloat escalation and the suite
// would have stayed green, because a BigFloat compares = to the #m literals those tests
// assert against. These rows are the guard.
func TestContagionOverflowsToInfinity(t *testing.T) {
	c := qt.New(t)

	huge := new(big.Int).Exp(big.NewInt(2), big.NewInt(2000), nil)
	bigHuge := values.NewBigInteger(huge)

	// Exact + inexact: the exact operand is rounded into float64 and overflows.
	sum := bigHuge.Add(values.NewFloat(1.5))
	f, ok := sum.(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("contagion must land on *values.Float, got %T", sum))
	c.Assert(math.IsInf(f.Value, 1), qt.IsTrue,
		qt.Commentf("(+ 1.5 (expt 2 2000)) must be +inf.0, got %s", sum.SchemeString()))
	c.Assert(sum.IsExact(), qt.IsFalse)

	// Same the other way round: contagion is symmetric.
	sum2 := values.NewFloat(1.5).Add(bigHuge)
	c.Assert(sum2.SchemeString(), qt.Equals, sum.SchemeString(),
		qt.Commentf("contagion must not depend on operand order"))

	// Negative overflow.
	negHuge := values.NewBigInteger(new(big.Int).Neg(huge))
	neg := negHuge.Add(values.NewFloat(1.5))
	nf, ok := neg.(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsInf(nf.Value, -1), qt.IsTrue,
		qt.Commentf("must be -inf.0, got %s", neg.SchemeString()))

	// The #m literal is how a program ASKS to keep the precision, and it still works.
	// This is the escape hatch, and it is the reason the overflow above is acceptable.
	kept := bigHuge.Add(values.NewBigFloatFromFloat64(1.5))
	_, isBig := kept.(*values.BigFloat)
	c.Assert(isBig, qt.IsTrue, qt.Commentf("a BigFloat operand must preserve precision, got %T", kept))
	c.Assert(kept.IsExact(), qt.IsFalse)
	bf := kept.(*values.BigFloat)
	c.Assert(bf.IsFinite(), qt.IsTrue,
		qt.Commentf("BigFloat has the range to hold 2^2000; it must not overflow"))
}

// TestComparisonNeverRoundsAnOperand pins TRICHOTOMY across the exact/inexact boundary,
// including against infinities.
//
// For any two real numbers exactly one of a<b, a=b, a>b holds. That is not negotiable,
// and it was violated: the comparison dispatchers carried an IEEE special-value guard
// that, when either operand was an Inf/NaN Float, short-circuited BOTH operands through
// float64. An exact bignum too large for float64 became +Inf, and then compared EQUAL to
// the infinity it was being compared against:
//
//	(<  (expt 10 400) +inf.0)   =>  #f
//	(>= (expt 10 400) +inf.0)   =>  #t      -- simultaneously
//
// The guard's justification (that BigFloat/BigComplex could not hold Inf/NaN) went stale
// when they were made IEEE-capable. Rounding an operand is the one thing comparison must
// never do; it is why comparisonTable exists at all.
func TestComparisonNeverRoundsAnOperand(t *testing.T) {
	c := qt.New(t)

	huge := values.NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil))
	negHuge := values.NewBigInteger(new(big.Int).Neg(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)))
	posInf := values.NewFloat(math.Inf(1))
	negInf := values.NewFloat(math.Inf(-1))

	// A finite exact number, however large, is strictly less than +inf.0.
	c.Assert(huge.LessThan(posInf), qt.IsTrue, qt.Commentf("10^400 < +inf.0"))
	c.Assert(huge.Compare(posInf), qt.Equals, -1)
	c.Assert(posInf.LessThan(huge), qt.IsFalse, qt.Commentf("+inf.0 is not < 10^400"))
	c.Assert(posInf.Compare(huge), qt.Equals, 1)

	// And strictly greater than -inf.0.
	c.Assert(negInf.LessThan(huge), qt.IsTrue)
	c.Assert(huge.LessThan(negInf), qt.IsFalse)
	c.Assert(negHuge.LessThan(posInf), qt.IsTrue)
	c.Assert(negInf.LessThan(negHuge), qt.IsTrue, qt.Commentf("-inf.0 < -10^400"))

	// TRICHOTOMY, stated as the law rather than as cases: for every ordered pair,
	// exactly one of <, =, > holds. The old guard made (10^400, +inf.0) satisfy NONE of
	// the strict relations while also reporting Compare == 0.
	pairs := []struct {
		name string
		a, b values.Number
	}{
		{"10^400 vs +inf.0", huge, posInf},
		{"10^400 vs -inf.0", huge, negInf},
		{"-10^400 vs +inf.0", negHuge, posInf},
		{"+inf.0 vs 10^400", posInf, huge},
		{"2^100-1 vs (inexact 2^100)", values.NewBigInteger(
			new(big.Int).Sub(new(big.Int).Exp(big.NewInt(2), big.NewInt(100), nil), big.NewInt(1))),
			values.NewFloat(math.Pow(2, 100))},
	}
	for _, tc := range pairs {
		lt := tc.a.LessThan(tc.b)
		gt := tc.b.LessThan(tc.a)
		eq := values.NumericEquals(tc.a, tc.b)
		n := 0
		for _, b := range []bool{lt, gt, eq} {
			if b {
				n++
			}
		}
		c.Assert(n, qt.Equals, 1, qt.Commentf(
			"trichotomy violated for %s: less=%v greater=%v equal=%v (exactly one must hold)",
			tc.name, lt, gt, eq))

		// Compare must agree with LessThan rather than contradict it.
		cmp := tc.a.Compare(tc.b)
		switch {
		case lt:
			c.Assert(cmp, qt.Equals, -1, qt.Commentf("%s: LessThan says <, Compare disagrees", tc.name))
		case gt:
			c.Assert(cmp, qt.Equals, 1, qt.Commentf("%s: LessThan says >, Compare disagrees", tc.name))
		default:
			c.Assert(cmp, qt.Equals, 0, qt.Commentf("%s: equal, Compare disagrees", tc.name))
		}
	}
}

// TestEqvAgreesWithNumericEqualsOnExactComplex closes an R7RS §6.1 lattice violation:
// two EXACT numbers that are = must be eqv?.
//
// An exact BigComplex with an exact-ZERO imaginary part is a real number. `=` said so
// (NumericEquals compared it to Integer 1 and answered #t) while eqv? rejected any
// complex-vs-real pair outright and answered #f. Two exact numbers, numerically equal,
// not eqv? — exactly the disagreement between the predicates that R7RS forbids and that
// values/eqv.go was written to make impossible.
//
// Scheme cannot construct such a value — make-rectangular canonicalizes, so
// (make-rectangular 1 0) evaluates to 1 — but values.NewBigComplex is PUBLIC API and does
// not canonicalize. EqvNumber calls itself the single authority on numeric equivalence,
// so it does not get to lean on an invariant it neither owns nor states.
func TestEqvAgreesWithNumericEqualsOnExactComplex(t *testing.T) {
	c := qt.New(t)

	one := values.NewInteger(1)
	exactOnePlusZeroI := values.NewBigComplex(
		values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(0))

	// The law: for EXACT operands, = and eqv? must agree.
	c.Assert(values.NumericEquals(exactOnePlusZeroI, one), qt.IsTrue)
	c.Assert(values.EqvNumber(exactOnePlusZeroI, one), qt.IsTrue,
		qt.Commentf("exact 1+0i IS the exact number 1; R7RS 6.1 requires eqv? to agree with ="))
	c.Assert(values.EqvNumber(one, exactOnePlusZeroI), qt.IsTrue,
		qt.Commentf("and it must not depend on operand order"))

	// INEXACT does not collapse, and must not. 1.0+0.0i has an imaginary part that is an
	// inexact zero, not an absent one, so it is a distinct object from 1.0.
	// Chez: (eqv? 1.0 1.0+0.0i) => #f.
	inexactOnePlusZeroI := values.NewComplex(complex(1, 0))
	c.Assert(values.EqvNumber(inexactOnePlusZeroI, values.NewFloat(1.0)), qt.IsFalse,
		qt.Commentf("an INEXACT complex with a 0.0 imaginary part is not eqv? to the real"))

	// A nonzero imaginary part is still not a real number, exact or not.
	c.Assert(values.EqvNumber(
		values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
		one), qt.IsFalse)
}
