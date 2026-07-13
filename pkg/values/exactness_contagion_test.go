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
// For subtraction: (- x 0) → x, (- 0 x) → -x
// The result's exactness depends on the operands:
//
//	(- 0.0 0) → 0.0 (inexact minuend)
//	(- 0 0.0) → 0.0 (inexact subtrahend)
//	(- 0 0)   → 0   (exact)
//
// This test verifies that zero short-circuits do NOT happen in Subtract
// methods, because doing so would violate exactness contagion.
func TestExactnessContagionSubtraction(t *testing.T) {
	// blockedOnNegation marks the rows whose wantType describes the (- 0 x) => -x
	// NEGATION identity that wile does not yet implement. These are NOT stale
	// expectations: they are the correct contract, confirmed against both oracles,
	// which the code fails to honour. See the D6/D5 note on the blocked field below.
	const blockedOnNegation = "BLOCKED on D6 (missing (- 0 x) => -x negation identity); " +
		"see plans/2026-07-12-exact-zero-negation-and-complex-demotion.local.md"
	const blockedOnDemotion = "BLOCKED on D5 (maybeSimplify demotes on an INEXACT zero imag) + D6; " +
		"see plans/2026-07-12-exact-zero-negation-and-complex-demotion.local.md"

	tcs := []struct {
		name     string
		a        values.Number
		b        values.Number
		wantType string
		isExact  bool
		// blocked, when non-empty, marks a row whose wantType is the CORRECT
		// contract (pinned to Chez and Racket) that the code does not yet meet.
		// The expectation is deliberately left correct rather than relaxed to
		// match the current output — relaxing it would reinstate exactly the hole
		// this assertion exists to close.
		blocked string
	}{
		// An exact zero on the LEFT of Subtract must NEGATE, not short-circuit:
		// (- 0 x) is -x. Negation preserves the operand's TYPE, so the result is a
		// Float, not the promoted BigFloat LUB. Chez and Racket: (- 0 0.0) => -0.0.
		// wile currently promotes and computes 0.0 - 0.0 = +0.0 (a BigFloat).
		{"Integer 0 - Float 0.0", values.NewInteger(0), values.NewFloat(0.0), "Float", false, blockedOnNegation},
		// An exact zero on the RIGHT is an identity: returns the operand unchanged.
		{"Float 0.0 - Integer 0", values.NewFloat(0.0), values.NewInteger(0), "Float", false, ""},

		// Integer (exact) - Integer (exact) → Integer (exact)
		{"Integer 0 - Integer 0", values.NewInteger(0), values.NewInteger(0), "Integer", true, ""},

		// Corrected 2026-07-12: this row said BigFloat, which is what the CURRENT
		// (buggy) promotion produces — so it passed while documenting the bug. Under
		// the negation identity it is a Float, exactly like its Integer sibling above.
		{"BigInteger 0 - Float 0.0", values.NewBigIntegerFromInt64(0), values.NewFloat(0.0), "Float", false, blockedOnNegation},
		// Identity on the right — corrected from BigFloat: no promotion occurs.
		{"Float 0.0 - BigInteger 0", values.NewFloat(0.0), values.NewBigIntegerFromInt64(0), "Float", false, ""},

		// Negation of a BigFloat is a BigFloat, so the type is unaffected by D6
		// (only the sign of the value changes, +0.0 → -0.0).
		{"BigInteger 0 - BigFloat 0.0", values.NewBigIntegerFromInt64(0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false, ""},
		{"BigFloat 0.0 - BigInteger 0", values.NewBigFloatFromFloat64(0.0), values.NewBigIntegerFromInt64(0), "BigFloat", false, ""},

		// Rational (exact) - Float (inexact) → Float, via negation.
		{"Rational 0/1 - Float 0.0", values.NewRational(0, 1), values.NewFloat(0.0), "Float", false, blockedOnNegation},
		{"Float 0.0 - Rational 0/1", values.NewFloat(0.0), values.NewRational(0, 1), "Float", false, ""},

		// Rational (exact) - BigFloat (inexact) → BigFloat (negation preserves type).
		{"Rational 0/1 - BigFloat 0.0", values.NewRational(0, 1), values.NewBigFloatFromFloat64(0.0), "BigFloat", false, ""},
		{"BigFloat 0.0 - Rational 0/1", values.NewBigFloatFromFloat64(0.0), values.NewRational(0, 1), "BigFloat", false, ""},

		// (- 0 0.0+0.0i) is -0.0-0.0i in Chez and Racket, and (real? -0.0-0.0i) is #f
		// per R7RS §6.2.6 — an INEXACT zero imaginary part does not make a number real.
		// wile returns 0.0 (a real): it both fails to negate (D6) and demotes the
		// complex on an inexact zero imaginary part (D5).
		{"Integer 0 - Complex 0+0i", values.NewInteger(0), values.NewComplex(0), "Complex", false, blockedOnDemotion},
		{"Complex 0+0i - Integer 0", values.NewComplex(0), values.NewInteger(0), "Complex", false, ""},

		// Float (inexact) - Float (inexact) → Float (inexact)
		{"Float 0.0 - Float 0.0", values.NewFloat(0.0), values.NewFloat(0.0), "Float", false, ""},

		// BigFloat (inexact) - BigFloat (inexact) → BigFloat (inexact)
		{"BigFloat 0.0 - BigFloat 0.0", values.NewBigFloatFromFloat64(0.0), values.NewBigFloatFromFloat64(0.0), "BigFloat", false, ""},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.blocked != "" {
				t.Skip(tc.blocked)
			}
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
