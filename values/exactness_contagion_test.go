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
	"testing"

	qt "github.com/frankban/quicktest"
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
	c := qt.New(t)

	// Test case structure: exact zero + inexact zero = inexact zero
	// This is the critical case that detects improper zero short-circuits.
	tcs := []struct {
		name     string
		a        Number
		b        Number
		wantType string
		isExact  bool
	}{
		// Integer (exact) + Float (inexact) → Float (inexact)
		{"Integer 0 + Float 0.0", NewInteger(0), NewFloat(0.0), "Float", false},
		{"Float 0.0 + Integer 0", NewFloat(0.0), NewInteger(0), "Float", false},

		// Integer (exact) + Integer (exact) → Integer (exact)
		{"Integer 0 + Integer 0", NewInteger(0), NewInteger(0), "Integer", true},

		// BigInteger (exact) + Float (inexact) → Float (inexact)
		{"BigInteger 0 + Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "Float", false},
		{"Float 0.0 + BigInteger 0", NewFloat(0.0), NewBigIntegerFromInt64(0), "Float", false},

		// BigInteger (exact) + BigFloat (inexact) → BigFloat (inexact)
		{"BigInteger 0 + BigFloat 0.0", NewBigIntegerFromInt64(0), NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 + BigInteger 0", NewBigFloatFromFloat64(0.0), NewBigIntegerFromInt64(0), "BigFloat", false},

		// Rational (exact) + Float (inexact) → Float (inexact)
		{"Rational 0/1 + Float 0.0", NewRational(0, 1), NewFloat(0.0), "Float", false},
		{"Float 0.0 + Rational 0/1", NewFloat(0.0), NewRational(0, 1), "Float", false},

		// Rational (exact) + BigFloat (inexact) → BigFloat (inexact)
		{"Rational 0/1 + BigFloat 0.0", NewRational(0, 1), NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 + Rational 0/1", NewBigFloatFromFloat64(0.0), NewRational(0, 1), "BigFloat", false},

		// Integer (exact) + Complex (inexact) → Complex (inexact)
		{"Integer 0 + Complex 0+0i", NewInteger(0), NewComplex(0), "Complex", false},
		{"Complex 0+0i + Integer 0", NewComplex(0), NewInteger(0), "Complex", false},

		// BigInteger (exact) + BigComplex (can be exact or inexact depending on parts)
		{"BigInteger 0 + BigComplex(inexact)", NewBigIntegerFromInt64(0),
			NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), NewBigFloatFromFloat64(0)),
			"BigComplex", false},

		// Float (inexact) + Float (inexact) → Float (inexact)
		{"Float 0.0 + Float 0.0", NewFloat(0.0), NewFloat(0.0), "Float", false},

		// BigFloat (inexact) + BigFloat (inexact) → BigFloat (inexact)
		{"BigFloat 0.0 + BigFloat 0.0", NewBigFloatFromFloat64(0.0), NewBigFloatFromFloat64(0.0), "BigFloat", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.a.Add(tc.b)
			c.Assert(result.IsExact(), qt.Equals, tc.isExact,
				qt.Commentf("Expected %s exactness to be %v, got %v", tc.name, tc.isExact, result.IsExact()))
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
	c := qt.New(t)

	tcs := []struct {
		name     string
		a        Number
		b        Number
		wantType string
		isExact  bool
	}{
		// Integer (exact) - Float (inexact) → Float (inexact)
		{"Integer 0 - Float 0.0", NewInteger(0), NewFloat(0.0), "Float", false},
		{"Float 0.0 - Integer 0", NewFloat(0.0), NewInteger(0), "Float", false},

		// Integer (exact) - Integer (exact) → Integer (exact)
		{"Integer 0 - Integer 0", NewInteger(0), NewInteger(0), "Integer", true},

		// BigInteger (exact) - Float (inexact) → Float (inexact)
		{"BigInteger 0 - Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "Float", false},
		{"Float 0.0 - BigInteger 0", NewFloat(0.0), NewBigIntegerFromInt64(0), "Float", false},

		// BigInteger (exact) - BigFloat (inexact) → BigFloat (inexact)
		{"BigInteger 0 - BigFloat 0.0", NewBigIntegerFromInt64(0), NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 - BigInteger 0", NewBigFloatFromFloat64(0.0), NewBigIntegerFromInt64(0), "BigFloat", false},

		// Rational (exact) - Float (inexact) → Float (inexact)
		{"Rational 0/1 - Float 0.0", NewRational(0, 1), NewFloat(0.0), "Float", false},
		{"Float 0.0 - Rational 0/1", NewFloat(0.0), NewRational(0, 1), "Float", false},

		// Rational (exact) - BigFloat (inexact) → BigFloat (inexact)
		{"Rational 0/1 - BigFloat 0.0", NewRational(0, 1), NewBigFloatFromFloat64(0.0), "BigFloat", false},
		{"BigFloat 0.0 - Rational 0/1", NewBigFloatFromFloat64(0.0), NewRational(0, 1), "BigFloat", false},

		// Integer (exact) - Complex (inexact) → Complex (inexact)
		{"Integer 0 - Complex 0+0i", NewInteger(0), NewComplex(0), "Complex", false},
		{"Complex 0+0i - Integer 0", NewComplex(0), NewInteger(0), "Complex", false},

		// Float (inexact) - Float (inexact) → Float (inexact)
		{"Float 0.0 - Float 0.0", NewFloat(0.0), NewFloat(0.0), "Float", false},

		// BigFloat (inexact) - BigFloat (inexact) → BigFloat (inexact)
		{"BigFloat 0.0 - BigFloat 0.0", NewBigFloatFromFloat64(0.0), NewBigFloatFromFloat64(0.0), "BigFloat", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := tc.a.Subtract(tc.b)
			c.Assert(result.IsExact(), qt.Equals, tc.isExact,
				qt.Commentf("Expected %s exactness to be %v, got %v", tc.name, tc.isExact, result.IsExact()))
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

	posZero := NewFloat(+0.0)
	negZero := NewFloat(math.Copysign(0.0, -1.0)) // -0.0

	// Helper to check sign via math.Signbit
	isNegativeZero := func(n Number) bool {
		f, ok := n.(*Float)
		if !ok {
			return false
		}
		return f.Value == 0.0 && math.Signbit(f.Value)
	}

	isPositiveZero := func(n Number) bool {
		f, ok := n.(*Float)
		if !ok {
			return false
		}
		return f.Value == 0.0 && !math.Signbit(f.Value)
	}

	tcs := []struct {
		name    string
		a       Number
		b       Number
		op      func(Number, Number) Number
		wantPos bool // true = +0.0, false = -0.0
	}{
		// Addition
		{"(+0.0) + (+0.0)", posZero, posZero, func(a, b Number) Number {
			return a.Add(b)
		}, true},
		{"(+0.0) + (-0.0)", posZero, negZero, func(a, b Number) Number {
			return a.Add(b)
		}, true},
		{"(-0.0) + (-0.0)", negZero, negZero, func(a, b Number) Number {
			return a.Add(b)
		}, false},
		{"(-0.0) + (+0.0)", negZero, posZero, func(a, b Number) Number {
			return a.Add(b)
		}, true},

		// Subtraction
		{"(+0.0) - (+0.0)", posZero, posZero, func(a, b Number) Number {
			return a.Subtract(b)
		}, true},
		{"(+0.0) - (-0.0)", posZero, negZero, func(a, b Number) Number {
			return a.Subtract(b)
		}, true},
		{"(-0.0) - (+0.0)", negZero, posZero, func(a, b Number) Number {
			return a.Subtract(b)
		}, false},
		{"(-0.0) - (-0.0)", negZero, negZero, func(a, b Number) Number {
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
						result.SchemeString(), math.Signbit(result.(*Float).Value)))
			}
		})
	}
}

// TestMultiplicationExactZeroOptimization validates that multiplication
// can safely use exact zero optimization (Dybvig's approach from Chez Scheme).
//
// R7RS §6.2.2: "Exact 0 is a number, and infinities and NaNs are numbers
// whose use in arithmetic should be restricted to those occasions where
// they are needed."
//
// Chez Scheme (and other implementations) optimize (* 0 x) → 0 (exact)
// when x is finite, because the mathematical result is exact zero regardless
// of x's exactness.
//
// Why multiplication is different from addition/subtraction:
//
//	(* 0 x)   → 0 (always exact zero when x is finite)
//	(+ 0 x)   → x (exactness depends on x — cannot optimize)
//	(- x 0)   → x (exactness depends on x — cannot optimize)
//
// The key difference: multiplication by zero yields an exact mathematical
// result (zero), while addition/subtraction of zero is the identity operation
// (returns the other operand unchanged, preserving its exactness).
//
// This test documents the exception: multiplication CAN return exact zero
// even when one operand is inexact, but ONLY for finite operands.
// Special cases:
//
//	(* 0 +inf.0) → +nan.0 (not zero)
//	(* 0 -inf.0) → +nan.0 (not zero)
//	(* 0 +nan.0) → +nan.0 (not zero)
func TestMultiplicationExactZeroOptimization(t *testing.T) {
	c := qt.New(t)

	// Case 1: Exact zero * finite inexact → exact zero
	// This is the optimization: mathematically unambiguous result
	result := NewInteger(0).Multiply(NewFloat(42.5))
	c.Assert(result.IsZero(), qt.IsTrue)
	c.Assert(result.IsExact(), qt.IsTrue, qt.Commentf("(* 0 42.5) should be exact 0"))

	// Case 2: Inexact zero * finite exact → depends on implementation
	// Some implementations return exact 0, others return inexact 0
	result = NewFloat(0.0).Multiply(NewInteger(42))
	c.Assert(result.IsZero(), qt.IsTrue)
	// No exactness assertion here — both behaviors are acceptable per R7RS

	// Case 3: Exact zero * infinity → NaN (NOT zero)
	result = NewInteger(0).Multiply(NewFloat(math.Inf(1)))
	c.Assert(result.IsNaN(), qt.IsTrue, qt.Commentf("(* 0 +inf.0) should be +nan.0"))

	// Case 4: Exact zero * -infinity → NaN (NOT zero)
	result = NewInteger(0).Multiply(NewFloat(math.Inf(-1)))
	c.Assert(result.IsNaN(), qt.IsTrue, qt.Commentf("(* 0 -inf.0) should be +nan.0"))

	// Case 5: Exact zero * NaN → NaN (NOT zero)
	result = NewInteger(0).Multiply(NewFloat(math.NaN()))
	c.Assert(result.IsNaN(), qt.IsTrue, qt.Commentf("(* 0 +nan.0) should be +nan.0"))
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
// Multiplication: Absorbing element (for finite operands)
//
//	(* 0 x) = 0 → result is exact zero (when x is finite)
//	Zero is the mathematical result, not a passthrough.
//	Zero short-circuit can return exact zero.
//
// This is why the architectural review finding (H3) only applies to
// Add/Subtract methods, not Multiply methods.
func TestExactnessContagionWhyMultiplicationIsDifferent(t *testing.T) {
	c := qt.New(t)

	// Demonstrate: addition preserves exactness of non-zero operand
	addResult := NewInteger(0).Add(NewFloat(0.0))
	c.Assert(addResult.IsExact(), qt.IsFalse,
		qt.Commentf("(+ 0 0.0) must be inexact because 0.0 is inexact"))

	// Demonstrate: subtraction preserves exactness of minuend
	subResult := NewFloat(0.0).Subtract(NewInteger(0))
	c.Assert(subResult.IsExact(), qt.IsFalse,
		qt.Commentf("(- 0.0 0) must be inexact because 0.0 is inexact"))

	// Demonstrate: multiplication can return exact zero
	mulResult := NewInteger(0).Multiply(NewFloat(42.5))
	c.Assert(mulResult.IsExact(), qt.IsTrue,
		qt.Commentf("(* 0 42.5) can be exact because result is mathematically 0"))
}
