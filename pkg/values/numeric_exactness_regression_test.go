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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// TestMultiply_ExactZeroDominates verifies R7RS §6.2.2 exact zero behavior.
//
// R7RS permits (* 0 x) to return exact 0 even when x is inexact.
// This project follows Chez Scheme: exact zero always dominates.
// If either operand is an exact zero, the result is exact zero.
// If both operands are inexact, the result is inexact zero.
func TestMultiply_ExactZeroDominates(t *testing.T) {
	exactZero := values.NewInteger(0)

	tcs := []struct {
		nm  string
		in0 values.Number
		in1 values.Number
		out values.Number
	}{
		// Exact zero (Integer) × inexact → exact zero
		{
			nm:  "Integer(0) * Float(5.0)",
			in0: values.NewInteger(0),
			in1: values.NewFloat(5.0),
			out: exactZero,
		},
		{
			nm:  "Float(5.0) * Integer(0)",
			in0: values.NewFloat(5.0),
			in1: values.NewInteger(0),
			out: exactZero,
		},
		{
			nm:  "Integer(0) * BigFloat(5.0)",
			in0: values.NewInteger(0),
			in1: values.NewBigFloatFromFloat64(5.0),
			out: exactZero,
		},
		{
			nm:  "BigFloat(5.0) * Integer(0)",
			in0: values.NewBigFloatFromFloat64(5.0),
			in1: values.NewInteger(0),
			out: exactZero,
		},
		{
			nm:  "Integer(0) * Complex(1+2i)",
			in0: values.NewInteger(0),
			in1: values.NewComplex(complex(1, 2)),
			out: exactZero,
		},
		{
			nm:  "Complex(1+2i) * Integer(0)",
			in0: values.NewComplex(complex(1, 2)),
			in1: values.NewInteger(0),
			out: exactZero,
		},
		// Exact zero (BigInteger) × inexact → exact zero
		{
			nm:  "BigInteger(0) * Float(5.0)",
			in0: values.NewBigIntegerFromInt64(0),
			in1: values.NewFloat(5.0),
			out: exactZero,
		},
		{
			nm:  "Float(5.0) * BigInteger(0)",
			in0: values.NewFloat(5.0),
			in1: values.NewBigIntegerFromInt64(0),
			out: exactZero,
		},
		// Exact zero (Rational) × inexact → exact zero
		{
			nm:  "Rational(0) * Float(5.0)",
			in0: values.NewRational(0, 1),
			in1: values.NewFloat(5.0),
			out: exactZero,
		},
		{
			nm:  "Float(5.0) * Rational(0)",
			in0: values.NewFloat(5.0),
			in1: values.NewRational(0, 1),
			out: exactZero,
		},
		// Exact × exact → exact zero
		{
			nm:  "Integer(0) * Integer(5)",
			in0: values.NewInteger(0),
			in1: values.NewInteger(5),
			out: exactZero,
		},
		{
			nm:  "Integer(5) * Integer(0)",
			in0: values.NewInteger(5),
			in1: values.NewInteger(0),
			out: exactZero,
		},
		{
			nm:  "BigInteger(0) * Rational(1/3)",
			in0: values.NewBigIntegerFromInt64(0),
			in1: values.NewRational(1, 3),
			out: exactZero,
		},
		{
			nm:  "Rational(1/3) * Integer(0)",
			in0: values.NewRational(1, 3),
			in1: values.NewInteger(0),
			out: exactZero,
		},
		// Inexact × inexact → inexact zero (contagion preserved)
		{
			nm:  "Float(0.0) * Float(5.0)",
			in0: values.NewFloat(0.0),
			in1: values.NewFloat(5.0),
			out: values.NewFloat(0.0),
		},
		{
			nm:  "Float(5.0) * Float(0.0)",
			in0: values.NewFloat(5.0),
			in1: values.NewFloat(0.0),
			out: values.NewFloat(0.0),
		},
		{
			nm:  "Complex(0+0i) * Complex(1+2i)",
			in0: values.NewComplex(complex(0, 0)),
			in1: values.NewComplex(complex(1, 2)),
			out: values.NewComplex(complex(0, 0)),
		},
		{
			nm:  "BigFloat(0.0) * BigFloat(5.0)",
			in0: values.NewBigFloatFromFloat64(0.0),
			in1: values.NewBigFloatFromFloat64(5.0),
			out: values.NewBigFloatFromFloat64(0.0),
		},
		// Cross-type: exact zero × BigComplex → exact zero
		{
			nm:  "Integer(0) * BigComplex(1+2i)",
			in0: values.NewInteger(0),
			in1: values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1.0), values.NewBigFloatFromFloat64(2.0)),
			out: exactZero,
		},
		{
			nm:  "BigComplex(1+2i) * Integer(0)",
			in0: values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1.0), values.NewBigFloatFromFloat64(2.0)),
			in1: values.NewInteger(0),
			out: exactZero,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.nm, func(t *testing.T) {
			result := tc.in0.Multiply(tc.in1)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
			// Verify exactness is correct
			qt.Assert(t, result.IsExact(), qt.Equals, tc.out.IsExact(),
				qt.Commentf("exactness mismatch: got %T (exact=%v), want %T (exact=%v)",
					result, result.IsExact(), tc.out, tc.out.IsExact()))
		})
	}
}

// TestMultiply_ExactZeroCommutativity verifies that multiplication by zero
// is commutative in both value and exactness.
func TestMultiply_ExactZeroCommutativity(t *testing.T) {
	tcs := []struct {
		nm string
		a  values.Number
		b  values.Number
	}{
		{"Integer(0) * Float(5.0)", values.NewInteger(0), values.NewFloat(5.0)},
		{"Integer(0) * BigFloat(5.0)", values.NewInteger(0), values.NewBigFloatFromFloat64(5.0)},
		{"Integer(0) * Complex(1+2i)", values.NewInteger(0), values.NewComplex(complex(1, 2))},
		{"BigInteger(0) * Float(5.0)", values.NewBigIntegerFromInt64(0), values.NewFloat(5.0)},
		{"Rational(0) * Float(5.0)", values.NewRational(0, 1), values.NewFloat(5.0)},
		{"Float(0.0) * Float(5.0)", values.NewFloat(0.0), values.NewFloat(5.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.nm, func(t *testing.T) {
			ab := tc.a.Multiply(tc.b)
			ba := tc.b.Multiply(tc.a)
			qt.Assert(t, ab, valuestest.SchemeEquals, ba,
				qt.Commentf("a*b=%v (%T), b*a=%v (%T)", ab, ab, ba, ba))
			qt.Assert(t, ab.IsExact(), qt.Equals, ba.IsExact(),
				qt.Commentf("exactness differs: a*b exact=%v, b*a exact=%v",
					ab.IsExact(), ba.IsExact()))
		})
	}
}

// TestEqualTo_NumericSymmetry verifies that EqualTo is symmetric across
// all cross-type numeric pairs that we handle.
func TestEqualTo_NumericSymmetry(t *testing.T) {
	tcs := []struct {
		nm  string
		in0 values.Value
		in1 values.Value
		out bool
	}{
		// Float ↔ BigFloat: NOT equal, and symmetrically so.
		//
		// Both are inexact, and for inexact numbers the representation is observable:
		// (+ x 1e-20) distinguishes a float64 from a 256-bit BigFloat, which is exactly
		// R7RS 6.1's "yield the same results under any finite composition of standard
		// arithmetic" test. So eqv? says #f, and equal? must say the same (6.1 gives no
		// latitude on numbers).
		//
		// These two rows asserted `true` until exactness contagion was fixed. Note the
		// EXACT cross-kind rows below still assert `true` and must keep doing so — for
		// exact numbers the representation is NOT observable, so Integer 5, BigInteger 5
		// and Rational 10/2 are all the same number. Same code shape, opposite verdicts;
		// exactness is what discriminates. That asymmetry is the whole subtlety of
		// values.EqvNumber.
		{
			nm:  "Float(3.0) != BigFloat(3.0)",
			in0: values.NewFloat(3.0),
			in1: values.NewBigFloatFromFloat64(3.0),
			out: false,
		},
		{
			nm:  "BigFloat(3.0) != Float(3.0)",
			in0: values.NewBigFloatFromFloat64(3.0),
			in1: values.NewFloat(3.0),
			out: false,
		},
		{
			nm:  "Float(3.0) == BigFloat(4.0)",
			in0: values.NewFloat(3.0),
			in1: values.NewBigFloatFromFloat64(4.0),
			out: false,
		},
		{
			nm:  "BigFloat(4.0) == Float(3.0)",
			in0: values.NewBigFloatFromFloat64(4.0),
			in1: values.NewFloat(3.0),
			out: false,
		},
		// Rational ↔ Integer
		{
			nm:  "Rational(10/2) == Integer(5)",
			in0: values.NewRational(10, 2),
			in1: values.NewInteger(5),
			out: true,
		},
		{
			nm:  "Integer(5) == Rational(10/2)",
			in0: values.NewInteger(5),
			in1: values.NewRational(10, 2),
			out: true,
		},
		{
			nm:  "Rational(1/3) == Integer(0)",
			in0: values.NewRational(1, 3),
			in1: values.NewInteger(0),
			out: false,
		},
		// Rational ↔ BigInteger
		{
			nm:  "Rational(14/2) == BigInteger(7)",
			in0: values.NewRational(14, 2),
			in1: values.NewBigIntegerFromInt64(7),
			out: true,
		},
		{
			nm:  "BigInteger(7) == Rational(14/2)",
			in0: values.NewBigIntegerFromInt64(7),
			in1: values.NewRational(14, 2),
			out: true,
		},
		// Integer ↔ BigInteger (pre-existing, verify still works)
		{
			nm:  "Integer(42) == BigInteger(42)",
			in0: values.NewInteger(42),
			in1: values.NewBigIntegerFromInt64(42),
			out: true,
		},
		{
			nm:  "BigInteger(42) == Integer(42)",
			in0: values.NewBigIntegerFromInt64(42),
			in1: values.NewInteger(42),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.nm, func(t *testing.T) {
			forward := tc.in0.EqualTo(tc.in1)
			reverse := tc.in1.EqualTo(tc.in0)
			qt.Assert(t, forward, qt.Equals, tc.out)
			qt.Assert(t, reverse, qt.Equals, tc.out,
				qt.Commentf("asymmetric: %T.EqualTo(%T)=%v but %T.EqualTo(%T)=%v",
					tc.in0, tc.in1, forward, tc.in1, tc.in0, reverse))
		})
	}
}

// TestPair_EqualTo_CircularList verifies that Pair.EqualTo terminates
// and returns correct results for circular lists.
func TestPair_EqualTo_CircularList(t *testing.T) {
	t.Run("identical circular lists are equal", func(t *testing.T) {
		// Build circular list: (1 2 3 1 2 3 ...)
		a := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2),
				values.NewCons(values.NewInteger(3), values.EmptyList)))
		// Make it circular: last cdr points back to head
		a[1].(*values.Pair)[1].(*values.Pair)[1] = a

		b := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2),
				values.NewCons(values.NewInteger(3), values.EmptyList)))
		b[1].(*values.Pair)[1].(*values.Pair)[1] = b

		qt.Assert(t, a.EqualTo(b), qt.IsTrue)
	})

	t.Run("different circular lists are not equal", func(t *testing.T) {
		a := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2), values.EmptyList))
		a[1].(*values.Pair)[1] = a

		b := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(99), values.EmptyList))
		b[1].(*values.Pair)[1] = b

		qt.Assert(t, a.EqualTo(b), qt.IsFalse)
	})

	t.Run("self-referential pair is equal to itself", func(t *testing.T) {
		a := values.NewCons(values.NewInteger(1), values.EmptyList)
		a[1] = a

		qt.Assert(t, a.EqualTo(a), qt.IsTrue)
	})

	t.Run("top-level EqualTo also handles circular lists", func(t *testing.T) {
		a := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2), values.EmptyList))
		a[1].(*values.Pair)[1] = a

		b := values.NewCons(values.NewInteger(1),
			values.NewCons(values.NewInteger(2), values.EmptyList))
		b[1].(*values.Pair)[1] = b

		qt.Assert(t, values.EqualTo(a, b), qt.IsTrue)
	})
}
