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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestPromotionTable_Symmetry(t *testing.T) {
	c := qt.New(t)
	for a := range numKinds {
		for b := range numKinds {
			c.Assert(
				promotionTable[a][b], qt.Equals, promotionTable[b][a],
				qt.Commentf("promotionTable[%d][%d] != promotionTable[%d][%d]", a, b, b, a),
			)
		}
	}
}

func TestPromotionTable_Diagonal(t *testing.T) {
	c := qt.New(t)
	// Every type paired with itself should return itself.
	for k := range numKinds {
		c.Assert(
			promotionTable[k][k], qt.Equals, k,
			qt.Commentf("diagonal: kind %d should promote to itself", k),
		)
	}
}

func TestPromotionTable_ExactResults(t *testing.T) {
	c := qt.New(t)
	// Verify every entry in the no-loss result type matrix.
	// Layout: promotionTable[row][col] = expected
	type entry struct {
		a, b     NumericKind
		expected NumericKind
	}

	entries := []entry{
		// Zone 1: Exact × Exact
		{KindInteger, KindInteger, KindInteger},
		{KindInteger, KindBigInteger, KindBigInteger},
		{KindInteger, KindRational, KindRational},
		{KindBigInteger, KindBigInteger, KindBigInteger},
		{KindBigInteger, KindRational, KindRational},
		{KindRational, KindRational, KindRational},

		// Zone 2: Exact × Inexact Real → BigFloat
		{KindInteger, KindFloat, KindBigFloat},
		{KindInteger, KindBigFloat, KindBigFloat},
		{KindBigInteger, KindFloat, KindBigFloat},
		{KindBigInteger, KindBigFloat, KindBigFloat},
		{KindRational, KindFloat, KindBigFloat},
		{KindRational, KindBigFloat, KindBigFloat},

		// Inexact × Inexact
		{KindFloat, KindFloat, KindFloat},
		{KindFloat, KindBigFloat, KindBigFloat},
		{KindBigFloat, KindBigFloat, KindBigFloat},

		// Zone 3: Anything × Complex
		{KindInteger, KindComplex, KindBigComplex},
		{KindInteger, KindBigComplex, KindBigComplex},
		{KindBigInteger, KindComplex, KindBigComplex},
		{KindBigInteger, KindBigComplex, KindBigComplex},
		{KindRational, KindComplex, KindBigComplex},
		{KindRational, KindBigComplex, KindBigComplex},
		{KindFloat, KindComplex, KindComplex},
		{KindFloat, KindBigComplex, KindBigComplex},
		{KindBigFloat, KindComplex, KindBigComplex},
		{KindBigFloat, KindBigComplex, KindBigComplex},
		{KindComplex, KindComplex, KindComplex},
		{KindComplex, KindBigComplex, KindBigComplex},
		{KindBigComplex, KindBigComplex, KindBigComplex},
	}

	for _, e := range entries {
		c.Assert(
			PromotionResultKind(e.a, e.b), qt.Equals, e.expected,
			qt.Commentf("PromotionResultKind(%d, %d)", e.a, e.b),
		)
		// Verify symmetry inline.
		c.Assert(
			PromotionResultKind(e.b, e.a), qt.Equals, e.expected,
			qt.Commentf("PromotionResultKind(%d, %d) (symmetric)", e.b, e.a),
		)
	}
}

func TestPromotionTable_NoLossyPaths(t *testing.T) {
	c := qt.New(t)
	// Exact types meeting Float or Complex must NEVER produce Float or Complex.
	// They must produce BigFloat or BigComplex respectively.
	exactKinds := []NumericKind{KindInteger, KindBigInteger, KindRational}
	for _, exact := range exactKinds {
		// Exact + Float must NOT be Float.
		result := promotionTable[exact][KindFloat]
		c.Assert(
			result != KindFloat, qt.IsTrue,
			qt.Commentf("exact kind %d + Float must not produce Float (got %d)", exact, result),
		)
		// Exact + Complex must NOT be Complex.
		result = promotionTable[exact][KindComplex]
		c.Assert(
			result != KindComplex, qt.IsTrue,
			qt.Commentf("exact kind %d + Complex must not produce Complex (got %d)", exact, result),
		)
	}

	// BigFloat + Complex must NOT be Complex.
	result := promotionTable[KindBigFloat][KindComplex]
	c.Assert(
		result != KindComplex, qt.IsTrue,
		qt.Commentf("BigFloat + Complex must not produce Complex (got %d)", result),
	)
}

func TestPromoter_AllReachablePathsPopulated(t *testing.T) {
	c := qt.New(t)
	// For every (a, b) pair, the promoter for a→result and b→result must exist.
	for a := range numKinds {
		for b := range numKinds {
			result := promotionTable[a][b]
			c.Assert(
				promoter[a][result] != nil, qt.IsTrue,
				qt.Commentf("promoter[%d][%d] is nil (needed for pair %d+%d→%d)", a, result, a, b, result),
			)
			c.Assert(
				promoter[b][result] != nil, qt.IsTrue,
				qt.Commentf("promoter[%d][%d] is nil (needed for pair %d+%d→%d)", b, result, a, b, result),
			)
		}
	}
}

func TestPromote_IntegerToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewInteger(42)

	// Identity.
	c.Assert(Promote(n, KindInteger), qt.Equals, n)

	// → BigInteger
	bi := Promote(n, KindBigInteger)
	c.Assert(bi.Kind(), qt.Equals, KindBigInteger)
	c.Assert(bi.(*BigInteger).value.Int64(), qt.Equals, int64(42))

	// → Rational
	r := Promote(n, KindRational)
	c.Assert(r.Kind(), qt.Equals, KindRational)
	c.Assert(r.(*Rational).NumInt64(), qt.Equals, int64(42))
	c.Assert(r.(*Rational).DenomInt64(), qt.Equals, int64(1))

	// → BigFloat
	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)
	f64, _ := bf.(*BigFloat).value.Float64()
	c.Assert(f64, qt.Equals, float64(42))

	// → BigComplex (exact parts)
	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
	bcc := bc.(*BigComplex)
	c.Assert(bcc.real.Kind(), qt.Equals, KindBigInteger)
	c.Assert(bcc.imag.IsZero(), qt.IsTrue)
}

func TestPromote_BigIntegerToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewBigIntegerFromInt64(1000000)

	c.Assert(Promote(n, KindBigInteger), qt.Equals, n)

	r := Promote(n, KindRational)
	c.Assert(r.Kind(), qt.Equals, KindRational)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
	c.Assert(bc.(*BigComplex).real.Kind(), qt.Equals, KindBigInteger)
}

func TestPromote_RationalToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewRational(3, 7)

	c.Assert(Promote(n, KindRational), qt.Equals, n)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_FloatToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewFloat(3.14)

	c.Assert(Promote(n, KindFloat), qt.Equals, n)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	cx := Promote(n, KindComplex)
	c.Assert(cx.Kind(), qt.Equals, KindComplex)
	c.Assert(real(cx.(*Complex).Value), qt.Equals, 3.14)
	c.Assert(imag(cx.(*Complex).Value), qt.Equals, 0.0)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_BigFloatToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewBigFloatFromFloat64(2.718)

	c.Assert(Promote(n, KindBigFloat), qt.Equals, n)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_ComplexToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewComplex(complex(1.0, 2.0))

	c.Assert(Promote(n, KindComplex), qt.Equals, n)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_BigComplexIdentity(t *testing.T) {
	c := qt.New(t)
	n := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1.0), NewBigFloatFromFloat64(2.0))
	c.Assert(Promote(n, KindBigComplex), qt.Equals, n)
}

func TestPromote_PreservesExactness(t *testing.T) {
	c := qt.New(t)
	// Integer → BigComplex should produce exact parts (BigInteger, not BigFloat).
	n := NewInteger(7)
	bc := Promote(n, KindBigComplex).(*BigComplex)
	c.Assert(bc.real.IsExact(), qt.IsTrue)
	c.Assert(bc.imag.IsExact(), qt.IsTrue)

	// BigInteger → BigComplex should preserve exact BigInteger parts.
	bi := NewBigIntegerFromInt64(999)
	bc2 := Promote(bi, KindBigComplex).(*BigComplex)
	c.Assert(bc2.real.IsExact(), qt.IsTrue)
	c.Assert(bc2.imag.IsExact(), qt.IsTrue)
}

func TestPromotionResultKind_API(t *testing.T) {
	c := qt.New(t)
	// Public API produces same results as direct table access.
	c.Assert(PromotionResultKind(KindInteger, KindFloat), qt.Equals, KindBigFloat)
	c.Assert(PromotionResultKind(KindRational, KindComplex), qt.Equals, KindBigComplex)
	c.Assert(PromotionResultKind(KindFloat, KindComplex), qt.Equals, KindComplex)
}
