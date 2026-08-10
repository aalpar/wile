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
	"testing"

	qt "github.com/frankban/quicktest"
)

// twoTo300 is the largest power of two both a float64 and a 256-bit BigFloat
// hold exactly, which makes 2^300+1 the smallest witness that a rounding
// implementation cannot tell apart from 2^300.
func twoTo300() *big.Int {
	return new(big.Int).Lsh(big.NewInt(1), 300)
}

func bigIntPlusOne(n *big.Int) *BigInteger {
	return NewBigInteger(new(big.Int).Add(n, big.NewInt(1)))
}

// TestCompareNumbers_NeverRounds replaces TestComparisonTable_NoLossyPaths,
// which could not fail for the reason its own name gave.
//
// The old test asserted KIND identities and nothing else: that comparing an
// exact kind against Float landed on BigFloat rather than Float, and against
// Complex on BigComplex rather than Complex. A kind identity is not
// losslessness. DefaultBigFloatPrecision is 256, so the very cell the test
// insisted on ROUNDED any exact operand needing more than 256 significant bits,
// and the test stayed green through the entire defect it was named for. Its
// stated witness -- (= (- (expt 2 100) 1) (exact->inexact (expt 2 100))) --
// happened to fit in 256 bits; the review's did not.
//
// This version asserts VALUES, on operands chosen so that rounding EITHER of
// them changes the verdict. Every row goes red if the kernel rounds.
func TestCompareNumbers_NeverRounds(t *testing.T) {
	c := qt.New(t)

	pow := twoTo300()
	floatPow := NewFloat(math.Ldexp(1, 300))
	exactPow := NewBigInteger(pow)
	exactPowPlus1 := bigIntPlusOne(pow)

	tests := []struct {
		name string
		a    Number
		b    Number
		want Ordering
	}{
		{
			// The review's witness. 2^300+1 needs 301 significant bits, so both
			// float64 and a 256-bit BigFloat collapse it onto 2^300.
			name: "float 2^300 vs exact 2^300+1",
			a:    floatPow,
			b:    exactPowPlus1,
			want: OrderLess,
		},
		{
			name: "exact 2^300+1 vs float 2^300",
			a:    exactPowPlus1,
			b:    floatPow,
			want: OrderGreater,
		},
		{
			name: "float 2^300 vs exact 2^300 is genuinely equal",
			a:    floatPow,
			b:    exactPow,
			want: OrderEqual,
		},
		{
			// 1/3 is not a dyadic rational, so no binary float represents it and
			// the nearest float64 falls short of it.
			name: "rational 1/3 vs nearest float64",
			a:    NewRational(1, 3),
			b:    NewFloat(0.3333333333333333),
			want: OrderGreater,
		},
		{
			// Just past float64's exactly-representable integer range: rounding
			// the exact operand answers OrderEqual.
			name: "fixnum 2^53+1 vs float 2^53",
			a:    NewInteger((int64(1) << 53) + 1),
			b:    NewFloat(math.Ldexp(1, 53)),
			want: OrderGreater,
		},
		{
			// The exact × Complex axis, which the deleted test also only checked
			// by kind. complex128's real part is a float64, so rounding through
			// it answers OrderEqual.
			name: "exact 2^300+1 vs complex 2^300+0i",
			a:    exactPowPlus1,
			b:    NewComplexFromParts(math.Ldexp(1, 300), 0),
			want: OrderUnordered,
		},
		{
			// The preservation pin at value level: a float64 and a BigFloat
			// holding the same number ARE equal. (eqv? separates them, and must;
			// see EqvNumber.)
			name: "float 3.0 vs bigfloat 3.0",
			a:    NewFloat(3),
			b:    NewBigFloatFromFloat64(3),
			want: OrderEqual,
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(CompareNumbers(tt.a, tt.b), qt.Equals, tt.want)
		})
	}
}

// compareWitnesses spans the real kinds plus the special values, and is the
// table both properties below run over.
func compareWitnesses() []Number {
	pow := twoTo300()
	return []Number{
		NewInteger(0),
		NewInteger(1),
		NewInteger(-7),
		NewBigInteger(pow),
		bigIntPlusOne(pow),
		NewRational(1, 3),
		NewRational(-7, 2),
		NewFloat(math.Ldexp(1, 300)),
		NewFloat(0.3333333333333333),
		// A genuine negative zero: the Go literal -0.0 is +0.0, so it has to be
		// built. It is here because a signed zero is the one value where the
		// verdict and eqv? must disagree -- (= -0.0 0.0) is #t.
		NewFloat(math.Copysign(0, -1)),
		NewBigFloatFromFloat64(3),
		NewBigFloatFromFloat64(0.5),
		NewFloat(math.Inf(1)),
		NewFloat(math.Inf(-1)),
		NewBigFloat(new(big.Float).SetInf(false)),
		NewBigFloat(new(big.Float).SetInf(true)),
	}
}

// TestCompareNumbers_IsAntisymmetric replaces TestComparisonTable_Symmetry,
// which asserted that a KIND table was symmetric. That is a property of a table,
// not of an answer, and the table it described has been deleted; a comparison
// that promoted symmetrically could still (and did) answer inconsistently.
//
// The property that actually matters is on the verdict: swapping the operands
// mirrors it, and every number compares equal to itself.
func TestCompareNumbers_IsAntisymmetric(t *testing.T) {
	c := qt.New(t)
	mirror := map[Ordering]Ordering{
		OrderLess:      OrderGreater,
		OrderGreater:   OrderLess,
		OrderEqual:     OrderEqual,
		OrderUnordered: OrderUnordered,
	}
	ws := compareWitnesses()
	for _, a := range ws {
		c.Assert(CompareNumbers(a, a), qt.Equals, OrderEqual,
			qt.Commentf("reflexivity: %s", a.SchemeString()))
		for _, b := range ws {
			forward := CompareNumbers(a, b)
			c.Assert(CompareNumbers(b, a), qt.Equals, mirror[forward],
				qt.Commentf("antisymmetry: %s vs %s", a.SchemeString(), b.SchemeString()))
		}
	}
}

// TestCompareNumbers_NaNIsTheFourthVerdict pins the state a three-valued
// comparison cannot express. A Compare(Number) int used to live on the Number
// interface and gave NaN a 0, which every caller read as "equal".
func TestCompareNumbers_NaNIsTheFourthVerdict(t *testing.T) {
	c := qt.New(t)
	nan := NewFloat(math.NaN())
	bigNaN := NewBigFloatNaN()
	for _, n := range []Number{nan, bigNaN} {
		c.Assert(CompareNumbers(n, n), qt.Equals, OrderUnordered)
		for _, w := range compareWitnesses() {
			c.Assert(CompareNumbers(n, w), qt.Equals, OrderUnordered,
				qt.Commentf("NaN vs %s", w.SchemeString()))
			c.Assert(CompareNumbers(w, n), qt.Equals, OrderUnordered,
				qt.Commentf("%s vs NaN", w.SchemeString()))
		}
	}
}

// TestCompareNumbers_InfinitiesAreEqualAcrossKinds is item 12b. (= x x) was #f
// for an infinity in any representation other than *Float, because equality fell
// through to Subtract(...).IsZero() and Inf - Inf is NaN. eqv? answered #t on the
// identical value, contradicting R7RS §6.1's "numerically equal (in the sense of
// =)".
func TestCompareNumbers_InfinitiesAreEqualAcrossKinds(t *testing.T) {
	c := qt.New(t)
	floatPos := NewFloat(math.Inf(1))
	floatNeg := NewFloat(math.Inf(-1))
	bigPos := NewBigFloat(new(big.Float).SetInf(false))
	bigNeg := NewBigFloat(new(big.Float).SetInf(true))

	c.Assert(CompareNumbers(bigPos, bigPos), qt.Equals, OrderEqual)
	c.Assert(CompareNumbers(bigPos, floatPos), qt.Equals, OrderEqual)
	c.Assert(CompareNumbers(bigNeg, floatNeg), qt.Equals, OrderEqual)
	c.Assert(CompareNumbers(bigNeg, bigPos), qt.Equals, OrderLess)
	c.Assert(CompareNumbers(floatPos, bigNeg), qt.Equals, OrderGreater)

	// An infinity outranks every finite value, including one too large for
	// float64 -- the trichotomy break the old IEEE guard used to cause by
	// rounding the exact operand to ±Inf and calling the two equal.
	huge := NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil))
	c.Assert(CompareNumbers(huge, floatPos), qt.Equals, OrderLess)
	c.Assert(CompareNumbers(huge, floatNeg), qt.Equals, OrderGreater)
}
