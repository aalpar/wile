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
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestLessThanNaN pins the Number.LessThan NaN contract across the tower: a NaN
// operand yields false, in either position, for every numeric kind, and never
// panics.
//
// LessThan is the tower's only ordering primitive (Compare was removed — see
// values.Number), so this is the whole of what Wile promises about ordering a
// NaN. False in both directions is not a shortfall of the bool: it is what
// "unordered" means, and it is why the bool was kept and the int discarded.
//
// The panic clause is load-bearing and outlived the method it was written for.
// Comparison paths that promote to big.Float are a standing hazard, because
// big.Float has no NaN and SetFloat64 rejects it — a promoting comparison takes
// down the host on an input a Scheme program can produce with (/ 0. 0.). These
// are public API, so an embedder reaches them without any Scheme at all.
func TestLessThanNaN(t *testing.T) {
	c := qt.New(t)

	finite := []struct {
		name string
		n    values.Number
	}{
		{"Float", values.NewFloat(1)},
		{"Integer", values.NewInteger(1)},
		{"BigInteger", values.NewBigIntegerFromInt64(1)},
		{"Rational", values.NewRationalFromRat(big.NewRat(1, 1))},
		{"BigFloat", values.NewBigFloatFromFloat64(1)},
		{"Complex", values.NewComplex(complex(1, 0))},
	}
	nans := []struct {
		name string
		n    values.Number
	}{
		{"Float(NaN)", values.NewFloat(math.NaN())},
		{"BigFloat(NaN)", values.NewBigFloatNaN()},
		{"Complex(NaN)", values.NewComplex(complex(math.NaN(), 0))},
	}

	for _, nan := range nans {
		for _, fin := range finite {
			c.Run(nan.name+" vs "+fin.name, func(c *qt.C) {
				c.Assert(nan.n.LessThan(fin.n), qt.IsFalse)
				c.Assert(fin.n.LessThan(nan.n), qt.IsFalse)
			})
		}
	}

	// NaN against NaN, including same-kind.
	for _, a := range nans {
		for _, b := range nans {
			c.Run(a.name+" vs "+b.name, func(c *qt.C) {
				c.Assert(a.n.LessThan(b.n), qt.IsFalse)
			})
		}
	}
}

// TestLessThanNaNIsNotEquality states the sharp edge that survives Compare's
// removal, so it cannot be re-learned the hard way.
//
// "Not less than in either direction" is how numEqual (eqv.go) spells equality,
// and for a NaN that reading is wrong: two NaNs are unordered, not equal. The
// bool does not conflate the two the way Compare's 0 did — nothing here CLAIMS
// they are equal — but a caller that infers equality from the absence of ordering
// must exclude NaN first, exactly as numEqual's callers do.
//
// eqv? answers #t for NaN-vs-NaN, and it gets there via an explicit IsNaN guard,
// never by this route.
func TestLessThanNaNIsNotEquality(t *testing.T) {
	c := qt.New(t)

	nan := values.NewFloat(math.NaN())
	other := values.NewFloat(math.NaN())
	one := values.NewFloat(1)

	// Neither less than the other, in both cases — yet only one pair is equal.
	c.Assert(nan.LessThan(other), qt.IsFalse)
	c.Assert(other.LessThan(nan), qt.IsFalse)
	c.Assert(nan.LessThan(one), qt.IsFalse)
	c.Assert(one.LessThan(nan), qt.IsFalse)

	// So the antisymmetry reading is only valid once NaN is excluded.
	c.Assert(nan.IsNaN(), qt.IsTrue)
	c.Assert(other.IsNaN(), qt.IsTrue)

	// eqv? reaches the right answers by guarding, not by ordering.
	c.Assert(values.EqvNumber(nan, other), qt.IsTrue)
	c.Assert(values.EqvNumber(nan, one), qt.IsFalse)
}

// TestLessThanFloatMatchesFloat64Ordering pins Float.LessThan against Go's own
// float64 <, signed zero and the infinities included, so the tower's fast path
// cannot drift from IEEE.
func TestLessThanFloatMatchesFloat64Ordering(t *testing.T) {
	c := qt.New(t)

	vals := []float64{
		math.Inf(-1), -1e308, -1, -0.5, math.Copysign(0, -1), 0, 0.5, 1, 1e308, math.Inf(1),
	}
	for _, a := range vals {
		for _, b := range vals {
			got := values.NewFloat(a).LessThan(values.NewFloat(b))
			c.Assert(got, qt.Equals, a < b, qt.Commentf("LessThan(%v, %v)", a, b))
		}
	}
}

// TestBigComplexLessThanIsNaNSafe pins BigComplex.LessThan's NaN contract: it
// orders on real parts only, matching Complex, and the real-part comparison owns
// the NaN answer.
//
// A real-part NaN is unordered (BigFloat.LessThan yields #f both ways, directly or
// after promotion). An imaginary-part NaN is irrelevant to a real-parts-only order
// and is ignored, exactly as Complex ignores it. An earlier IsNaN() guard (real OR
// imag) over-reached and made imag-NaN values unordered, which Complex never did;
// this test also pins that parity with Complex so the guard cannot come back.
func TestBigComplexLessThanIsNaNSafe(t *testing.T) {
	c := qt.New(t)

	nan := values.NewBigComplex(values.NewBigFloatNaN(), values.NewBigFloatFromFloat64(0))
	one := values.NewBigComplex(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(0))

	c.Assert(nan.LessThan(one), qt.IsFalse)
	c.Assert(one.LessThan(nan), qt.IsFalse)
	c.Assert(nan.LessThan(nan), qt.IsFalse)

	// Cross-kind: a real-part NaN reaches BigFloat.LessThan after promotion, so it
	// stays unordered without any guard in BigComplex.LessThan.
	c.Assert(nan.LessThan(values.NewInteger(1)), qt.IsFalse)
	c.Assert(values.NewInteger(1).LessThan(nan), qt.IsFalse)

	// Non-NaN ordering still works, so the assertions above are not vacuous.
	two := values.NewBigComplex(values.NewBigFloatFromFloat64(2), values.NewBigFloatFromFloat64(0))
	c.Assert(one.LessThan(two), qt.IsTrue)
	c.Assert(two.LessThan(one), qt.IsFalse)
	c.Assert(one.LessThan(values.NewInteger(2)), qt.IsTrue)

	// An imaginary-part NaN is irrelevant to a real-parts-only order: the value is
	// ordered by its (non-NaN) real part, and BigComplex agrees with Complex. This
	// is the parity an earlier IsNaN() guard broke by treating imag-NaN as unordered.
	imagNaN := values.NewBigComplex(values.NewBigFloatFromFloat64(1), values.NewBigFloatNaN())
	c.Assert(imagNaN.LessThan(two), qt.IsTrue)
	c.Assert(two.LessThan(imagNaN), qt.IsFalse)

	cImagNaN := values.NewComplex(complex(1, math.NaN()))
	cTwo := values.NewComplex(complex(2, 0))
	c.Assert(imagNaN.LessThan(two), qt.Equals, cImagNaN.LessThan(cTwo))
	c.Assert(two.LessThan(imagNaN), qt.Equals, cTwo.LessThan(cImagNaN))
}
