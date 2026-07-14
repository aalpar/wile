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
)

// TestRationalArithmeticNeverYieldsDenominatorOne is the tower invariant that
// reviews/2026-07-13 found broken: an exact value has exactly ONE representation.
//
// Rational +, -, * and / returned a raw *Rational without simplifying, so
// (+ 1/2 1/2) produced a denominator-1 *Rational. It printed as 1, answered #t to
// both integer? and exact?, and still answered #f to (eqv? … 1) — because eqv?
// dispatches on the concrete type and found *Rational where the tower guarantees
// only *Integer can be.
//
// Pinning "eqv? works" alone would have been the wrong test: eqv? was one consumer
// of a broken invariant, and every other type-dispatching consumer (hashing, memv,
// assv, Kind() dispatch) stood on the same crack. So this pins the INVARIANT — no
// arithmetic path may hand back an integer-valued *Rational — and the eqv? test
// below pins the symptom that made it visible.
func TestRationalArithmeticNeverYieldsDenominatorOne(t *testing.T) {
	half := values.NewRational(1, 2)
	third := values.NewRational(1, 3)
	twoThirds := values.NewRational(2, 3)
	threeHalves := values.NewRational(3, 2)

	div := func(a, b values.Number) values.Number {
		q, err := a.Divide(b)
		qt.Assert(t, err, qt.IsNil)
		return q
	}

	tcs := []struct {
		name string
		got  values.Number
	}{
		{"1/2 + 1/2", half.Add(half)},
		{"3/2 - 1/2", threeHalves.Subtract(half)},
		{"2/3 * 3/2", twoThirds.Multiply(threeHalves)},
		{"1/2 / 1/2", div(half, half)},
		{"1/3 + 2/3", third.Add(twoThirds)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, isRational := tc.got.(*values.Rational)
			qt.Assert(t, isRational, qt.IsFalse,
				qt.Commentf("%s = %s is integer-valued, so it must have descended out of "+
					"*Rational. An integer-valued *Rational is a second representation of a "+
					"value the tower says has exactly one, and every type-dispatching "+
					"consumer breaks on it.", tc.name, tc.got.SchemeString()))
			qt.Assert(t, tc.got.SchemeString(), qt.Equals, "1")
		})
	}
}

// TestRationalArithmeticKeepsGenuineRationals is the other half of the invariant,
// and the guard against "fixing" the above by demoting everything: a result that is
// NOT integer-valued must stay a *Rational and must not be laundered into a float.
func TestRationalArithmeticKeepsGenuineRationals(t *testing.T) {
	half := values.NewRational(1, 2)
	third := values.NewRational(1, 3)

	sum := half.Add(third) // 5/6
	_, isRational := sum.(*values.Rational)
	qt.Assert(t, isRational, qt.IsTrue)
	qt.Assert(t, sum.SchemeString(), qt.Equals, "5/6")
	qt.Assert(t, sum.IsExact(), qt.IsTrue)
}

// TestSimplifyPreservesExactnessClass pins the rule Simplify's own doc comment
// states and two of its SimplifyDown functions broke: Simplify descends WITHIN an
// exactness class. An integer-valued 2.0 is inexact and must stay inexact.
//
// Latent when filed — nothing called Simplify on a float — but wiring Simplify into
// an arithmetic path is a two-line change, which is exactly what rational.go now
// does. The next person to do it for floats would have silently made every
// whole-valued float exact.
func TestSimplifyPreservesExactnessClass(t *testing.T) {
	f := values.NewFloat(2.0)
	got := values.Simplify(f)
	qt.Assert(t, got.IsExact(), qt.IsFalse,
		qt.Commentf("Simplify(2.0) must stay inexact; returning an exact Integer would "+
			"make (exact? 2.0) answer #t"))
	_, stillFloat := got.(*values.Float)
	qt.Assert(t, stillFloat, qt.IsTrue)
}
