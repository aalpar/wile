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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestExactZeroAnnihilatesProduct pins the exact-zero rule for multiplication
// against Chez Scheme and Racket, which agree on every case here.
//
// R7RS §6.2.2 licenses (* 0 x) => exact 0 for an EXACT zero, regardless of x —
// an exact 0 is a mathematical zero, not an IEEE +0.0, so IEEE's 0*inf = NaN
// rule does not govern it. An INEXACT zero is an IEEE value and must not
// short-circuit at all.
func TestExactZeroAnnihilatesProduct(t *testing.T) {
	c := qt.New(t)

	inf := values.NewFloat(math.Inf(1))
	nan := values.NewFloat(math.NaN())
	negZero := values.NewFloat(math.Copysign(0, -1))

	tcs := []struct {
		name      string
		a, b      values.Number
		wantExact bool
		check     func(*qt.C, values.Number)
	}{
		// Exact zero annihilates unconditionally — even against inf and NaN.
		{"exact 0 * 5", values.NewInteger(0), values.NewInteger(5), true, nil},
		{"exact 0 * 3.14", values.NewInteger(0), values.NewFloat(3.14), true, nil},
		{"3.14 * exact 0", values.NewFloat(3.14), values.NewInteger(0), true, nil},
		{"exact 0 * +inf.0", values.NewInteger(0), inf, true, nil},
		{"+inf.0 * exact 0", inf, values.NewInteger(0), true, nil},
		{"exact 0 * +nan.0", values.NewInteger(0), nan, true, nil},
		{"+nan.0 * exact 0", nan, values.NewInteger(0), true, nil},

		// Inexact zero never short-circuits: IEEE governs.
		{"5 * 0.0", values.NewInteger(5), values.NewFloat(0), false, nil},
		{"0.0 * 5", values.NewFloat(0), values.NewInteger(5), false, nil},
		{"1/2 * 0.0", values.NewRational(1, 2), values.NewFloat(0), false, nil},
		{"+inf.0 * 0.0", inf, values.NewFloat(0), false, func(c *qt.C, got values.Number) {
			c.Assert(got.IsNaN(), qt.IsTrue, qt.Commentf("inf * inexact 0 must be NaN"))
		}},
		{"-1.0 * 0.0", values.NewFloat(-1), values.NewFloat(0), false, func(c *qt.C, got values.Number) {
			f, ok := got.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("Float x Float stays on the Float path"))
			c.Assert(math.Signbit(f.Value), qt.IsTrue, qt.Commentf("-1.0 * 0.0 must be -0.0"))
		}},
		// NOTE: assert the VALUE, not the concrete type. LUB(Float, Integer) is
		// BigFloat (promotion.go), so an exact x inexact product lands on a
		// *BigFloat, not a *Float. big.Float DOES carry the sign of a zero (its
		// Float64() projection round-trips the sign bit) even though Sign() reports 0.
		{"-0.0 * 2 (exact)", negZero, values.NewInteger(2), false, func(c *qt.C, got values.Number) {
			c.Assert(math.Signbit(values.NumberToFloat64(got)), qt.IsTrue,
				qt.Commentf("-0.0 * 2 must be -0.0, got %v (%T)", got, got))
		}},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			got := tc.a.Multiply(tc.b)
			c.Assert(got.IsExact(), qt.Equals, tc.wantExact)
			if tc.check != nil {
				tc.check(c, got)
			}
		})
	}
}
