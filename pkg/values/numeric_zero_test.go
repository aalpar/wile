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

// TestExactZeroIsAdditiveIdentity pins (+ x 0) => x and (- x 0) => x for an
// EXACT zero, preserving sign and exactness of x. An inexact zero must not
// short-circuit: IEEE contagion governs, and (+ -0.0 0.0) is +0.0.
func TestExactZeroIsAdditiveIdentity(t *testing.T) {
	c := qt.New(t)

	negZero := values.NewFloat(math.Copysign(0, -1))
	exactZero := values.NewInteger(0)

	c.Run("(+ -0.0 exact-0) preserves negative zero", func(c *qt.C) {
		got := negZero.Add(exactZero)
		f, ok := got.(*values.Float)
		c.Assert(ok, qt.IsTrue)
		c.Assert(math.Signbit(f.Value), qt.IsTrue)
	})

	c.Run("(+ exact-0 -0.0) preserves negative zero", func(c *qt.C) {
		got := exactZero.Add(negZero)
		f, ok := got.(*values.Float)
		c.Assert(ok, qt.IsTrue)
		c.Assert(math.Signbit(f.Value), qt.IsTrue)
	})

	c.Run("(- -0.0 exact-0) preserves negative zero", func(c *qt.C) {
		got := negZero.Subtract(exactZero)
		f, ok := got.(*values.Float)
		c.Assert(ok, qt.IsTrue)
		c.Assert(math.Signbit(f.Value), qt.IsTrue)
	})

	// The asymmetry: the two operands of Subtract short-circuit DIFFERENTLY,
	// because subtraction is not commutative. The right operand yields the left one
	// unchanged ((- x 0) is x); an exact zero on the left NEGATES ((- 0 x) is -x).
	// The sign of that negation is pinned by TestExactZeroSubtractNegates.
	c.Run("(- exact-0 5) negates rather than short-circuiting", func(c *qt.C) {
		got := exactZero.Subtract(values.NewInteger(5))
		c.Assert(values.NumberToFloat64(got), qt.Equals, float64(-5))
	})

	c.Run("(+ exact-0 0.0) stays inexact (contagion)", func(c *qt.C) {
		got := exactZero.Add(values.NewFloat(0))
		c.Assert(got.IsExact(), qt.IsFalse)
	})

	c.Run("(+ exact-0 exact-0) stays exact", func(c *qt.C) {
		c.Assert(exactZero.Add(values.NewInteger(0)).IsExact(), qt.IsTrue)
	})

	// The Complex regression: an exact complex minus an inexact zero must
	// become inexact. Complex.Add/Subtract currently short-circuit on ANY zero.
	c.Run("(+ 0.0+0.0i 5) stays inexact", func(c *qt.C) {
		z := values.NewComplex(complex(0, 0))
		got := z.Add(values.NewInteger(5))
		c.Assert(got.IsExact(), qt.IsFalse,
			qt.Commentf("inexact complex zero must not hand back the exact operand"))
	})
}

// TestDivideByInexactZeroSign pins the sign of the infinity produced by dividing
// by an inexact zero. The sign is dividend-sign XOR divisor-sign; BigFloat.Divide
// consulted only the dividend.
//
// This is reachable from ordinary code with no BigFloat in it: the promotion table
// maps Exact x InexactReal -> BigFloat, so (/ 1 -0.0) divides through BigFloat.
func TestDivideByInexactZeroSign(t *testing.T) {
	c := qt.New(t)

	posZero := values.NewFloat(0)
	negZero := values.NewFloat(math.Copysign(0, -1))

	tcs := []struct {
		name    string
		a, b    values.Number
		wantNeg bool
	}{
		{"1 / -0.0", values.NewInteger(1), negZero, true},
		{"1 / 0.0", values.NewInteger(1), posZero, false},
		{"-1 / -0.0", values.NewInteger(-1), negZero, false},
		{"-1 / 0.0", values.NewInteger(-1), posZero, true},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			got, err := tc.a.Divide(tc.b)
			c.Assert(err, qt.IsNil)
			f := values.NumberToFloat64(got)
			c.Assert(math.IsInf(f, 0), qt.IsTrue, qt.Commentf("want an infinity, got %v", got))
			c.Assert(math.Signbit(f), qt.Equals, tc.wantNeg)
		})
	}

	// 0.0 / 0.0 is NaN, and big.Float signals that by panicking with ErrNaN --
	// which is exactly what recoverNaN is for. Guard the deletion of the
	// hand-rolled branch against reintroducing that panic.
	c.Run("0.0 / 0.0 is NaN, not a panic", func(c *qt.C) {
		got, err := values.NewBigFloatFromFloat64(0).Divide(posZero)
		c.Assert(err, qt.IsNil)
		c.Assert(got.IsNaN(), qt.IsTrue)
	})
}
