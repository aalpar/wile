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

// TestFloat64TimesExactBigComplexIsComplex pins the contagion of a Float or Complex
// operand into an EXACT BigComplex: the result is a Complex (float64 parts), as
// Float x Rational is a Float, in every operator and both operand orders.
//
// The parts are computed one by one with the exact parts kept exact, and each
// part operation is the real Float x exact contagion. That is what Chez does
// (s/5_3.ss: make-rectangular over generic part arithmetic, Smith's algorithm for
// a complex divisor), and it is observable: an exact zero part annihilates, so
// (* +inf.0 +2i) is 0.0+inf.0i, where rounding +2i to 0.0+2.0i first gives NaN.
//
// The want columns are petite's output for this grid, which racket -I r5rs
// reproduces on all 144 rows:
//
//	fs = (1.0 -0.0 +inf.0 +nan.0 1.0+2.0i -1.0-0.0i), zs = (1+1/3i +2i +1/3i)
//	(write (list (real-part (op f z)) (imag-part (op f z))))   ; and (op z f)
func TestFloat64TimesExactBigComplexIsComplex(t *testing.T) {
	negZero := math.Copysign(0, -1)
	inexact := map[string]values.Number{
		"1.0":       values.NewFloat(1.0),
		"-0.0":      values.NewFloat(negZero),
		"+inf.0":    values.NewFloat(math.Inf(1)),
		"+nan.0":    values.NewFloat(math.NaN()),
		"1.0+2.0i":  values.NewComplex(complex(1, 2)),
		"-1.0-0.0i": values.NewComplex(complex(-1, negZero)),
	}
	exact := map[string]values.Number{
		"1+1/3i": values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewRational(1, 3)),
		"0+2i":   values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(2)),
		"0+1/3i": values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewRational(1, 3)),
	}
	ops := map[string]func(a, b values.Number) (values.Number, error){
		"+": func(a, b values.Number) (values.Number, error) {
			return a.Add(b), nil
		},
		"-": func(a, b values.Number) (values.Number, error) {
			return a.Subtract(b), nil
		},
		"*": func(a, b values.Number) (values.Number, error) {
			return a.Multiply(b), nil
		},
		"/": func(a, b values.Number) (values.Number, error) {
			return a.Divide(b)
		},
	}
	tcs := []struct {
		op     string
		fFirst bool
		f, z   string
		re, im float64
	}{
		{"+", true, "1.0", "1+1/3i", 2.0, 0.3333333333333333},
		{"+", false, "1.0", "1+1/3i", 2.0, 0.3333333333333333},
		{"+", true, "1.0", "0+2i", 1.0, 2.0},
		{"+", false, "1.0", "0+2i", 1.0, 2.0},
		{"+", true, "1.0", "0+1/3i", 1.0, 0.3333333333333333},
		{"+", false, "1.0", "0+1/3i", 1.0, 0.3333333333333333},
		{"+", true, "-0.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"+", false, "-0.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"+", true, "-0.0", "0+2i", negZero, 2.0},
		{"+", false, "-0.0", "0+2i", negZero, 2.0},
		{"+", true, "-0.0", "0+1/3i", negZero, 0.3333333333333333},
		{"+", false, "-0.0", "0+1/3i", negZero, 0.3333333333333333},
		{"+", true, "+inf.0", "1+1/3i", math.Inf(1), 0.3333333333333333},
		{"+", false, "+inf.0", "1+1/3i", math.Inf(1), 0.3333333333333333},
		{"+", true, "+inf.0", "0+2i", math.Inf(1), 2.0},
		{"+", false, "+inf.0", "0+2i", math.Inf(1), 2.0},
		{"+", true, "+inf.0", "0+1/3i", math.Inf(1), 0.3333333333333333},
		{"+", false, "+inf.0", "0+1/3i", math.Inf(1), 0.3333333333333333},
		{"+", true, "+nan.0", "1+1/3i", math.NaN(), 0.3333333333333333},
		{"+", false, "+nan.0", "1+1/3i", math.NaN(), 0.3333333333333333},
		{"+", true, "+nan.0", "0+2i", math.NaN(), 2.0},
		{"+", false, "+nan.0", "0+2i", math.NaN(), 2.0},
		{"+", true, "+nan.0", "0+1/3i", math.NaN(), 0.3333333333333333},
		{"+", false, "+nan.0", "0+1/3i", math.NaN(), 0.3333333333333333},
		{"+", true, "1.0+2.0i", "1+1/3i", 2.0, 2.3333333333333335},
		{"+", false, "1.0+2.0i", "1+1/3i", 2.0, 2.3333333333333335},
		{"+", true, "1.0+2.0i", "0+2i", 1.0, 4.0},
		{"+", false, "1.0+2.0i", "0+2i", 1.0, 4.0},
		{"+", true, "1.0+2.0i", "0+1/3i", 1.0, 2.3333333333333335},
		{"+", false, "1.0+2.0i", "0+1/3i", 1.0, 2.3333333333333335},
		{"+", true, "-1.0-0.0i", "1+1/3i", 0.0, 0.3333333333333333},
		{"+", false, "-1.0-0.0i", "1+1/3i", 0.0, 0.3333333333333333},
		{"+", true, "-1.0-0.0i", "0+2i", -1.0, 2.0},
		{"+", false, "-1.0-0.0i", "0+2i", -1.0, 2.0},
		{"+", true, "-1.0-0.0i", "0+1/3i", -1.0, 0.3333333333333333},
		{"+", false, "-1.0-0.0i", "0+1/3i", -1.0, 0.3333333333333333},
		{"-", true, "1.0", "1+1/3i", 0.0, -0.3333333333333333},
		{"-", false, "1.0", "1+1/3i", 0.0, 0.3333333333333333},
		{"-", true, "1.0", "0+2i", 1.0, -2.0},
		{"-", false, "1.0", "0+2i", -1.0, 2.0},
		{"-", true, "1.0", "0+1/3i", 1.0, -0.3333333333333333},
		{"-", false, "1.0", "0+1/3i", -1.0, 0.3333333333333333},
		{"-", true, "-0.0", "1+1/3i", -1.0, -0.3333333333333333},
		{"-", false, "-0.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"-", true, "-0.0", "0+2i", negZero, -2.0},
		{"-", false, "-0.0", "0+2i", 0.0, 2.0},
		{"-", true, "-0.0", "0+1/3i", negZero, -0.3333333333333333},
		{"-", false, "-0.0", "0+1/3i", 0.0, 0.3333333333333333},
		{"-", true, "+inf.0", "1+1/3i", math.Inf(1), -0.3333333333333333},
		{"-", false, "+inf.0", "1+1/3i", math.Inf(-1), 0.3333333333333333},
		{"-", true, "+inf.0", "0+2i", math.Inf(1), -2.0},
		{"-", false, "+inf.0", "0+2i", math.Inf(-1), 2.0},
		{"-", true, "+inf.0", "0+1/3i", math.Inf(1), -0.3333333333333333},
		{"-", false, "+inf.0", "0+1/3i", math.Inf(-1), 0.3333333333333333},
		{"-", true, "+nan.0", "1+1/3i", math.NaN(), -0.3333333333333333},
		{"-", false, "+nan.0", "1+1/3i", math.NaN(), 0.3333333333333333},
		{"-", true, "+nan.0", "0+2i", math.NaN(), -2.0},
		{"-", false, "+nan.0", "0+2i", math.NaN(), 2.0},
		{"-", true, "+nan.0", "0+1/3i", math.NaN(), -0.3333333333333333},
		{"-", false, "+nan.0", "0+1/3i", math.NaN(), 0.3333333333333333},
		{"-", true, "1.0+2.0i", "1+1/3i", 0.0, 1.6666666666666667},
		{"-", false, "1.0+2.0i", "1+1/3i", 0.0, -1.6666666666666667},
		{"-", true, "1.0+2.0i", "0+2i", 1.0, 0.0},
		{"-", false, "1.0+2.0i", "0+2i", -1.0, 0.0},
		{"-", true, "1.0+2.0i", "0+1/3i", 1.0, 1.6666666666666667},
		{"-", false, "1.0+2.0i", "0+1/3i", -1.0, -1.6666666666666667},
		{"-", true, "-1.0-0.0i", "1+1/3i", -2.0, -0.3333333333333333},
		{"-", false, "-1.0-0.0i", "1+1/3i", 2.0, 0.3333333333333333},
		{"-", true, "-1.0-0.0i", "0+2i", -1.0, -2.0},
		{"-", false, "-1.0-0.0i", "0+2i", 1.0, 2.0},
		{"-", true, "-1.0-0.0i", "0+1/3i", -1.0, -0.3333333333333333},
		{"-", false, "-1.0-0.0i", "0+1/3i", 1.0, 0.3333333333333333},
		{"*", true, "1.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"*", false, "1.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"*", true, "1.0", "0+2i", 0.0, 2.0},
		{"*", false, "1.0", "0+2i", 0.0, 2.0},
		{"*", true, "1.0", "0+1/3i", 0.0, 0.3333333333333333},
		{"*", false, "1.0", "0+1/3i", 0.0, 0.3333333333333333},
		{"*", true, "-0.0", "1+1/3i", negZero, negZero},
		{"*", false, "-0.0", "1+1/3i", negZero, negZero},
		{"*", true, "-0.0", "0+2i", 0.0, negZero},
		{"*", false, "-0.0", "0+2i", 0.0, negZero},
		{"*", true, "-0.0", "0+1/3i", 0.0, negZero},
		{"*", false, "-0.0", "0+1/3i", 0.0, negZero},
		{"*", true, "+inf.0", "1+1/3i", math.Inf(1), math.Inf(1)},
		{"*", false, "+inf.0", "1+1/3i", math.Inf(1), math.Inf(1)},
		{"*", true, "+inf.0", "0+2i", 0.0, math.Inf(1)},
		{"*", false, "+inf.0", "0+2i", 0.0, math.Inf(1)},
		{"*", true, "+inf.0", "0+1/3i", 0.0, math.Inf(1)},
		{"*", false, "+inf.0", "0+1/3i", 0.0, math.Inf(1)},
		{"*", true, "+nan.0", "1+1/3i", math.NaN(), math.NaN()},
		{"*", false, "+nan.0", "1+1/3i", math.NaN(), math.NaN()},
		{"*", true, "+nan.0", "0+2i", 0.0, math.NaN()},
		{"*", false, "+nan.0", "0+2i", 0.0, math.NaN()},
		{"*", true, "+nan.0", "0+1/3i", 0.0, math.NaN()},
		{"*", false, "+nan.0", "0+1/3i", 0.0, math.NaN()},
		{"*", true, "1.0+2.0i", "1+1/3i", 0.33333333333333337, 2.3333333333333335},
		{"*", false, "1.0+2.0i", "1+1/3i", 0.33333333333333337, 2.3333333333333335},
		{"*", true, "1.0+2.0i", "0+2i", -4.0, 2.0},
		{"*", false, "1.0+2.0i", "0+2i", -4.0, 2.0},
		{"*", true, "1.0+2.0i", "0+1/3i", -0.6666666666666666, 0.3333333333333333},
		{"*", false, "1.0+2.0i", "0+1/3i", -0.6666666666666666, 0.3333333333333333},
		{"*", true, "-1.0-0.0i", "1+1/3i", -1.0, -0.3333333333333333},
		{"*", false, "-1.0-0.0i", "1+1/3i", -1.0, -0.3333333333333333},
		{"*", true, "-1.0-0.0i", "0+2i", 0.0, -2.0},
		{"*", false, "-1.0-0.0i", "0+2i", 0.0, -2.0},
		{"*", true, "-1.0-0.0i", "0+1/3i", 0.0, -0.3333333333333333},
		{"*", false, "-1.0-0.0i", "0+1/3i", 0.0, -0.3333333333333333},
		{"/", true, "1.0", "1+1/3i", 0.8999999999999999, -0.3},
		{"/", false, "1.0", "1+1/3i", 1.0, 0.3333333333333333},
		{"/", true, "1.0", "0+2i", 0.0, -0.5},
		{"/", false, "1.0", "0+2i", 0.0, 2.0},
		{"/", true, "1.0", "0+1/3i", 0.0, -3.0},
		{"/", false, "1.0", "0+1/3i", 0.0, 0.3333333333333333},
		{"/", true, "-0.0", "1+1/3i", negZero, 0.0},
		{"/", false, "-0.0", "1+1/3i", math.Inf(-1), math.Inf(-1)},
		{"/", true, "-0.0", "0+2i", 0.0, 0.0},
		{"/", false, "-0.0", "0+2i", 0.0, math.Inf(-1)},
		{"/", true, "-0.0", "0+1/3i", 0.0, 0.0},
		{"/", false, "-0.0", "0+1/3i", 0.0, math.Inf(-1)},
		{"/", true, "+inf.0", "1+1/3i", math.Inf(1), math.Inf(-1)},
		{"/", false, "+inf.0", "1+1/3i", 0.0, 0.0},
		{"/", true, "+inf.0", "0+2i", 0.0, math.Inf(-1)},
		{"/", false, "+inf.0", "0+2i", 0.0, 0.0},
		{"/", true, "+inf.0", "0+1/3i", 0.0, math.Inf(-1)},
		{"/", false, "+inf.0", "0+1/3i", 0.0, 0.0},
		{"/", true, "+nan.0", "1+1/3i", math.NaN(), math.NaN()},
		{"/", false, "+nan.0", "1+1/3i", math.NaN(), math.NaN()},
		{"/", true, "+nan.0", "0+2i", 0.0, math.NaN()},
		{"/", false, "+nan.0", "0+2i", 0.0, math.NaN()},
		{"/", true, "+nan.0", "0+1/3i", 0.0, math.NaN()},
		{"/", false, "+nan.0", "0+1/3i", 0.0, math.NaN()},
		{"/", true, "1.0+2.0i", "1+1/3i", 1.4999999999999998, 1.5},
		{"/", false, "1.0+2.0i", "1+1/3i", 0.3333333333333333, -0.33333333333333337},
		{"/", true, "1.0+2.0i", "0+2i", 1.0, -0.5},
		{"/", false, "1.0+2.0i", "0+2i", 0.8, 0.4},
		{"/", true, "1.0+2.0i", "0+1/3i", 6.0, -3.0},
		{"/", false, "1.0+2.0i", "0+1/3i", 0.13333333333333333, 0.06666666666666667},
		{"/", true, "-1.0-0.0i", "1+1/3i", -0.8999999999999999, 0.3},
		{"/", false, "-1.0-0.0i", "1+1/3i", -1.0, -0.3333333333333333},
		{"/", true, "-1.0-0.0i", "0+2i", negZero, 0.5},
		{"/", false, "-1.0-0.0i", "0+2i", negZero, -2.0},
		{"/", true, "-1.0-0.0i", "0+1/3i", negZero, 3.0},
		{"/", false, "-1.0-0.0i", "0+1/3i", negZero, -0.3333333333333333},
	}
	for _, tc := range tcs {
		a, b := inexact[tc.f], exact[tc.z]
		name := "(" + tc.op + " " + tc.f + " " + tc.z + ")"
		if !tc.fFirst {
			a, b = b, a
			name = "(" + tc.op + " " + tc.z + " " + tc.f + ")"
		}
		t.Run(name, func(t *testing.T) {
			c := qt.New(t)
			got, err := ops[tc.op](a, b)
			c.Assert(err, qt.IsNil)
			z, ok := got.(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T %s", got, got.SchemeString()))
			assertSameFloat64(c, real(z.Value), tc.re, "real part")
			assertSameFloat64(c, imag(z.Value), tc.im, "imaginary part")
		})
	}
}

// TestFloat64ComplexDivisionIsSmith pins the divisor half of the rule: a quotient
// with a complex divisor is Smith's algorithm over the parts, as in Chez. The
// direct formula's c^2+d^2 overflows float64 here (1e400), or rounds an exact
// 2*10^400 to +inf.0 before dividing, and gives 0.0. petite and racket agree:
//
//	(let ((r (/ 1+1/3i 1e200+1e200i))) (write (list (real-part r) (imag-part r))))
func TestFloat64ComplexDivisionIsSmith(t *testing.T) {
	ten200 := values.NewBigInteger(new(big.Int).Exp(big.NewInt(10), big.NewInt(200), nil))
	exactHuge := values.NewBigComplex(ten200, ten200)
	tcs := []struct {
		name   string
		a, b   values.Number
		re, im float64
	}{
		{"(/ 1+1/3i 1e200+1e200i)",
			values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewRational(1, 3)),
			values.NewComplex(complex(1e200, 1e200)), 6.666666666666667e-201, -3.3333333333333335e-201},
		{"(/ 1.0 10^200+10^200i)", values.NewFloat(1), exactHuge, 5e-201, -5e-201},
		{"(/ 1.0+1.0i 10^200+10^200i)", values.NewComplex(complex(1, 1)), exactHuge, 1e-200, 0},
		{"(/ 10^200+1i 1e200+1e-200i)",
			values.NewBigComplex(ten200, values.NewBigIntegerFromInt64(1)),
			values.NewComplex(complex(1e200, 1e-200)), 1, 1e-200},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := tc.a.Divide(tc.b)
			c.Assert(err, qt.IsNil)
			z, ok := got.(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", got))
			assertSameFloat64(c, real(z.Value), tc.re, "real part")
			assertSameFloat64(c, imag(z.Value), tc.im, "imaginary part")
		})
	}
}

// assertSameFloat64 compares bit patterns, so a zero's sign counts, and treats
// every NaN as the same value.
func assertSameFloat64(c *qt.C, got, want float64, what string) {
	if math.IsNaN(want) {
		c.Assert(math.IsNaN(got), qt.IsTrue, qt.Commentf("%s: got %v, want NaN", what, got))
		return
	}
	c.Assert(math.Float64bits(got), qt.Equals, math.Float64bits(want), qt.Commentf("%s: got %v, want %v", what, got, want))
}
