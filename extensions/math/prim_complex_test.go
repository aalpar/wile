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

package math_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestComplexOps(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-rectangular
		{"make-rectangular real", `(= (real-part (make-rectangular 3.0 4.0)) 3.0)`, values.TrueValue},
		{"make-rectangular imag", `(= (imag-part (make-rectangular 3.0 4.0)) 4.0)`, values.TrueValue},
		{"make-rectangular zero imag", `(= (make-rectangular 3 0) 3)`, values.TrueValue},

		// make-polar
		{"make-polar unit real", `(< (abs (- (real-part (make-polar 1 0)) 1.0)) 1e-10)`, values.TrueValue},
		{"make-polar unit imag", `(< (abs (imag-part (make-polar 1 0))) 1e-10)`, values.TrueValue},
		{"make-polar magnitude", `(< (abs (- (magnitude (make-polar 5 1.0)) 5.0)) 1e-10)`, values.TrueValue},

		// real-part
		{"real-part integer", `(= (real-part 5) 5.0)`, values.TrueValue},
		{"real-part float", `(= (real-part 3.14) 3.14)`, values.TrueValue},

		// imag-part
		{"imag-part integer", `(= (imag-part 5) 0.0)`, values.TrueValue},
		{"imag-part float", `(= (imag-part 3.14) 0.0)`, values.TrueValue},

		// magnitude
		{"magnitude 3+4i", `(< (abs (- (magnitude (make-rectangular 3.0 4.0)) 5.0)) 1e-10)`, values.TrueValue},
		{"magnitude positive", `(= (magnitude 5) 5.0)`, values.TrueValue},
		{"magnitude negative", `(= (magnitude -5) 5.0)`, values.TrueValue},
		{"magnitude zero", `(= (magnitude 0) 0.0)`, values.TrueValue},

		// angle
		{"angle positive", `(= (angle 1) 0.0)`, values.TrueValue},
		{"angle negative", `(< (abs (- (angle -1) 3.141592653589793)) 1e-10)`, values.TrueValue},
		{"angle pure imaginary",
			`(< (abs (- (angle (make-rectangular 0.0 1.0)) 1.5707963267948966)) 1e-10)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestMagnitudeAllTypes covers all numeric type cases in PrimMagnitude.
func TestMagnitudeAllTypes(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Float case
		{"magnitude float positive", `(= (magnitude 3.14) 3.14)`, values.TrueValue},
		{"magnitude float negative", `(= (magnitude -3.14) 3.14)`, values.TrueValue},
		// Rational case
		{"magnitude rational positive", `(< (abs (- (magnitude 3/4) 0.75)) 1e-10)`, values.TrueValue},
		{"magnitude rational negative", `(< (abs (- (magnitude -3/4) 0.75)) 1e-10)`, values.TrueValue},
		// BigInteger case (expt 2 100 produces a BigInteger)
		{"magnitude biginteger", `(> (magnitude (expt 2 100)) 0)`, values.TrueValue},
		// BigComplex case: exact integers create a BigComplex via make-rectangular
		{"magnitude bigcomplex 3+4i", `(< (abs (- (magnitude (make-rectangular 3 4)) 5.0)) 1e-10)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestAngleAllTypes covers all numeric type cases in PrimAngle.
func TestAngleAllTypes(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Float cases
		{"angle float positive", `(= (angle 3.14) 0.0)`, values.TrueValue},
		{"angle float negative", `(< (abs (- (angle -3.14) 3.141592653589793)) 1e-10)`, values.TrueValue},
		// Rational cases
		{"angle rational positive", `(= (angle 3/4) 0.0)`, values.TrueValue},
		{"angle rational negative", `(< (abs (- (angle -3/4) 3.141592653589793)) 1e-10)`, values.TrueValue},
		// BigInteger cases
		{"angle biginteger positive", `(>= (magnitude (angle (expt 2 100))) 0)`, values.TrueValue},
		{"angle biginteger negative", `(> (angle (- (expt 2 100))) 3.0)`, values.TrueValue},
		// BigComplex case via exact integer make-rectangular
		{"angle bigcomplex first quadrant", `(> (angle (make-rectangular 3 4)) 0)`, values.TrueValue},
		// Overflow regression: components exceed float64 range but the ratio is
		// finite. angle(1e400 + 1e401 i) = atan(10) ≈ 1.4711, not π/4 (the value
		// the prior float64-truncating path returned once both saturated to +Inf).
		{"angle bigcomplex huge finite ratio",
			`(< (abs (- (angle (make-rectangular (expt 10 400) (expt 10 401))) 1.4711276743037345)) 1e-10)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestMakeRectangularExactTypes covers exact integer and rational inputs to make-rectangular,
// which exercise toExactBigComplexPart and create BigComplex values.
func TestMakeRectangularExactTypes(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Exact integers -> BigComplex (toExactBigComplexPart Integer case)
		{"make-rectangular exact int real-part", `(= (real-part (make-rectangular 3 4)) 3)`, values.TrueValue},
		{"make-rectangular exact int imag-part", `(= (imag-part (make-rectangular 3 4)) 4)`, values.TrueValue},
		// BigInteger parts (toExactBigComplexPart BigInteger case)
		{"make-rectangular bigint parts", `(> (real-part (make-rectangular (expt 2 100) 1)) 0)`, values.TrueValue},
		// Rational parts (toExactBigComplexPart Rational case)
		{"make-rectangular rational parts real", `(< (abs (- (real-part (make-rectangular 3/4 1/2)) 0.75)) 1e-10)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestRealPartExactness verifies R7RS 6.2.6: real-part returns the number
// itself for non-complex reals, preserving exactness.
func TestRealPartExactness(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"real-part of integer is exact", `(exact? (real-part 5))`, values.TrueValue},
		{"real-part of integer is self", `(= (real-part 5) 5)`, values.TrueValue},
		{"real-part of rational is exact", `(exact? (real-part 3/4))`, values.TrueValue},
		{"real-part of rational is self", `(= (real-part 3/4) 3/4)`, values.TrueValue},
		{"real-part of float is inexact", `(inexact? (real-part 5.0))`, values.TrueValue},
		{"imag-part of integer is exact 0", `(exact? (imag-part 5))`, values.TrueValue},
		{"imag-part of integer is 0", `(= (imag-part 5) 0)`, values.TrueValue},
		{"imag-part of float is inexact 0", `(inexact? (imag-part 5.0))`, values.TrueValue},
		{"magnitude of integer is exact", `(exact? (magnitude 5))`, values.TrueValue},
		{"magnitude of negative integer is exact", `(exact? (magnitude -5))`, values.TrueValue},
		{"magnitude of integer abs", `(= (magnitude -5) 5)`, values.TrueValue},
		{"magnitude of rational is exact", `(exact? (magnitude -3/4))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestMakeRectangularComplexArgs covers isRealNumber with Complex and BigComplex inputs.
func TestMakeRectangularComplexArgs(t *testing.T) {
	engine := newEngine(t)
	// Complex (non-real) as argument -- isRealNumber returns false
	evalExpectError(t, engine, `(make-rectangular (make-rectangular 1.0 1.0) 0.0)`)
	// BigComplex (non-real) as argument -- isRealNumber returns false
	evalExpectError(t, engine, `(make-rectangular (make-rectangular 1 1) 0)`)
}

func TestComplexErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"real-part string", `(real-part "hello")`},
		{"imag-part string", `(imag-part "hello")`},
		{"magnitude string", `(magnitude "hello")`},
		{"angle string", `(angle "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestComplexEdgeCases covers additional edge cases for complex operations.
func TestComplexEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-rectangular with mixed exact/inexact
		{"make-rectangular exact+inexact", `(inexact? (make-rectangular 3 4.0))`, values.TrueValue},
		{"make-rectangular inexact+exact", `(inexact? (make-rectangular 3.0 4))`, values.TrueValue},

		// make-rectangular both exact produces exact BigComplex
		{"make-rectangular both exact is exact", `(exact? (make-rectangular 3 4))`, values.TrueValue},

		// make-polar at various angles
		{"make-polar at pi", `(< (abs (+ (real-part (make-polar 1 3.141592653589793)) 1.0)) 1e-10)`, values.TrueValue},
		{"make-polar at pi/2", `(< (abs (- (imag-part (make-polar 1 1.5707963267948966)) 1.0)) 1e-10)`, values.TrueValue},
		{"make-polar zero magnitude", `(< (magnitude (make-polar 0 1.0)) 1e-10)`, values.TrueValue},

		// real-part/imag-part of complex with zero imaginary
		{"real-part of complex zero-imag", `(= (real-part (make-rectangular 5.0 0.0)) 5.0)`, values.TrueValue},

		// magnitude of pure imaginary
		{"magnitude pure imaginary", `(< (abs (- (magnitude (make-rectangular 0.0 5.0)) 5.0)) 1e-10)`, values.TrueValue},

		// angle in all quadrants
		{"angle Q1", `(> (angle (make-rectangular 1.0 1.0)) 0)`, values.TrueValue},
		{"angle Q2", `(> (angle (make-rectangular -1.0 1.0)) 1.5)`, values.TrueValue},
		{"angle Q3", `(< (angle (make-rectangular -1.0 -1.0)) -1.5)`, values.TrueValue},
		{"angle Q4", `(< (angle (make-rectangular 1.0 -1.0)) 0)`, values.TrueValue},

		// imag-part of rational is exact 0
		{"imag-part rational is exact 0", `(exact? (imag-part 3/4))`, values.TrueValue},
		{"imag-part rational is 0", `(= (imag-part 3/4) 0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestBigComplexTranscendentalPrecision guards magnitude and sqrt on BigComplex
// against the pre-existing bug where both truncated the components to float64
// before computing. When a component exceeds the float64 range (~1.8e308) the
// truncation overflows to +inf, so the whole result collapsed to +inf even
// though the true value is finite and representable. magnitude must route
// through (*BigComplex).Magnitude and sqrt through (*BigComplex).Sqrt, both of
// which stay at big.Float precision. See TODO "BigComplex precision-loss bugs"
// sites (1) and (2). Site (4) (angle/Phase, big atan2) is deferred.
func TestBigComplexTranscendentalPrecision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// make-rectangular of two exact parts yields a BigComplex; parts >1e308
	// overflow a float64 truncation.
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// magnitude no longer overflows to +inf on huge components.
		{"magnitude big finite", `(finite? (magnitude (make-rectangular (expt 10 400) (expt 10 400))))`, values.TrueValue},
		// |0 + 10^400 i| = 10^400 exactly (at big precision).
		//
		// The comparand is an explicit BigFloat literal (#m). It used to be
		// (* 1.0 (expt 10 400)), which relied on exact × Float promoting to BigFloat.
		// Exactness contagion now sends that to Float, so it correctly overflows to
		// +inf.0 — Chez gives +inf.0 for (* 1.0 (expt 10 400)) too. An arbitrary-
		// precision inexact value has to be ASKED for, and #m is how you ask.
		{"magnitude big value", `(= (magnitude (make-rectangular 0 (expt 10 400))) #m1e400)`, values.TrueValue},
		// magnitude of a small exact BigComplex still correct: |3+4i| = 5.
		{"magnitude small exact", `(= (magnitude (make-rectangular 3 4)) 5)`, values.TrueValue},

		// sqrt no longer overflows both components to +inf.
		{"sqrt big real finite", `(finite? (real-part (sqrt (make-rectangular (expt 10 400) (expt 10 400)))))`, values.TrueValue},
		{"sqrt big imag finite", `(finite? (imag-part (sqrt (make-rectangular (expt 10 400) (expt 10 400)))))`, values.TrueValue},
		// sqrt of the pure imaginary 2*10^400 i is 10^200 + 10^200 i:
		// a=0 => re = sqrt(|z|/2) = sqrt(10^400) = 10^200; im = b/(2 re) = 10^200.
		// #m literals again: (* 1.0 (expt 10 200)) is now a Float, i.e. the float64
		// APPROXIMATION of 10^200, while sqrt of a big exact operand returns a BigFloat
		// carrying 256 bits. Comparing them is lossless (the comparison table refuses
		// to round) and so correctly reports them UNEQUAL in the low bits. The value
		// under test is the 256-bit one, so name it with #m.
		{"sqrt big real value", `(= (real-part (sqrt (make-rectangular 0 (* 2 (expt 10 400))))) #m1e200)`, values.TrueValue},
		{"sqrt big imag value", `(= (imag-part (sqrt (make-rectangular 0 (* 2 (expt 10 400))))) #m1e200)`, values.TrueValue},
		// Roundtrip at a scale beyond float64: sqrt(z)^2 reproduces z (relative
		// error negligible). Guards the a>=0,b!=0 branch's value, not just its
		// finiteness — a wrong-but-finite result would fail this.
		{"sqrt big roundtrip", `(let ((z (make-rectangular (expt 10 400) (expt 10 400)))) (< (/ (magnitude (- (* (sqrt z) (sqrt z)) z)) (magnitude z)) 1e-50))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
