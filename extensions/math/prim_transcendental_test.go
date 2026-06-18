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

func TestTranscendental(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// exp
		{"exp zero", `(= (exp 0) 1.0)`, values.TrueValue},
		{"exp one", `(< (abs (- (exp 1) 2.718281828459045)) 1e-10)`, values.TrueValue},
		{"exp negative", `(< (abs (- (exp -1) 0.36787944117144233)) 1e-10)`, values.TrueValue},

		// log
		{"log one", `(< (abs (log 1)) 1e-10)`, values.TrueValue},
		{"log e", `(< (abs (- (log 2.718281828459045) 1.0)) 1e-10)`, values.TrueValue},
		{"log base 2", `(< (abs (- (log 8 2) 3.0)) 1e-10)`, values.TrueValue},

		// sin
		{"sin zero", `(< (abs (sin 0)) 1e-10)`, values.TrueValue},
		{"sin pi/2", `(< (abs (- (sin 1.5707963267948966) 1.0)) 1e-10)`, values.TrueValue},

		// cos
		{"cos zero", `(< (abs (- (cos 0) 1.0)) 1e-10)`, values.TrueValue},
		{"cos pi", `(< (abs (- (cos 3.141592653589793) -1.0)) 1e-10)`, values.TrueValue},

		// tan
		{"tan zero", `(< (abs (tan 0)) 1e-10)`, values.TrueValue},
		{"tan pi/4", `(< (abs (- (tan 0.7853981633974483) 1.0)) 1e-10)`, values.TrueValue},

		// asin
		{"asin zero", `(< (abs (asin 0)) 1e-10)`, values.TrueValue},
		{"asin one", `(< (abs (- (asin 1) 1.5707963267948966)) 1e-10)`, values.TrueValue},

		// acos
		{"acos one", `(< (abs (acos 1)) 1e-10)`, values.TrueValue},
		{"acos zero", `(< (abs (- (acos 0) 1.5707963267948966)) 1e-10)`, values.TrueValue},

		// atan (single arg)
		{"atan zero", `(< (abs (atan 0)) 1e-10)`, values.TrueValue},
		{"atan one", `(< (abs (- (atan 1) 0.7853981633974483)) 1e-10)`, values.TrueValue},

		// atan (two args — atan2)
		{"atan2 diagonal", `(< (abs (- (atan 1 1) 0.7853981633974483)) 1e-10)`, values.TrueValue},
		{"atan2 y-axis", `(< (abs (- (atan 1 0) 1.5707963267948966)) 1e-10)`, values.TrueValue},
		// PR-2 migration regression: atan2 must accept lossy real
		// operands (1/3, BigFloat overflow, etc.) per R7RS §6.2.6
		// since the result is inherently inexact. It goes through
		// helpers.ToFloat64Lossy (silent truncation) rather than the
		// strict helpers.ToFloat64 (which errors on lossy inputs). A
		// future reversion would surface here as a failing test.
		{"atan2 rational y", `(< (abs (- (atan 1/3 1) 0.3217505543966422)) 1e-10)`, values.TrueValue},
		{"atan2 rational x", `(< (abs (- (atan 1 1/3) 1.2490457723982544)) 1e-10)`, values.TrueValue},
		{"atan2 rational both", `(< (abs (- (atan 1/3 2/7) 0.8621700546672261)) 1e-10)`, values.TrueValue},
		{"atan2 big float operand",
			`(< (abs (- (atan 1 (+ 1.0 (expt 10 60))) 1e-60)) 1e-50)`, values.TrueValue},

		// sqrt
		{"sqrt perfect square", `(< (abs (- (sqrt 4) 2.0)) 1e-10)`, values.TrueValue},
		{"sqrt zero", `(< (abs (sqrt 0)) 1e-10)`, values.TrueValue},
		{"sqrt irrational", `(< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10)`, values.TrueValue},
		{"sqrt negative real", `(< (abs (real-part (sqrt -1))) 1e-10)`, values.TrueValue},
		{"sqrt negative imag", `(< (abs (- (imag-part (sqrt -1)) 1.0)) 1e-10)`, values.TrueValue},
		{"sqrt -4 imag", `(< (abs (- (imag-part (sqrt -4)) 2.0)) 1e-10)`, values.TrueValue},

		// expt
		{"expt integer", `(= (expt 2 10) 1024)`, values.TrueValue},
		{"expt zero power", `(= (expt 5 0) 1)`, values.TrueValue},
		{"expt 0^0", `(= (expt 0 0) 1)`, values.TrueValue},
		{"expt negative exp", `(= (expt 2 -1) 1/2)`, values.TrueValue},
		{"expt rational base", `(= (expt 3/2 2) 9/4)`, values.TrueValue},
		{"expt inexact", `(< (abs (- (expt 2.0 0.5) 1.4142135623730951)) 1e-10)`, values.TrueValue},
		{"expt large result", `(= (expt 2 64) 18446744073709551616)`, values.TrueValue},

		// square
		{"square positive", `(= (square 5) 25)`, values.TrueValue},
		{"square negative", `(= (square -3) 9)`, values.TrueValue},
		{"square zero", `(= (square 0) 0)`, values.TrueValue},
		{"square rational", `(= (square 1/3) 1/9)`, values.TrueValue},
		{"square float", `(< (abs (- (square 1.5) 2.25)) 1e-10)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestL17_ExptBigIntegerPrecision tests L17 fix for large integer exponentiation.
func TestL17_ExptBigIntegerPrecision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Large integer exponentiation stays exact
		{"2^1000 is exact", `(exact? (expt 2 1000))`, values.TrueValue},
		{"2^100 is exact", `(exact? (expt 2 100))`, values.TrueValue},
		{"10^50 is exact", `(exact? (expt 10 50))`, values.TrueValue},

		// Verify correctness for small cases
		{"2^10 = 1024", `(= (expt 2 10) 1024)`, values.TrueValue},
		{"10^3 = 1000", `(= (expt 10 3) 1000)`, values.TrueValue},

		// Exactness preservation through composition
		{"(2^500)^2 is exact", `(exact? (expt (expt 2 500) 2))`, values.TrueValue},
		{"(2^500)^2 = 2^1000", `(= (expt (expt 2 500) 2) (expt 2 1000))`, values.TrueValue},

		// Negative integer exponents return exact rationals
		{"2^-1 = 1/2", `(= (expt 2 -1) 1/2)`, values.TrueValue},
		{"2^-1 is exact", `(exact? (expt 2 -1))`, values.TrueValue},

		// Fractional exponents return inexact
		{"2^0.5 is inexact", `(inexact? (expt 2 0.5))`, values.TrueValue},

		// Large base stays exact
		{"(10^20)^2 is exact", `(exact? (expt (expt 10 20) 2))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestExptAdditionalCases covers missing branches in PrimExpt:
// BigInteger base with negative exponent, rational base with negative exponent.
func TestExptAdditionalCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// BigInteger base, negative exponent → rational result
		{"bigint base negative exp", `(rational? (expt (expt 2 100) -1))`, values.TrueValue},
		{"bigint base neg exp positive", `(> (expt (expt 2 100) -1) 0)`, values.TrueValue},
		// BigInteger base, positive exponent (verifies BigInteger→BigInteger path)
		{"bigint base pos exp exact", `(exact? (expt (expt 2 100) 3))`, values.TrueValue},
		// Rational base, negative exponent (non-integer result)
		{"rational base neg exp 3/2→2/3", `(= (expt 3/2 -1) 2/3)`, values.TrueValue},
		// Rational base, negative exponent (integer result: (expt 1/3 -1) = 3)
		{"rational base neg exp to integer", `(= (expt 1/3 -1) 3)`, values.TrueValue},
		{"rational base neg exp to rational", `(= (expt 2/3 -1) 3/2)`, values.TrueValue},
		// Rational base with negative exponent > 1
		{"expt 1/2 neg exp", `(= (expt 1/2 -2) 4)`, values.TrueValue},
		{"expt 1/2 neg exp exact", `(exact? (expt 1/2 -2))`, values.TrueValue},
		// Zero base with positive exponent
		{"expt 0 positive", `(= (expt 0 5) 0)`, values.TrueValue},

		// Complex exponentiation paths
		{"complex base integer exp", `(< (magnitude (- (expt 1+1i 2) 0+2i)) 1e-10)`, values.TrueValue},
		{"complex base float exp", `(number? (expt 1+1i 0.5))`, values.TrueValue},
		{"bigcomplex base", `(number? (expt (make-rectangular 1 1) 2))`, values.TrueValue},
		{"float base complex exp", `(number? (expt 2.0 1+1i))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestSqrtExactness verifies R7RS §6.2.6: sqrt returns exact for perfect squares.
func TestSqrtExactness(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"sqrt 4 is exact 2", `(exact? (sqrt 4))`, values.TrueValue},
		{"sqrt 4 is 2", `(= (sqrt 4) 2)`, values.TrueValue},
		{"sqrt 9 is exact 3", `(exact? (sqrt 9))`, values.TrueValue},
		{"sqrt 0 is exact 0", `(exact? (sqrt 0))`, values.TrueValue},
		{"sqrt 1/4 is exact", `(exact? (sqrt 1/4))`, values.TrueValue},
		{"sqrt 1/4 is 1/2", `(= (sqrt 1/4) 1/2)`, values.TrueValue},
		{"sqrt 9/4 is 3/2", `(= (sqrt 9/4) 3/2)`, values.TrueValue},
		{"sqrt -4 is exact", `(exact? (sqrt -4))`, values.TrueValue},
		{"sqrt -4 real is 0", `(= (real-part (sqrt -4)) 0)`, values.TrueValue},
		{"sqrt -4 imag is 2", `(= (imag-part (sqrt -4)) 2)`, values.TrueValue},
		{"sqrt 2 is inexact", `(inexact? (sqrt 2))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestSqrtBigInteger verifies sqrt behavior on BigInteger inputs.
func TestSqrtBigInteger(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// BigInteger perfect square → exact BigInteger result
		{"bigint sqrt perfect square", `(exact? (sqrt (expt 2 100)))`, values.TrueValue},
		{"bigint sqrt value correct", `(= (sqrt (expt 2 100)) (expt 2 50))`, values.TrueValue},
		// BigInteger non-perfect-square → inexact result
		{"bigint sqrt non-perfect", `(inexact? (sqrt (+ (expt 2 100) 1)))`, values.TrueValue},
		// Negative BigInteger perfect square → exact BigComplex
		{"bigint neg sqrt exact", `(exact? (sqrt (* -1 (expt 2 100))))`, values.TrueValue},
		{"bigint neg sqrt real is 0", `(= (real-part (sqrt (* -1 (expt 2 100)))) 0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestComplexSqrtBranchCuts verifies complexSqrtR7RS branch cut behavior.
func TestComplexSqrtBranchCuts(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Complex input: negative real, zero imaginary → positive imaginary result (R7RS branch cut)
		{"sqrt -1+0i imag positive", `(> (imag-part (sqrt (make-rectangular -1.0 0.0))) 0)`, values.TrueValue},
		// Complex input: non-negative real → normal sqrt
		{"sqrt 4+0i real part", `(< (abs (- (real-part (sqrt (make-rectangular 4.0 0.0))) 2.0)) 1e-10)`, values.TrueValue},
		// Complex input: non-zero imaginary → general case
		{"sqrt 0+1i both branches", `(> (real-part (sqrt (make-rectangular 0.0 1.0))) 0)`, values.TrueValue},
		// BigComplex input to sqrt: must have non-zero imaginary to stay as BigComplex
		{"sqrt BigComplex -4+1i", `(> (magnitude (sqrt (make-rectangular -4 1))) 0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestTranscendentalErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		// type errors — non-numeric arguments
		{"exp string", `(exp "hello")`},
		{"sin string", `(sin "hello")`},
		{"sqrt string", `(sqrt "hello")`},
		{"expt base string", `(expt "hello" 2)`},
		{"expt exp string", `(expt 2 "hello")`},
		{"square string", `(square "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestTranscendentalEdgeCases covers edge cases for transcendental functions.
func TestTranscendentalEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// log of negative number returns complex
		{"log negative returns complex", `(number? (log -1))`, values.TrueValue},
		{"log -1 is pi*i", `(< (abs (- (imag-part (log -1)) 3.141592653589793)) 1e-10)`, values.TrueValue},
		{"log -1 real part zero", `(< (abs (real-part (log -1))) 1e-10)`, values.TrueValue},

		// sqrt of negative float returns complex
		{"sqrt -1.0 is complex", `(complex? (sqrt -1.0))`, values.TrueValue},
		{"sqrt -1.0 imag is 1", `(< (abs (- (imag-part (sqrt -1.0)) 1.0)) 1e-10)`, values.TrueValue},

		// exp of large negative value approaches zero
		{"exp large negative is near zero", `(< (exp -100) 1e-40)`, values.TrueValue},

		// exp of exact integer
		{"exp integer", `(inexact? (exp 2))`, values.TrueValue},

		// trig at boundary values
		{"sin pi is ~0", `(< (abs (sin 3.141592653589793)) 1e-10)`, values.TrueValue},
		{"cos pi/2 is ~0", `(< (abs (cos 1.5707963267948966)) 1e-10)`, values.TrueValue},

		// atan2 quadrants
		{"atan2 negative x-axis", `(< (abs (- (atan 0 -1) 3.141592653589793)) 1e-10)`, values.TrueValue},
		{"atan2 negative y", `(< (atan -1 0) 0)`, values.TrueValue},

		// asin/acos domain: values outside [-1,1] return complex
		{"asin 2 is complex", `(number? (asin 2))`, values.TrueValue},
		{"acos 2 is complex", `(number? (acos 2))`, values.TrueValue},

		// expt with rational results
		{"expt 4 1/2 via inexact", `(< (abs (- (expt 4 0.5) 2.0)) 1e-10)`, values.TrueValue},
		{"expt 27 1/3 via inexact", `(< (abs (- (expt 27 (/ 1.0 3.0)) 3.0)) 1e-10)`, values.TrueValue},

		// sqrt of rational non-perfect-square
		{"sqrt 2/3 is inexact", `(inexact? (sqrt 2/3))`, values.TrueValue},
		{"sqrt -1/4 is exact complex", `(exact? (sqrt -1/4))`, values.TrueValue},
		{"sqrt -1/4 imag is 1/2", `(= (imag-part (sqrt -1/4)) 1/2)`, values.TrueValue},

		// expt 0^0 edge case (R7RS: returns 1)
		{"expt 0.0^0", `(= (expt 0.0 0) 1.0)`, values.TrueValue},
		{"expt 0^0.0", `(= (expt 0 0.0) 1.0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
