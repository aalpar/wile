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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

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
		// Overflow rescue: math.Exp(1000)=+Inf, but e^1000 is a finite bignum.
		{"exp overflow rescue finite", `(finite? (exp 1000))`, values.TrueValue},
		{"exp overflow rescue round-trip", `(< (abs (- (log (exp 1000)) 1000)) 1e-6)`, values.TrueValue},

		// log
		{"log one", `(< (abs (log 1)) 1e-10)`, values.TrueValue},
		{"log e", `(< (abs (- (log 2.718281828459045) 1.0)) 1e-10)`, values.TrueValue},
		{"log base 2", `(< (abs (- (log 8 2) 3.0)) 1e-10)`, values.TrueValue},
		// Overflow: log of a value beyond float64 range no longer sees +Inf input.
		// log(10^400) = 400·ln(10) ≈ 921.034.
		{"log beyond float64 range", `(< (abs (- (log (expt 10 400)) 921.0340371976184)) 1e-6)`, values.TrueValue},

		// Complex overflow rescue: cmplx.* returns +Inf/NaN on these; the big-complex
		// kernels stay finite (exp: re-part overflow; sin: imag/cosh overflow; log:
		// component beyond float64 range).
		{"exp complex real overflow finite", `(finite? (real-part (exp (make-rectangular 1000 1))))`, values.TrueValue},
		{"sin complex imag overflow finite", `(finite? (imag-part (sin (make-rectangular 1 1000))))`, values.TrueValue},
		{"log complex beyond float64", `(< (abs (- (real-part (log (make-rectangular (expt 10 400) 1))) 921.0340371976184)) 1e-3)`, values.TrueValue},

		// Big-precision constants pi and euler (256-bit bindings).
		{"pi value", `(< (abs (- pi 3.141592653589793)) 1e-10)`, values.TrueValue},
		// Agreement with an independent big-π (6·asin(1/2)) beyond float64 proves big precision.
		{"pi is big precision", `(< (abs (- pi (* 6 (asin 1/2)))) 1e-70)`, values.TrueValue},
		{"euler value", `(< (abs (- euler 2.718281828459045)) 1e-10)`, values.TrueValue},
		// log(euler) = 1 to big precision (float64 e would only give ~1e-16).
		{"euler is big precision", `(< (abs (- (log euler) 1)) 1e-70)`, values.TrueValue},

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
		// Big-tier rational in the real domain: value correct (π/6 for 1/2).
		{"asin rational in domain", `(< (abs (- (asin 1/2) 0.5235987755982989)) 1e-10)`, values.TrueValue},
		// Big-tier rational OUT of the real domain (|x|>1): kernel declines, the
		// primitive falls back to the complex path, so the result is complex.
		{"asin out-of-domain rational is complex", `(complex? (asin 3/2))`, values.TrueValue},

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
		// Big-precision atan2: huge exact operands with a finite ratio keep their
		// true angle atan(10) ≈ 1.4711 instead of overflowing to a wrong angle.
		{"atan2 big operands finite ratio",
			`(< (abs (- (atan (expt 10 401) (expt 10 400)) 1.4711276743037345)) 1e-10)`, values.TrueValue},
		// 1-arg atan on a real big operand stays on the big path (correct near π/2).
		{"atan big real operand",
			`(< (abs (- (atan (expt 10 60)) 1.5707963267948966)) 1e-10)`, values.TrueValue},
		// Huge *Rational operands: finite ratio 10 → atan(10), not an overflow to π/4.
		{"atan2 huge rational finite ratio",
			`(< (abs (- (atan (/ (expt 10 401) 3) (/ (expt 10 400) 3)) 1.4711276743037345)) 1e-10)`, values.TrueValue},
		// 1-arg atan of a BigComplex beyond float64 range: first-quadrant |z|→∞ ⇒
		// real part → π/2 (cmplx.Atan would return NaN on the truncated +Inf).
		{"atan huge bigcomplex real part",
			`(< (abs (- (real-part (atan (make-rectangular (expt 10 400) (expt 10 400)))) 1.5707963267948966)) 1e-6)`, values.TrueValue},

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

		// Negative real base with non-integer exponent → complex (R7RS §6.2.6),
		// not +nan.0 (C6). (expt -1 1/2) == i == (sqrt -1); (expt -8 1/3) has
		// magnitude 2 (a genuine complex cube root, not NaN).
		{"expt neg base half power is i", `(< (magnitude (- (expt -1 1/2) 0+1i)) 1e-10)`, values.TrueValue},
		{"expt neg base half power equals sqrt", `(< (magnitude (- (expt -1 1/2) (sqrt -1))) 1e-10)`, values.TrueValue},
		{"expt neg base cube root magnitude", `(< (abs (- (magnitude (expt -8 1/3)) 2)) 1e-10)`, values.TrueValue},
		{"expt neg base cube root not nan", `(not (nan? (magnitude (expt -8 1/3))))`, values.TrueValue},
		// Positive base + non-integer exponent stays real (unchanged).
		{"expt pos base half power real", `(< (abs (- (expt 4 1/2) 2.0)) 1e-10)`, values.TrueValue},
		// Negative base with an INTEGER-VALUED inexact exponent must stay REAL
		// (regression guard for the non-integer-only complex routing).
		{"expt neg base inexact-int exp real value", `(< (abs (- (expt -2 3.0) -8.0)) 1e-10)`, values.TrueValue},
		{"expt neg base inexact-int exp not complex", `(real? (expt -2 3.0))`, values.TrueValue},
		{"expt neg float base int exp real", `(< (abs (- (expt -2.0 3) -8.0)) 1e-10)`, values.TrueValue},
		// NaN exponent on a negative base stays REAL +nan.0 (degenerate input;
		// must not flip to a complex result via the Trunc(NaN) guard).
		{"expt neg base nan exp is nan", `(nan? (expt -2 +nan.0))`, values.TrueValue},
		{"expt neg base nan exp is real", `(real? (expt -2 +nan.0))`, values.TrueValue},
		// Inexact zero base with a negative exponent is the IEEE +inf.0, NOT the
		// exact-zero ErrDivisionByZero (which only applies to an exact 0 base).
		{"expt inexact zero base neg exp inf", `(= (expt 0.0 -1) +inf.0)`, values.TrueValue},

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

		// The int64 correction loops inside exactIntegerSqrt overflowed at the
		// top of the range. Two different non-terminations, one function.
		//
		// MaxInt64: `(root+1)*(root+1) <= n` compares a WRAPPED product, and
		// no int64 exceeds MaxInt64, so the guard was true for every value of
		// root — (sqrt 9223372036854775807) never returned.
		{"sqrt MaxInt64 terminates", `(< (abs (- (sqrt 9223372036854775807) 3037000499.9760497)) 1e-6)`, values.TrueValue},
		// MinInt64 negates to ITSELF, so the negative arm handed
		// exactIntegerSqrt a negative magnitude its loop could never satisfy.
		{"sqrt MinInt64 terminates", `(< (abs (- (imag-part (sqrt -9223372036854775808)) 3037000499.9760497)) 1e-6)`, values.TrueValue},
		{"sqrt MinInt64 real part is zero", `(= (real-part (sqrt -9223372036854775808)) 0)`, values.TrueValue},
		// 3037000499² — the first perfect square above the overflow threshold.
		// It terminated before the fix but lost exactness, answering the
		// inexact 3037000499.0 where the square one step lower answered the
		// exact 3037000498.
		{"sqrt of the threshold perfect square is exact", `(exact? (sqrt 9223372030926249001))`, values.TrueValue},
		{"sqrt of the threshold perfect square is its root", `(= (sqrt 9223372030926249001) 3037000499)`, values.TrueValue},
		{"sqrt of the negative threshold perfect square is exact", `(exact? (sqrt -9223372030926249001))`, values.TrueValue},
		{"sqrt of the negative threshold perfect square is its root", `(= (imag-part (sqrt -9223372030926249001)) 3037000499)`, values.TrueValue},
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

// TestExptZeroNegativeExponent verifies that (expt 0 <neg>) raises a clean,
// catchable ErrDivisionByZero rather than surfacing as a recovered panic /
// "internal error" from big.Rat.SetFrac(_, 0) (C7).
func TestExptZeroNegativeExponent(t *testing.T) {
	engine := newEngine(t)
	for _, code := range []string{"(expt 0 -1)", "(expt 0 -2)", "(expt 0/1 -3)"} {
		err := evalExpectError(t, engine, code)
		if !errors.Is(err, werr.ErrDivisionByZero) {
			t.Fatalf("%s: want ErrDivisionByZero, got %v", code, err)
		}
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
