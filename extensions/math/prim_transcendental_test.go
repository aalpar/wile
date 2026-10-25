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
	"fmt"
	"strings"
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

// TestSqrtBigFloat verifies sqrt on BigFloat operands. PrimSqrt had no
// BigFloat arm and raised "expected a number but got *values.BigFloat".
//
// The rows that matter are the non-finite ones. (/ #m-1 #m0) is -inf.0 and
// must take the branch cut to 0.0+inf.0i, like (sqrt -inf.0) on the Float arm
// and in Racket and Chez; an arm that short-circuited an infinite operand
// before the sign test would answer -inf.0. A NaN BigFloat has Sign 0, so a
// sign test run before the NaN test would answer as if it were zero.
//
// The eqv? rows compare against correctly rounded literals and against the
// BigFloat kind of each special result, which the predicate rows cannot see:
// a result computed in float64, at the wrong precision, or with the sign of a
// negative zero lost satisfies every predicate above them.
func TestSqrtBigFloat(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"positive finite", `(< (abs (- (sqrt #m2) 1.4142135623730951)) 1e-10)`, values.TrueValue},
		{"perfect square value", `(= (sqrt #m4) 2)`, values.TrueValue},
		{"perfect square stays inexact", `(exact? (sqrt #m4))`, values.FalseValue},
		{"zero", `(zero? (sqrt #m0))`, values.TrueValue},
		{"negative finite is imaginary", `(= (real-part (sqrt #m-4)) 0)`, values.TrueValue},
		{"negative finite imag part", `(= (imag-part (sqrt #m-4)) 2)`, values.TrueValue},
		// The negative arm builds its result at the operand's precision. Going
		// through (*BigComplex).Sqrt would open at DefaultBigFloatPrecision and
		// return fewer digits than the positive arm. This row holds at the
		// default precision, where both constructions agree; the wider-literal
		// row below is the one that tells them apart.
		{"negative arm carries the positive arm's digits", `(equal? (imag-part (sqrt #m-2)) (sqrt #m2))`, values.TrueValue},
		// A 120-significant-digit literal is wider than DefaultBigFloatPrecision (256 bits),
		// so a negative arm that delegated to (*BigComplex).Sqrt would round
		// to 256 bits and differ from the positive arm; at the default
		// precision both arms agree whichever way they are built.
		{"negative arm keeps a wider-than-default precision", `(equal? (imag-part (sqrt #m-2.123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)) (sqrt #m2.123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890))`, values.TrueValue},
		{"positive infinity", `(let ((r (sqrt (/ #m1 #m0)))) (and (infinite? r) (> r 0)))`, values.TrueValue},
		{"negative infinity takes the branch cut", `(let ((r (sqrt (/ #m-1 #m0)))) (and (zero? (real-part r)) (infinite? (imag-part r)) (> (imag-part r) 0)))`, values.TrueValue},
		{"nan", `(nan? (sqrt (/ #m0 #m0)))`, values.TrueValue},
		// Correctly rounded values. A predicate against a float64 constant
		// cannot see a result computed at float64 precision, or at a precision
		// other than the operand's; eqv? against the literal can.
		{"golden sqrt 2", `(eqv? (sqrt #m2) 1.4142135623730950488016887242096980785696718753769480731766797379907324784621l0)`, values.TrueValue},
		{"golden sqrt 3", `(eqv? (sqrt #m3) 1.7320508075688772935274463415058723669428052538103806280558069794519330169088l0)`, values.TrueValue},
		{"golden sqrt 5", `(eqv? (sqrt #m5) 2.23606797749978969640917366873127623544061835961152572427089724541052092563782l0)`, values.TrueValue},
		// Expected text cross-checked against Python's decimal at 200 digits.
		{"golden wide operand", `(string=? (number->string (sqrt #m2.123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)) "1.4572085605747537190458491165921673542867757439927436248464565622923209398775715433421430030375202786347858342742204472055l0")`, values.TrueValue},
		// Kind and sign of the special results. eqv? distinguishes -0.0 from
		// 0.0 and a NaN or infinity BigFloat from a Float, which the
		// predicate rows above do not.
		{"negative zero keeps its sign", `(eqv? (sqrt #m-0.0) #m-0.0)`, values.TrueValue},
		{"zero is an inexact zero", `(eqv? (sqrt #m0) #m0)`, values.TrueValue},
		{"nan stays a BigFloat nan", `(eqv? (sqrt (/ #m0 #m0)) (/ #m0 #m0))`, values.TrueValue},
		{"positive infinity stays a BigFloat", `(eqv? (sqrt (/ #m1 #m0)) (/ #m1 #m0))`, values.TrueValue},
		{"negative infinity imaginary part is a BigFloat +inf", `(eqv? (imag-part (sqrt (/ #m-1 #m0))) (/ #m1 #m0))`, values.TrueValue},
		{"negative finite real part is a BigFloat zero", `(eqv? (real-part (sqrt #m-4)) #m0)`, values.TrueValue},
		{"golden negative imag part", `(eqv? (imag-part (sqrt #m-3)) 1.7320508075688772935274463415058723669428052538103806280558069794519330169088l0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestTranscendentalNonFiniteBigFloat pins the transcendentals at a NaN or
// infinite BigFloat operand, which the big kernels cannot reduce. Before the
// fix exp/sin/cos/tan of an infinity dereferenced a nil integer part, log of
// one panicked with big.ErrNaN, and a NaN read as the zero under its flag
// ((exp nan) was 1.0l0, (atan nan) 0.0l0). The answers are the IEEE ones
// racket and petite give for the Float operand; the eqv? rows also pin the
// BigFloat kind, as TestSqrtBigFloat does for sqrt.
func TestTranscendentalNonFiniteBigFloat(t *testing.T) {
	engine := newEngine(t)
	const inf, ninf, nan = "(/ #m1 #m0)", "(/ #m-1 #m0)", "(/ #m0 #m0)"
	tcs := []struct{ name, code string }{
		{"exp +inf", "(eqv? (exp " + inf + ") " + inf + ")"},
		{"exp -inf", "(eqv? (exp " + ninf + ") #m0)"},
		{"sin +inf", "(eqv? (sin " + inf + ") " + nan + ")"},
		{"cos -inf", "(eqv? (cos " + ninf + ") " + nan + ")"},
		{"tan +inf", "(eqv? (tan " + inf + ") " + nan + ")"},
		{"log +inf", "(eqv? (log " + inf + ") " + inf + ")"},
		{"log +inf base 2", "(eqv? (log " + inf + " 2) " + inf + ")"},
		{"log 8 base +inf", "(eqv? (log #m8 " + inf + ") #m0)"},
		{"atan +inf keeps full precision", "(eqv? (atan " + inf + ") (* 2 (atan #m1)))"},
		{"exp nan", "(eqv? (exp " + nan + ") " + nan + ")"},
		{"sin nan", "(eqv? (sin " + nan + ") " + nan + ")"},
		{"cos nan", "(eqv? (cos " + nan + ") " + nan + ")"},
		{"asin nan", "(eqv? (asin " + nan + ") " + nan + ")"},
		{"acos nan", "(eqv? (acos " + nan + ") " + nan + ")"},
		{"atan nan", "(eqv? (atan " + nan + ") " + nan + ")"},
		{"log nan", "(nan? (log " + nan + "))"},
		{"atan2 nan y", "(eqv? (atan " + nan + " #m1) " + nan + ")"},
		{"atan2 Float nan x", "(eqv? (atan #m1 +nan.0) " + nan + ")"},
		{"atan2 Float +inf x", "(eqv? (atan #m1 +inf.0) #m0)"},
		{"atan2 both infinite", "(eqv? (atan " + inf + " " + ninf + ") (* 3 (atan #m1)))"},
		{"magnitude nan part", "(eqv? (magnitude (make-rectangular " + nan + " #m4)) " + nan + ")"},
		{"magnitude nan beside inf is inf", "(eqv? (magnitude (make-rectangular " + nan + " " + inf + ")) " + inf + ")"},
		{"angle nan part", "(eqv? (angle (make-rectangular " + nan + " #m4)) " + nan + ")"},
		{"angle both infinite", "(eqv? (angle (make-rectangular " + inf + " " + inf + ")) (atan #m1))"},
		{"sqrt nan part", "(let ((r (sqrt (make-rectangular " + nan + " #m4)))) (and (nan? (real-part r)) (nan? (imag-part r))))"},
		{"sin of a complex with a nan part is not rescued", "(nan? (sin (make-rectangular " + nan + " #m4)))"},
		// A Float infinity beside a big operand stays on the big path: the float64
		// arm would truncate 10^400 to +inf.0 and answer atan2(inf, inf) = π/4.
		{"atan2 big y, Float +inf x", "(eqv? (atan (expt 10 400) +inf.0) #m0)"},
		{"atan2 Float +inf y, big x", "(eqv? (atan +inf.0 (expt 10 400)) (* 2 (atan #m1)))"},
		{"log big to an infinite base", "(eqv? (log #m1e400 " + inf + ") #m0)"},
		{"log tiny big to an infinite base", "(eqv? (log #m1e-400 " + inf + ") #m-0.0)"},
		{"log exact big to a Float infinite base", "(eqv? (log (expt 10 400) +inf.0) #m0)"},
		{"log inf base inf", "(eqv? (log " + inf + " " + inf + ") " + nan + ")"},
		// C99 Annex G csqrt: an infinite imaginary part wins over a NaN real part.
		{"sqrt nan + inf i", "(equal? (sqrt (make-rectangular " + nan + " " + inf + ")) (make-rectangular " + inf + " " + inf + "))"},
		{"sqrt +inf + nan i", "(let ((r (sqrt (make-rectangular " + inf + " " + nan + ")))) (and (eqv? (real-part r) " + inf + ") (nan? (imag-part r))))"},
		{"sqrt -inf + nan i", "(let ((r (sqrt (make-rectangular " + ninf + " " + nan + ")))) (and (nan? (real-part r)) (infinite? (imag-part r))))"},
		{"atan of a complex with a nan part", "(nan? (atan (make-rectangular " + nan + " #m1e400)))"},
		// A complex special result is not boxed as a real BigFloat.
		{"asin +inf stays complex", "(not (real? (asin " + inf + ")))"},
		{"log negative to a big base stays complex", "(not (real? (log #m-8 #m2)))"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(eval(t, engine, tc.code).Internal(), qt.Equals, values.TrueValue, qt.Commentf("%s", tc.code))
		})
	}
}

// TestLogTwoArgumentEdges pins two-argument log where a logarithm is zero or
// infinite. Go's complex division answers Inf/x and x/0 with a NaN imaginary
// part, so (log 8 1) was +inf.0+nan.0i; on the big path ln(1) was 9.2e-92
// rather than 0, so (log #m8 #m1) was a finite 2.26e91 and (log #m1 #m1) 1.0l0.
func TestLogTwoArgumentEdges(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct{ code, want string }{
		{"(log 8 1)", "+inf.0"},
		{"(log 0 2)", "-inf.0"},
		{"(log 8 2)", "3.0"},
		{"(log 1 1)", "+nan.0"},
		{"(log #m1)", "0.0l0"},
		{"(log #m8 #m1)", "+inf.0"},
		{"(log #m1 #m1)", "+nan.0"},
		{"(log #m8 #m2)", "3.0l0"},
	}
	for _, tc := range tcs {
		t.Run(tc.code, func(t *testing.T) {
			c := qt.New(t)
			code := `(string=? (number->string ` + tc.code + `) "` + tc.want + `")`
			c.Assert(eval(t, engine, code).Internal(), qt.Equals, values.TrueValue, qt.Commentf("%s", tc.code))
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

// TestSqrtExptBigTier pins the tier rule for the sqrt and expt arms that used to
// round-trip through float64: a non-square BigInteger or Rational operand of
// sqrt, and an expt with a BigFloat operand, yield a BigFloat (or a BigComplex
// with BigFloat parts). want is the Go type of the result; prefix is the leading
// text of its printed form. Leading digits are cross-checked against Python's
// decimal at 90 digits and, where they apply, racket and petite (whose results
// are float64, so only their leading digits are comparable).
func TestSqrtExptBigTier(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name   string
		code   string
		want   string
		prefix string
	}{
		// sqrt, negative non-square BigInteger: racket 0.0+1e+200i (was 0.0+inf.0i)
		{"sqrt neg bigint imag", `(imag-part (sqrt (- 1 (expt 10 400))))`, "*values.BigFloat", "1l+200"},
		{"sqrt neg bigint real", `(real-part (sqrt (- 1 (expt 10 400))))`, "*values.BigFloat", "0.0"},
		{"sqrt neg bigint kind", `(sqrt (- 1 (expt 10 400)))`, "*values.BigComplex", ""},
		// sqrt, Rational: racket 5.773502691896257e+199 (was +inf.0) and 1e-200 (was 0.0)
		{"sqrt huge rational", `(sqrt (/ (+ 1 (expt 10 400)) 3))`, "*values.BigFloat", "5.77350269189625764509148780501957455647601751270126876018602326483977672302"},
		{"sqrt tiny rational", `(sqrt (/ 1 (+ 1 (expt 10 400))))`, "*values.BigFloat", "1l-200"},
		{"sqrt small rational", `(sqrt 2/3)`, "*values.BigFloat", "0.81649658092772603273242802490196379732198249355222337614423085575032012581"},
		{"sqrt neg rational imag", `(imag-part (sqrt -2/3))`, "*values.BigFloat", "0.81649658092772603273242802490196379732198249355222337614423085575032012581"},
		// sqrt controls
		{"sqrt perfect square bigint", `(sqrt (expt 10 400))`, "*values.BigInteger", "1000000000"},
		{"sqrt pos bigint", `(sqrt (+ 1 (expt 10 400)))`, "*values.BigFloat", "1l+200"},
		{"sqrt perfect square rational", `(sqrt 4/9)`, "*values.Rational", "2/3"},
		// expt with a BigFloat operand (was a Float)
		{"expt bigfloat bigfloat", `(expt #m2 #m0.5)`, "*values.BigFloat", "1.41421356237309504880168872420969807856967187537694807317667973799073247846"},
		{"expt bigfloat exact int", `(expt #m2.5 2)`, "*values.BigFloat", "6.25l0"},
		{"expt bigfloat cube", `(expt #m2 3)`, "*values.BigFloat", "8.0l0"},
		{"expt bigfloat negative int", `(expt #m2 -1)`, "*values.BigFloat", "0.5l0"},
		{"expt negative bigfloat odd", `(expt #m-2 3)`, "*values.BigFloat", "-8.0l0"},
		{"expt negative bigfloat integral bigfloat", `(expt #m-2 #m3)`, "*values.BigFloat", "-8.0l0"},
		{"expt general", `(expt #m1.5 #m2.5)`, "*values.BigFloat", "2.75567596063107536047194458404412781596169091573875389448677913815733042463"},
		{"expt exact base bigfloat exponent", `(expt 2 #m0.5)`, "*values.BigFloat", "1.41421356237309504880168872420969807856967187537694807317667973799073247846"},
		{"expt bigfloat rational exponent", `(expt #m2 1/2)`, "*values.BigFloat", "1.41421356237309504880168872420969807856967187537694807317667973799073247846"},
		{"expt float base bigfloat exponent", `(expt 2.0 #m0.5)`, "*values.BigFloat", "1.41421356237309504880168872420969807856967187537694807317667973799073247846"},
		{"expt bigfloat huge exponent", `(expt #m2 (expt 10 20))`, "*values.BigFloat", "+inf.0"},
		// an odd exponent wider than the working precision keeps its parity: racket/petite -1.0 and -inf.0
		{"expt minus one odd wide exponent", `(expt #m-1 (+ 1 (expt 10 200)))`, "*values.BigFloat", "-1.0l0"},
		{"expt minus two odd wide exponent", `(expt #m-2 (+ 1 (expt 10 200)))`, "*values.BigFloat", "-inf.0"},
		// a Rational base near 1 under a wide exponent: (1+1/N)^N with N = 3*2^200,
		// Python decimal at 200 digits (it differs from e from the 61st digit on)
		{"expt rational base near one", `(let ((n (* 3 (expt 2 200)))) (expt (+ 1 (/ 1 n)) (* #m1 n)))`, "*values.BigFloat", "2.71828182845904523536028747135266249775724709369995957496696734579225416313"},
		// a narrow BigInteger root rounds at the default 256 bits, not at its 71
		{"sqrt narrow bigint", `(sqrt (+ 1 (expt 2 70)))`, "*values.BigFloat", "3.4359738368000000000014551915228366851806637543512088980422635111740352757"},
		{"expt minus one odd huge exponent", `(expt #m-1 (+ 1 (expt 10 20)))`, "*values.BigFloat", "-1.0l0"},
		{"expt beyond float64 range", `(expt #m10 #m400.5)`, "*values.BigFloat", "3.162277660168379331998893544432718533719555139325216826857504852792594438639"},
		// IEEE special values, answered in big.Float
		{"expt zero base negative exponent", `(expt #m0 -1)`, "*values.BigFloat", "+inf.0"},
		{"expt negative zero odd negative exponent", `(expt #m-0.0 -1)`, "*values.BigFloat", "-inf.0"},
		{"expt zero base positive exponent", `(expt #m0 #m0.5)`, "*values.BigFloat", "0.0"},
		{"expt infinite exponent", `(expt #m2 +inf.0)`, "*values.BigFloat", "+inf.0"},
		{"expt infinite exponent small base", `(expt #m0.5 +inf.0)`, "*values.BigFloat", "0.0"},
		// float64 rounds this base to 1.0, where pow(1, +inf) is 1.
		{"expt infinite exponent base just above one", `(expt #m1.000000000000000000000000000001 +inf.0)`, "*values.BigFloat", "+inf.0"},
		// a base of -1 has magnitude 1, so an infinite exponent leaves it 1 (Float arm: (expt -1.0 +inf.0) is 1.0)
		{"expt minus one infinite exponent", `(expt #m-1 +inf.0)`, "*values.BigFloat", "1.0l0"},
		{"expt minus one negative infinite exponent", `(expt #m-1 -inf.0)`, "*values.BigFloat", "1.0l0"},
		{"expt exact minus one bigfloat infinite exponent", `(expt -1 (/ #m1 #m0))`, "*values.BigFloat", "1.0l0"},
		{"expt nan base zero exponent", `(expt (/ #m0 #m0) 0)`, "*values.BigFloat", "1.0l0"},
		{"expt one base nan exponent", `(expt #m1 +nan.0)`, "*values.BigFloat", "1.0l0"},
		{"expt nan exponent", `(expt #m2 +nan.0)`, "*values.BigFloat", "+nan.0"},
		// negative base, non-integer exponent: stays on the complex path
		{"expt negative bigfloat fractional", `(expt #m-2 #m0.5)`, "*values.Complex", ""},
		// expt controls: no BigFloat operand, kinds unchanged
		{"expt exact", `(expt 2 10)`, "*values.Integer", "1024"},
		{"expt float", `(expt 2.0 0.5)`, "*values.Float", "1.4142135623730951"},
		{"expt float base bigint exponent", `(expt 2.0 (expt 10 20))`, "*values.Float", "+inf.0"},
		{"expt rational base float exponent", `(expt 2/3 0.5)`, "*values.Float", "0.816496580927726"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := eval(t, engine, tc.code).Internal()
			c.Assert(fmt.Sprintf("%T", result), qt.Equals, tc.want)
			c.Assert(strings.HasPrefix(result.SchemeString(), tc.prefix), qt.IsTrue,
				qt.Commentf("%s printed %s", tc.code, result.SchemeString()))
		})
	}
}

// TestSqrtExptBigTierAgreement pins identities that a result rounded at the
// wrong precision, or computed in float64, breaks: the two sqrt arms agree, and
// expt agrees with sqrt and with multiplication.
func TestSqrtExptBigTierAgreement(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"negative bigint arm carries the positive arm's digits", `(eqv? (imag-part (sqrt (- 1 (expt 10 400)))) (sqrt (- (expt 10 400) 1)))`},
		// A BigInteger root keeps the operand's bit length, so a wide operand
		// keeps every digit (408 characters here, not a 256-bit 84), on both arms.
		{"wide bigint root keeps its digits", `(= (string-length (number->string (sqrt (+ 3 (expt 10 401))))) 408)`},
		{"wide negative bigint arm carries the positive arm's digits", `(eqv? (imag-part (sqrt (- (+ 3 (expt 10 401))))) (sqrt (+ 3 (expt 10 401))))`},
		{"narrow bigint root is the 256-bit root", `(eqv? (sqrt (+ 1 (expt 2 70))) (sqrt (+ #m1 (expt 2 70))))`},
		{"narrow negative bigint arm carries the positive arm's digits", `(eqv? (imag-part (sqrt (- -1 (expt 2 70)))) (sqrt (+ 1 (expt 2 70))))`},
		{"negative rational arm carries the positive arm's digits", `(eqv? (imag-part (sqrt -2/3)) (sqrt 2/3))`},
		{"expt one half is sqrt", `(eqv? (expt #m2 #m0.5) (sqrt #m2))`},
		{"expt one half is sqrt, exact base", `(eqv? (expt 3 #m0.5) (sqrt #m3))`},
		{"expt keeps a wider-than-default operand precision", `(let ((w #m2.123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)) (eqv? (expt w 2) (* w w)))`},
		{"expt wide operand one half is sqrt", `(let ((w #m2.123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)) (eqv? (expt w #m0.5) (sqrt w)))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(eval(t, engine, tc.code).Internal(), qt.Equals, values.TrueValue)
		})
	}
}
