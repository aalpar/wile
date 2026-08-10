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

func TestRationalOps(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// numerator
		{"numerator rational", `(= (numerator 3/5) 3)`, values.TrueValue},
		{"numerator integer", `(= (numerator 7) 7)`, values.TrueValue},
		{"numerator negative", `(= (numerator -3/5) -3)`, values.TrueValue},
		{"numerator inexact", `(= (numerator 0.5) 1.0)`, values.TrueValue},

		// denominator
		{"denominator rational", `(= (denominator 3/5) 5)`, values.TrueValue},
		{"denominator integer", `(= (denominator 7) 1)`, values.TrueValue},
		{"denominator inexact", `(= (denominator 0.5) 2.0)`, values.TrueValue},

		// rationalize
		{"rationalize exact", `(= (rationalize 3/10 1/10) 1/3)`, values.TrueValue},
		{"rationalize zero tolerance", `(= (rationalize 1/3 0) 1/3)`, values.TrueValue},
		{"rationalize zero result", `(= (rationalize 0 1/10) 0)`, values.TrueValue},
		{"rationalize inexact x", `(inexact? (rationalize 0.5 1/10))`, values.TrueValue},
		{"rationalize inexact y", `(inexact? (rationalize 1/3 0.1))`, values.TrueValue},
		{"rationalize negative exact", `(= (rationalize -1 1/10) -1)`, values.TrueValue},

		// exact-integer-sqrt (returns two values)
		{"exact-integer-sqrt 14",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt 14)) list) '(3 5))`,
			values.TrueValue},
		{"exact-integer-sqrt perfect square",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt 4)) list) '(2 0))`,
			values.TrueValue},
		{"exact-integer-sqrt zero",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt 0)) list) '(0 0))`,
			values.TrueValue},
		{"exact-integer-sqrt 100",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt 100)) list) '(10 0))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestL18_RationalToInexactPrecision tests L18 fix for rational precision.
func TestL18_RationalToInexactPrecision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Large rationals preserve magnitude
		{"large rational magnitude", `(> (inexact (/ (expt 2 100) 3)) 1e29)`, values.TrueValue},
		{"very large rational", `(> (inexact (/ (expt 10 50) 7)) 1e48)`, values.TrueValue},

		// Small rationals still work
		{"1/3 approximation", `(< (abs (- (inexact (/ 1 3)) 0.333333)) 0.001)`, values.TrueValue},
		{"1/2 exact", `(= (inexact (/ 1 2)) 0.5)`, values.TrueValue},

		// Exactness contagion
		{"inexact rational is inexact", `(inexact? (inexact (/ 1 3)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestExactIntegerSqrtBigInteger covers the BigInteger case in PrimExactIntegerSqrt.
func TestExactIntegerSqrtBigInteger(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// BigInteger case: expt 2 100 returns a BigInteger
		{"exact-integer-sqrt bigint perfect square",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt (expt 2 100))) list)
			         (list (expt 2 50) 0))`,
			values.TrueValue},
		{"exact-integer-sqrt bigint non-perfect",
			`(let-values (((s r) (exact-integer-sqrt (+ (expt 2 100) 1))))
			   (= r 1))`,
			values.TrueValue},

		// The int64 arm used to correct a float64 seed with `(s+1)*(s+1) <= v`,
		// which WRAPS for v >= 3037000499². 9223372030926249001 is exactly
		// that square and is the smallest wrong answer: it returned
		// (14564942733 26161807488), a pair that satisfies n = s² + r only
		// modulo 2^64. Assert the R7RS §6.2.6 invariant itself, in exact
		// arithmetic, rather than the literal pair — a wrapped answer passes
		// an == check against whatever it happened to produce.
		{"exact-integer-sqrt at the int64 overflow threshold",
			`(let-values (((s r) (exact-integer-sqrt 9223372030926249001)))
			   (and (= s 3037000499) (= r 0)
			        (= (+ (* s s) r) 9223372030926249001)
			        (< 9223372030926249001 (* (+ s 1) (+ s 1)))))`,
			values.TrueValue},
		// One below the threshold: correct before the fix, so this is the
		// control that shows the boundary is where the report says it is.
		{"exact-integer-sqrt just below the threshold",
			`(let-values (((s r) (exact-integer-sqrt 9223372030926249000)))
			   (and (= s 3037000498) (= (+ (* s s) r) 9223372030926249000)))`,
			values.TrueValue},
		// At MaxInt64 the wrapped guard was true for EVERY int64, so this did
		// not merely answer wrongly — it never returned.
		{"exact-integer-sqrt at MaxInt64 terminates",
			`(let-values (((s r) (exact-integer-sqrt 9223372036854775807)))
			   (and (= s 3037000499) (= r 5928526806)
			        (= (+ (* s s) r) 9223372036854775807)))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestExactIntegerSqrtBigIntegerNegative covers the error path for negative BigInteger.
func TestExactIntegerSqrtBigIntegerNegative(t *testing.T) {
	engine := newEngine(t)
	evalExpectError(t, engine, `(exact-integer-sqrt (- (expt 2 100)))`)
}

func TestRationalErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"numerator string", `(numerator "hello")`},
		{"denominator string", `(denominator "hello")`},
		{"exact-integer-sqrt negative", `(exact-integer-sqrt -1)`},
		{"exact-integer-sqrt float", `(exact-integer-sqrt 1.5)`},
		{"numerator infinity", `(numerator +inf.0)`},
		{"denominator nan", `(denominator +nan.0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestRationalEdgeCases covers additional edge cases for rational operations.
func TestRationalEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// rationalize with large tolerance
		{"rationalize large tolerance", `(= (rationalize 3/7 1) 0)`, values.TrueValue},

		// rationalize negative range
		{"rationalize negative", `(= (rationalize -3/10 1/10) -1/3)`, values.TrueValue},

		// numerator/denominator of zero
		{"numerator zero", `(= (numerator 0) 0)`, values.TrueValue},
		{"denominator zero", `(= (denominator 0) 1)`, values.TrueValue},

		// exact-integer-sqrt of 1
		{"exact-integer-sqrt 1",
			`(equal? (call-with-values (lambda () (exact-integer-sqrt 1)) list) '(1 0))`,
			values.TrueValue},

		// exact-integer-sqrt of large non-perfect-square
		{"exact-integer-sqrt large non-perfect",
			`(let-values (((s r) (exact-integer-sqrt 15)))
			   (and (= s 3) (= r 6)))`,
			values.TrueValue},

		// numerator of negative integer
		{"numerator negative integer", `(= (numerator -7) -7)`, values.TrueValue},

		// denominator of negative rational
		{"denominator negative rational", `(= (denominator -3/5) 5)`, values.TrueValue},

		// rationalize with both inexact arguments
		{"rationalize both inexact", `(inexact? (rationalize 0.3 0.1))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
