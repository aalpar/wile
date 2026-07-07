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

func TestRounding(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// floor
		{"floor integer", `(= (floor 3) 3)`, values.TrueValue},
		{"floor positive", `(= (floor 3.7) 3.0)`, values.TrueValue},
		{"floor negative", `(= (floor -3.7) -4.0)`, values.TrueValue},
		{"floor rational", `(= (floor 7/2) 3)`, values.TrueValue},
		{"floor exact output", `(exact? (floor 7/2))`, values.TrueValue},

		// ceiling
		{"ceiling integer", `(= (ceiling 3) 3)`, values.TrueValue},
		{"ceiling positive", `(= (ceiling 3.2) 4.0)`, values.TrueValue},
		{"ceiling negative", `(= (ceiling -3.7) -3.0)`, values.TrueValue},
		{"ceiling rational", `(= (ceiling 7/2) 4)`, values.TrueValue},

		// truncate
		{"truncate positive", `(= (truncate 3.7) 3.0)`, values.TrueValue},
		{"truncate negative", `(= (truncate -3.7) -3.0)`, values.TrueValue},
		{"truncate rational", `(= (truncate 7/2) 3)`, values.TrueValue},

		// round (R7RS banker's rounding -- round to even)
		{"round half-even up", `(= (round 3.5) 4.0)`, values.TrueValue},
		{"round half-even down", `(= (round 4.5) 4.0)`, values.TrueValue},
		{"round half-zero", `(= (round 0.5) 0.0)`, values.TrueValue},
		{"round above half", `(= (round 3.7) 4.0)`, values.TrueValue},
		{"round below half", `(= (round 3.2) 3.0)`, values.TrueValue},
		{"round negative half", `(= (round -3.5) -4.0)`, values.TrueValue},
		{"round rational", `(= (round 7/2) 4)`, values.TrueValue},

		// inexactness preservation
		{"floor inexact output", `(inexact? (floor 3.7))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestRoundingBigPrecision guards floor/ceiling/truncate/round against the
// pre-existing float64 round-trip bug: BigInteger inputs collapsed to inexact
// Float, large Rationals overflowed the int64 cast, and BigFloat inputs lost
// digits beyond float64 precision before the rounding op applied. All four
// operations must round at exact / big.Float precision instead. See TODO
// "BigComplex precision-loss bugs" site (3).
func TestRoundingBigPrecision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 10^30 exceeds int64 (BigInteger); (2*10^20+1)/2 is an exact half-integer
	// Rational whose integer part exceeds int64; (1.0 * 10^30) + 0.25 is a
	// BigFloat whose fractional part is invisible to a float64 round-trip.
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// BigInteger: rounding an integer is identity and stays exact.
		{"floor bigint identity", `(= (floor (expt 10 30)) (expt 10 30))`, values.TrueValue},
		{"floor bigint exact", `(exact? (floor (expt 10 30)))`, values.TrueValue},
		{"ceiling bigint identity", `(= (ceiling (expt 10 30)) (expt 10 30))`, values.TrueValue},
		{"truncate bigint identity", `(= (truncate (expt 10 30)) (expt 10 30))`, values.TrueValue},
		{"round bigint identity", `(= (round (expt 10 30)) (expt 10 30))`, values.TrueValue},

		// Rational beyond int64: no overflow, exact integer result.
		{"floor big rational", `(= (floor (/ (+ (* 2 (expt 10 20)) 1) 2)) (expt 10 20))`, values.TrueValue},
		{"floor big rational exact", `(exact? (floor (/ (+ (* 2 (expt 10 20)) 1) 2)))`, values.TrueValue},
		{"ceiling big rational", `(= (ceiling (/ (+ (* 2 (expt 10 20)) 1) 2)) (+ (expt 10 20) 1))`, values.TrueValue},
		{"truncate big rational", `(= (truncate (/ (+ (* 2 (expt 10 20)) 1) 2)) (expt 10 20))`, values.TrueValue},
		// 10^20 + 0.5 rounds to even -> 10^20 (even).
		{"round big rational half-even", `(= (round (/ (+ (* 2 (expt 10 20)) 1) 2)) (expt 10 20))`, values.TrueValue},

		// BigFloat: the 0.25 fraction survives 256-bit precision but a float64
		// round-trip would drop it and reintroduce float64's 10^30 rounding error.
		{"floor bigfloat precision", `(= (floor (+ (* 1.0 (expt 10 30)) 0.25)) (* 1.0 (expt 10 30)))`, values.TrueValue},
		{"ceiling bigfloat precision", `(= (ceiling (+ (* 1.0 (expt 10 30)) 0.25)) (+ (* 1.0 (expt 10 30)) 1))`, values.TrueValue},
		{"truncate bigfloat precision", `(= (truncate (+ (* 1.0 (expt 10 30)) 0.25)) (* 1.0 (expt 10 30)))`, values.TrueValue},
		{"round bigfloat precision", `(= (round (+ (* 1.0 (expt 10 30)) 0.25)) (* 1.0 (expt 10 30)))`, values.TrueValue},
		{"floor bigfloat inexact", `(inexact? (floor (+ (* 1.0 (expt 10 30)) 0.25)))`, values.TrueValue},

		// Negative inputs: floor/ceiling diverge, truncate differs from floor,
		// round-to-even's sign handling (Go DivMod yields a non-negative modulus).
		{"floor negative rational", `(= (floor -7/2) -4)`, values.TrueValue},
		{"ceiling negative rational", `(= (ceiling -7/2) -3)`, values.TrueValue},
		{"truncate negative differs from floor", `(= (truncate -7/2) -3)`, values.TrueValue},
		// Round-to-even across all three decision points, both signs.
		{"round even stays", `(= (round 5/2) 2)`, values.TrueValue},    // 2.5 -> 2 (even)
		{"round odd rounds up", `(= (round 7/2) 4)`, values.TrueValue}, // 3.5 -> 4 (even)
		{"round past half", `(= (round 9/5) 2)`, values.TrueValue},     // 1.8 -> 2
		{"round negative half to even", `(= (round -3/2) -2)`, values.TrueValue},
		{"round negative half to zero", `(= (round -1/2) 0)`, values.TrueValue},
		// Large negative Rational: exact, no int64 overflow, truncate != floor.
		{"truncate large negative", `(= (truncate (/ (- 1 (* 2 (expt 10 20))) 2)) (- 1 (expt 10 20)))`, values.TrueValue},
		{"truncate large negative exact", `(exact? (truncate (/ (- 1 (* 2 (expt 10 20))) 2)))`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestIntegerDivision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// floor/ (returns two values)
		{"floor/ positive",
			`(equal? (call-with-values (lambda () (floor/ 10 3)) list) '(3 1))`,
			values.TrueValue},
		{"floor/ negative dividend",
			`(equal? (call-with-values (lambda () (floor/ -10 3)) list) '(-4 2))`,
			values.TrueValue},
		{"floor/ negative divisor",
			`(equal? (call-with-values (lambda () (floor/ 10 -3)) list) '(-4 -2))`,
			values.TrueValue},
		{"floor/ inexact",
			`(equal? (call-with-values (lambda () (floor/ 10.0 3)) list) '(3.0 1.0))`,
			values.TrueValue},

		// floor-quotient
		{"floor-quotient positive", `(= (floor-quotient 10 3) 3)`, values.TrueValue},
		{"floor-quotient negative", `(= (floor-quotient -10 3) -4)`, values.TrueValue},
		{"floor-quotient exact", `(exact? (floor-quotient 10 3))`, values.TrueValue},

		// floor-remainder
		{"floor-remainder positive", `(= (floor-remainder 10 3) 1)`, values.TrueValue},
		{"floor-remainder negative", `(= (floor-remainder -10 3) 2)`, values.TrueValue},
		{"floor-remainder sign", `(= (floor-remainder 10 -3) -2)`, values.TrueValue},

		// truncate/ (returns two values)
		{"truncate/ positive",
			`(equal? (call-with-values (lambda () (truncate/ 10 3)) list) '(3 1))`,
			values.TrueValue},
		{"truncate/ negative dividend",
			`(equal? (call-with-values (lambda () (truncate/ -10 3)) list) '(-3 -1))`,
			values.TrueValue},
		{"truncate/ negative divisor",
			`(equal? (call-with-values (lambda () (truncate/ 10 -3)) list) '(-3 1))`,
			values.TrueValue},

		// truncate-quotient
		{"truncate-quotient positive", `(= (truncate-quotient 10 3) 3)`, values.TrueValue},
		{"truncate-quotient negative", `(= (truncate-quotient -10 3) -3)`, values.TrueValue},

		// truncate-remainder
		{"truncate-remainder positive", `(= (truncate-remainder 10 3) 1)`, values.TrueValue},
		{"truncate-remainder negative", `(= (truncate-remainder -10 3) -1)`, values.TrueValue},

		// inexact result paths (at least one float operand)
		{"floor-quotient inexact", `(= (floor-quotient 10.0 3) 3.0)`, values.TrueValue},
		{"floor-remainder inexact", `(= (floor-remainder 10.0 3) 1.0)`, values.TrueValue},
		{"truncate-quotient inexact", `(= (truncate-quotient 10.0 3) 3.0)`, values.TrueValue},
		{"truncate-remainder inexact", `(= (truncate-remainder 10.0 3) 1.0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestIntegerDivisionBigPrecision(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	// 10^30 exceeds int64 (BigInteger). The exact-integer division family must
	// route through big.Int rather than a float64 round-trip + int64 cast, which
	// saturates the quotient at int64 max and garbles the remainder.
	// 142857142857142857142857142857 = (10^30 - 1) / 7, so 10^30 = q*7 + 1.
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// floor family, positive big operands (floor == truncate here).
		{"floor-quotient big", `(= (floor-quotient (expt 10 30) 7) 142857142857142857142857142857)`, values.TrueValue},
		{"floor-remainder big", `(= (floor-remainder (expt 10 30) 7) 1)`, values.TrueValue},
		{"floor-quotient big exact", `(exact? (floor-quotient (expt 10 30) 7))`, values.TrueValue},
		{"floor-remainder big exact", `(exact? (floor-remainder (expt 10 30) 7))`, values.TrueValue},

		// truncate family, positive big operands.
		{"truncate-quotient big", `(= (truncate-quotient (expt 10 30) 7) 142857142857142857142857142857)`, values.TrueValue},
		{"truncate-remainder big", `(= (truncate-remainder (expt 10 30) 7) 1)`, values.TrueValue},

		// multi-value forms.
		{"floor/ big",
			`(equal? (call-with-values (lambda () (floor/ (expt 10 30) 7)) list) '(142857142857142857142857142857 1))`,
			values.TrueValue},
		{"truncate/ big",
			`(equal? (call-with-values (lambda () (truncate/ (expt 10 30) 7)) list) '(142857142857142857142857142857 1))`,
			values.TrueValue},

		// negative divisor: floor and truncate quotients diverge; floor-remainder
		// takes the divisor's sign, truncate-remainder the dividend's.
		{"floor-quotient big neg divisor", `(= (floor-quotient (expt 10 30) -7) -142857142857142857142857142858)`, values.TrueValue},
		{"floor-remainder big neg divisor", `(= (floor-remainder (expt 10 30) -7) -6)`, values.TrueValue},
		{"truncate-quotient big neg divisor", `(= (truncate-quotient (expt 10 30) -7) -142857142857142857142857142857)`, values.TrueValue},
		{"truncate-remainder big neg divisor", `(= (truncate-remainder (expt 10 30) -7) 1)`, values.TrueValue},

		// division identity n0 = q*n1 + r holds exactly at big magnitude.
		{"floor identity big",
			`(= (+ (* (floor-quotient (expt 10 30) 7) 7) (floor-remainder (expt 10 30) 7)) (expt 10 30))`,
			values.TrueValue},
		{"truncate identity big neg",
			`(= (+ (* (truncate-quotient (expt 10 30) -7) -7) (truncate-remainder (expt 10 30) -7)) (expt 10 30))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestNumericPredicates(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// finite?
		{"finite integer", `(finite? 42)`, values.TrueValue},
		{"finite float", `(finite? 3.14)`, values.TrueValue},
		{"finite rational", `(finite? 3/4)`, values.TrueValue},
		{"finite pos-inf", `(finite? +inf.0)`, values.FalseValue},
		{"finite neg-inf", `(finite? -inf.0)`, values.FalseValue},
		{"finite nan", `(finite? +nan.0)`, values.FalseValue},

		// infinite?
		{"infinite pos-inf", `(infinite? +inf.0)`, values.TrueValue},
		{"infinite neg-inf", `(infinite? -inf.0)`, values.TrueValue},
		{"infinite integer", `(infinite? 42)`, values.FalseValue},
		{"infinite nan", `(infinite? +nan.0)`, values.FalseValue},

		// nan?
		{"nan nan", `(nan? +nan.0)`, values.TrueValue},
		{"nan integer", `(nan? 42)`, values.FalseValue},
		{"nan inf", `(nan? +inf.0)`, values.FalseValue},
		{"nan float", `(nan? 3.14)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestRoundingErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"floor string", `(floor "hello")`},
		{"ceiling string", `(ceiling "hello")`},
		{"finite? string", `(finite? "hello")`},
		{"infinite? string", `(infinite? "hello")`},
		{"nan? string", `(nan? "hello")`},

		// division by zero
		{"floor/ zero divisor", `(floor/ 10 0)`},
		{"floor-quotient zero divisor", `(floor-quotient 10 0)`},
		{"floor-remainder zero divisor", `(floor-remainder 10 0)`},
		{"truncate/ zero divisor", `(truncate/ 10 0)`},
		{"truncate-quotient zero divisor", `(truncate-quotient 10 0)`},
		{"truncate-remainder zero divisor", `(truncate-remainder 10 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestRoundingEdgeCases covers additional edge cases for rounding operations.
func TestRoundingEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Rounding of infinities
		{"floor +inf.0", `(infinite? (floor +inf.0))`, values.TrueValue},
		{"ceiling -inf.0", `(infinite? (ceiling -inf.0))`, values.TrueValue},
		{"truncate +inf.0", `(infinite? (truncate +inf.0))`, values.TrueValue},
		{"round +inf.0", `(infinite? (round +inf.0))`, values.TrueValue},

		// Rounding of NaN
		{"floor +nan.0", `(nan? (floor +nan.0))`, values.TrueValue},
		{"ceiling +nan.0", `(nan? (ceiling +nan.0))`, values.TrueValue},
		{"truncate +nan.0", `(nan? (truncate +nan.0))`, values.TrueValue},
		{"round +nan.0", `(nan? (round +nan.0))`, values.TrueValue},

		// Rounding of negative zero
		{"floor -0.0", `(zero? (floor -0.0))`, values.TrueValue},
		{"ceiling -0.0", `(zero? (ceiling -0.0))`, values.TrueValue},
		{"truncate -0.0", `(zero? (truncate -0.0))`, values.TrueValue},
		{"round -0.0", `(zero? (round -0.0))`, values.TrueValue},

		// Rounding exact result for exact input
		{"floor exact integer passthrough", `(exact? (floor 5))`, values.TrueValue},
		{"ceiling exact integer passthrough", `(exact? (ceiling 5))`, values.TrueValue},
		{"truncate exact integer passthrough", `(exact? (truncate 5))`, values.TrueValue},
		{"round exact integer passthrough", `(exact? (round 5))`, values.TrueValue},

		// Rounding exact rational returns exact integer
		{"ceiling exact rational", `(exact? (ceiling 7/3))`, values.TrueValue},
		{"truncate exact rational", `(exact? (truncate 7/3))`, values.TrueValue},
		{"round exact rational", `(exact? (round 7/3))`, values.TrueValue},

		// Round banker's rounding edge cases
		{"round 2.5 to even", `(= (round 2.5) 2.0)`, values.TrueValue},
		{"round 1.5 to even", `(= (round 1.5) 2.0)`, values.TrueValue},
		{"round -0.5 to even", `(= (round -0.5) 0.0)`, values.TrueValue},
		{"round -1.5 to even", `(= (round -1.5) -2.0)`, values.TrueValue},

		// Integer division with rational arguments
		{"floor-quotient rational", `(= (floor-quotient 7/2 1) 3)`, values.TrueValue},
		{"truncate-quotient rational", `(= (truncate-quotient 7/2 1) 3)`, values.TrueValue},

		// Numeric predicate edge cases
		{"finite? zero", `(finite? 0)`, values.TrueValue},
		{"finite? negative zero", `(finite? -0.0)`, values.TrueValue},
		{"nan? zero", `(nan? 0)`, values.FalseValue},
		{"infinite? zero", `(infinite? 0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
