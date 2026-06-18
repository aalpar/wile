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
