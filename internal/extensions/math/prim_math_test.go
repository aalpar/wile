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
	"context"
	"testing"

	"github.com/aalpar/wile"
	extmath "github.com/aalpar/wile/internal/extensions/math"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the math extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(
		wile.WithExtension(extmath.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and asserts that it produces an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

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

		// round (R7RS banker's rounding — round to even)
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

func TestNumberToString(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer decimal", `(equal? (number->string 42) "42")`, values.TrueValue},
		{"negative", `(equal? (number->string -42) "-42")`, values.TrueValue},
		{"zero", `(equal? (number->string 0) "0")`, values.TrueValue},
		{"hex", `(equal? (number->string 255 16) "ff")`, values.TrueValue},
		{"binary", `(equal? (number->string 7 2) "111")`, values.TrueValue},
		{"octal", `(equal? (number->string 8 8) "10")`, values.TrueValue},
		{"float", `(equal? (number->string 1.5) "1.5")`, values.TrueValue},
		{"float integer-valued", `(equal? (number->string 1.0) "1.0")`, values.TrueValue},
		{"positive infinity", `(equal? (number->string +inf.0) "+inf.0")`, values.TrueValue},
		{"negative infinity", `(equal? (number->string -inf.0) "-inf.0")`, values.TrueValue},
		{"nan", `(equal? (number->string +nan.0) "+nan.0")`, values.TrueValue},
		{"rational", `(equal? (number->string 3/5) "3/5")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestStringToNumber(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer", `(= (string->number "42") 42)`, values.TrueValue},
		{"negative integer", `(= (string->number "-42") -42)`, values.TrueValue},
		{"float", `(= (string->number "1.5") 1.5)`, values.TrueValue},
		{"scientific", `(= (string->number "1e2") 100.0)`, values.TrueValue},
		{"hex radix arg", `(= (string->number "ff" 16) 255)`, values.TrueValue},
		{"binary radix arg", `(= (string->number "111" 2) 7)`, values.TrueValue},
		{"octal radix arg", `(= (string->number "10" 8) 8)`, values.TrueValue},
		{"rational", `(= (string->number "3/5") 3/5)`, values.TrueValue},
		{"prefix hex", `(= (string->number "#xff") 255)`, values.TrueValue},
		{"prefix binary", `(= (string->number "#b111") 7)`, values.TrueValue},
		{"prefix exact", `(= (string->number "#e1.5") 3/2)`, values.TrueValue},
		{"prefix inexact", `(= (string->number "#i42") 42.0)`, values.TrueValue},
		{"invalid returns false", `(equal? (string->number "hello") #f)`, values.TrueValue},
		{"empty returns false", `(equal? (string->number "") #f)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestMathErrors(t *testing.T) {
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
		{"floor string", `(floor "hello")`},
		{"ceiling string", `(ceiling "hello")`},
		{"finite? string", `(finite? "hello")`},
		{"infinite? string", `(infinite? "hello")`},
		{"nan? string", `(nan? "hello")`},
		{"numerator string", `(numerator "hello")`},
		{"denominator string", `(denominator "hello")`},
		{"real-part string", `(real-part "hello")`},
		{"imag-part string", `(imag-part "hello")`},
		{"magnitude string", `(magnitude "hello")`},
		{"angle string", `(angle "hello")`},
		{"number->string string", `(number->string "hello")`},
		{"string->number integer", `(string->number 42)`},

		// domain errors
		{"exact-integer-sqrt negative", `(exact-integer-sqrt -1)`},
		{"exact-integer-sqrt float", `(exact-integer-sqrt 1.5)`},
		{"numerator infinity", `(numerator +inf.0)`},
		{"denominator nan", `(denominator +nan.0)`},

		// division by zero
		{"floor/ zero divisor", `(floor/ 10 0)`},
		{"floor-quotient zero divisor", `(floor-quotient 10 0)`},
		{"floor-remainder zero divisor", `(floor-remainder 10 0)`},
		{"truncate/ zero divisor", `(truncate/ 10 0)`},
		{"truncate-quotient zero divisor", `(truncate-quotient 10 0)`},
		{"truncate-remainder zero divisor", `(truncate-remainder 10 0)`},

		// invalid radix
		{"number->string bad radix", `(number->string 42 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}
