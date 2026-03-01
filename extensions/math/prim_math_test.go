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
	extmath "github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the math extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
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
		// Complex and BigInteger types
		{"complex", `(string? (number->string (make-rectangular 3.0 4.0)))`, values.TrueValue},
		{"biginteger", `(string? (number->string (expt 2 100)))`, values.TrueValue},
		// Scientific notation through ensureInexactDecimal
		{"scientific no decimal", `(equal? (number->string 5e-324) "5.0e-324")`, values.TrueValue},
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
		// prefix directives
		{"prefix octal", `(= (string->number "#o10") 8)`, values.TrueValue},
		{"prefix decimal", `(= (string->number "#d42") 42)`, values.TrueValue},
		{"prefix unknown", `(equal? (string->number "#z42") #f)`, values.TrueValue},
		// exactness conversions
		{"prefix exact int passthrough", `(= (string->number "#e42") 42)`, values.TrueValue},
		{"prefix exact int-valued float", `(= (string->number "#e1.0") 1)`, values.TrueValue},
		{"prefix inexact biginteger",
			`(inexact? (string->number "#i99999999999999999999999"))`, values.TrueValue},
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
		// Negative BigInteger should error — tested via evalExpectError (separate test)
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
		// Exact integers → BigComplex (toExactBigComplexPart Integer case)
		{"make-rectangular exact int real-part", `(= (real-part (make-rectangular 3 4)) 3)`, values.TrueValue},
		{"make-rectangular exact int imag-part", `(= (imag-part (make-rectangular 3 4)) 4)`, values.TrueValue},
		// BigInteger parts (toExactBigComplexPart BigInteger case)
		{"make-rectangular bigint parts", `(> (real-part (make-rectangular (expt 2 100) 1)) 0)`, values.TrueValue},
		// Rational parts (toExactBigComplexPart Rational case)
		{"make-rectangular rational parts real", `(< (abs (- (real-part (make-rectangular 3/4 1/2)) 0.75)) 1e-10)`, values.TrueValue},
		// isRealNumber with Complex (non-real) — tested via evalExpectError (separate test)
		// isRealNumber with BigComplex (non-real) — tested via evalExpectError (separate test)
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

// TestMakeRectangularComplexArgs covers isRealNumber with Complex and BigComplex inputs.
func TestMakeRectangularComplexArgs(t *testing.T) {
	engine := newEngine(t)
	// Complex (non-real) as argument — isRealNumber returns false
	evalExpectError(t, engine, `(make-rectangular (make-rectangular 1.0 1.0) 0.0)`)
	// BigComplex (non-real) as argument — isRealNumber returns false
	evalExpectError(t, engine, `(make-rectangular (make-rectangular 1 1) 0)`)
}

// TestStringToNumberInexactPrefix covers the #i prefix case for various number types
// which exercises stringToNumberMakeInexact with different types.
func TestStringToNumberInexactPrefix(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Rational → inexact Float
		{"#i rational", `(< (abs (- (string->number "#i3/5") 0.6)) 1e-10)`, values.TrueValue},
		{"#i rational exact->inexact", `(inexact? (string->number "#i3/5"))`, values.TrueValue},
		// Already float stays float
		{"#i float", `(= (string->number "#i1.5") 1.5)`, values.TrueValue},
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

// TestNumberToStringRadix verifies R7RS §6.2.7: number->string respects radix for BigInteger.
func TestNumberToStringRadix(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"bigint hex", `(number->string (expt 2 64) 16)`, values.NewString("10000000000000000")},
		{"bigint binary", `(number->string (expt 2 10) 2)`, values.NewString("10000000000")},
		{"bigint octal", `(number->string (expt 2 9) 8)`, values.NewString("1000")},
		{"bigint decimal explicit", `(number->string (expt 2 10) 10)`, values.NewString("1024")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestRealPartExactness verifies R7RS §6.2.6: real-part returns the number
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

// TestStringToNumberComplex verifies R7RS §6.2.7: string->number handles
// complex literals and special float values.
func TestStringToNumberComplex(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Special floats
		{`+inf.0`, `(infinite? (string->number "+inf.0"))`, values.TrueValue},
		{`-inf.0`, `(infinite? (string->number "-inf.0"))`, values.TrueValue},
		{`+inf.0 positive`, `(positive? (string->number "+inf.0"))`, values.TrueValue},
		{`-inf.0 negative`, `(negative? (string->number "-inf.0"))`, values.TrueValue},
		{`+nan.0`, `(nan? (string->number "+nan.0"))`, values.TrueValue},
		// Complex numbers
		{`3+4i exact`, `(exact? (string->number "3+4i"))`, values.TrueValue},
		{`3+4i real`, `(= (real-part (string->number "3+4i")) 3)`, values.TrueValue},
		{`3+4i imag`, `(= (imag-part (string->number "3+4i")) 4)`, values.TrueValue},
		{`+i`, `(= (string->number "+i") +i)`, values.TrueValue},
		{`-i`, `(= (string->number "-i") -i)`, values.TrueValue},
		{`0+i`, `(= (string->number "0+i") +i)`, values.TrueValue},
		{`inexact complex`, `(inexact? (string->number "1.5+2.5i"))`, values.TrueValue},
		{`imag inf`, `(infinite? (imag-part (string->number "1+inf.0i")))`, values.TrueValue},
		// #f on invalid
		{`invalid`, `(string->number "abc")`, values.FalseValue},
		{`bare i`, `(string->number "i")`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
