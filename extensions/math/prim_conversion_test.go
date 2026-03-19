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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

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
		// Rational -> inexact Float
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

// TestStringToNumberComplex verifies R7RS 6.2.7: string->number handles
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

// TestNumberToStringRadix verifies R7RS 6.2.7: number->string respects radix for BigInteger.
func TestNumberToStringRadix(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"bigint hex", `(equal? (number->string (expt 2 64) 16) "10000000000000000")`, values.TrueValue},
		{"bigint binary", `(equal? (number->string (expt 2 10) 2) "10000000000")`, values.TrueValue},
		{"bigint octal", `(equal? (number->string (expt 2 9) 8) "1000")`, values.TrueValue},
		{"bigint decimal explicit", `(equal? (number->string (expt 2 10) 10) "1024")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestConversionErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"number->string string", `(number->string "hello")`},
		{"string->number integer", `(string->number 42)`},
		{"number->string bad radix", `(number->string 42 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestConversionEdgeCases covers additional edge cases for number/string conversion.
func TestConversionEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// #e prefix: exact conversion of various types
		{"#e 1.5 is exact rational", `(exact? (string->number "#e1.5"))`, values.TrueValue},
		{"#e 1.5 equals 3/2", `(= (string->number "#e1.5") 3/2)`, values.TrueValue},
		{"#e integer stays exact", `(exact? (string->number "#e42"))`, values.TrueValue},

		// #i prefix: inexact conversion of various types
		{"#i 3 is inexact", `(inexact? (string->number "#i3"))`, values.TrueValue},
		{"#i 3 equals 3.0", `(= (string->number "#i3") 3.0)`, values.TrueValue},
		{"#i rational", `(inexact? (string->number "#i1/3"))`, values.TrueValue},

		// Combined radix + exactness prefixes
		{"#e#xff is exact 255", `(= (string->number "#e#xff") 255)`, values.TrueValue},
		{"#b#e101 is exact 5", `(= (string->number "#b#e101") 5)`, values.TrueValue},

		// number->string for negative rational
		{"negative rational", `(equal? (number->string -3/5) "-3/5")`, values.TrueValue},

		// number->string for very large float
		{"very large float", `(string? (number->string 1e308))`, values.TrueValue},

		// string->number with R7RS exponent markers
		{"exponent marker s", `(= (string->number "1s2") 100.0)`, values.TrueValue},
		{"exponent marker f", `(= (string->number "1f2") 100.0)`, values.TrueValue},
		{"exponent marker d", `(= (string->number "1d2") 100.0)`, values.TrueValue},
		{"exponent marker l", `(= (string->number "1l2") 100.0)`, values.TrueValue},

		// string->number with division by zero in rational
		{"rational zero denominator", `(equal? (string->number "3/0") #f)`, values.TrueValue},

		// Round-trip: number->string->number
		{"round-trip integer", `(= (string->number (number->string 42)) 42)`, values.TrueValue},
		{"round-trip float", `(= (string->number (number->string 3.14)) 3.14)`, values.TrueValue},
		{"round-trip rational", `(= (string->number (number->string 3/5)) 3/5)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
