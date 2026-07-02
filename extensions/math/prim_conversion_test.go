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
		// Pure imaginary with a rational coefficient is EXACT (R7RS §6.2.5).
		// Before the number-parser unification this parsed as inexact 0.0+0.75i.
		{`+3/4i exact`, `(exact? (string->number "+3/4i"))`, values.TrueValue},
		{`+3/4i value`, `(= (string->number "+3/4i") (make-rectangular 0 3/4))`, values.TrueValue},
		{`-3/4i imag`, `(= (imag-part (string->number "-3/4i")) -3/4)`, values.TrueValue},
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

// --- Loss-signal primitives ---

// TestInexactLosslessQ covers the four-domain matrix of
// (inexact-lossless? n): exact-fits, exact-misses, complex with one
// or both components lossy, NaN identity.
func TestInexactLosslessQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"integer 7 lossless", `(inexact-lossless? 7)`, values.TrueValue},
		{"rational 1/2 (power-of-2 denom)", `(inexact-lossless? 1/2)`, values.TrueValue},
		{"rational 1/3 lossy", `(inexact-lossless? 1/3)`, values.FalseValue},
		{"bigint 2^53 fits exactly", `(inexact-lossless? (expt 2 53))`, values.TrueValue},
		{"bigint 10^100 lossy", `(inexact-lossless? (expt 10 100))`, values.FalseValue},
		// NaN is its own float64 identity → big.Exact; the predicate sees
		// no loss because IEEE-754 NaN propagates unchanged.
		{"NaN identity (no info lost)", `(inexact-lossless? +nan.0)`, values.TrueValue},
		// IEEE-754 specials: true ±Inf inputs convert to themselves
		// exactly (big.Exact); contrast with finite overflows that
		// saturate to ±Inf with accuracy Above/Below.
		{"+inf.0 identity (no info lost)", `(inexact-lossless? +inf.0)`, values.TrueValue},
		{"-inf.0 identity (no info lost)", `(inexact-lossless? -inf.0)`, values.TrueValue},
		{"-0.0 lossless", `(inexact-lossless? -0.0)`, values.TrueValue},
		{"complex 3+4i exact", `(inexact-lossless? 3+4i)`, values.TrueValue},
		{"complex 1/3+0i lossy real", `(inexact-lossless? (make-rectangular 1/3 0))`, values.FalseValue},
		{"bigcomplex both lossy",
			`(inexact-lossless? (make-rectangular (expt 10 100) (expt 10 100)))`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestInexactAccuracy covers polymorphic return: real input → 1 symbol,
// complex input → 2 symbols (via call-with-values).
func TestInexactAccuracy(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	realCases := []struct {
		name string
		code string
		want string
	}{
		{"integer 7 exact", `(symbol->string (inexact-accuracy 7))`, "exact"},
		{"rational 1/3 below", `(symbol->string (inexact-accuracy 1/3))`, "below"},
		{"rational 1/2 exact", `(symbol->string (inexact-accuracy 1/2))`, "exact"},
		{"bigint 10^100 above", `(symbol->string (inexact-accuracy (expt 10 100)))`, "above"},
		{"bigint -10^100 below", `(symbol->string (inexact-accuracy (- (expt 10 100))))`, "below"},
		// NaN propagates as float64 NaN; the helper reports big.Exact
		// (no info loss, just identity).
		{"NaN exact", `(symbol->string (inexact-accuracy +nan.0))`, "exact"},
		// True ±Inf inputs: float64 represents them exactly.
		{"+inf.0 exact", `(symbol->string (inexact-accuracy +inf.0))`, "exact"},
		{"-inf.0 exact", `(symbol->string (inexact-accuracy -inf.0))`, "exact"},
		{"-0.0 exact", `(symbol->string (inexact-accuracy -0.0))`, "exact"},
	}
	for _, tc := range realCases {
		t.Run("real/"+tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, `"`+tc.want+`"`)
		})
	}

	complexCases := []struct {
		name string
		code string
		want string // list-of-symbols rendered via SchemeString
	}{
		{"complex 3+4i exact-exact",
			`(call-with-values (lambda () (inexact-accuracy 3+4i)) list)`,
			"(exact exact)"},
		{"complex 1/3+1/7i below-below",
			`(call-with-values (lambda () (inexact-accuracy (make-rectangular 1/3 1/7))) list)`,
			"(below below)"},
	}
	for _, tc := range complexCases {
		t.Run("complex/"+tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestInexactWithAccuracy covers polymorphic return: real → (values
// inexact-n acc-sym); complex → (values inexact-c real-acc imag-acc).
func TestInexactWithAccuracy(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	realCases := []struct {
		name string
		code string
		want string
	}{
		{"integer 7",
			`(call-with-values (lambda () (inexact-with-accuracy 7)) list)`,
			"(7.0 exact)"},
		{"rational 1/2",
			`(call-with-values (lambda () (inexact-with-accuracy 1/2)) list)`,
			"(0.5 exact)"},
		{"rational 1/3",
			`(call-with-values (lambda () (inexact-with-accuracy 1/3)) list)`,
			"(0.3333333333333333 below)"},
		// 10^500 overflows float64 magnitude → saturates to +inf.0,
		// accuracy 'above. (10^100 fits in float64 with precision loss
		// but no overflow; tested in TestInexactLosslessQ.)
		{"bigint 10^500 saturates +inf",
			`(call-with-values (lambda () (inexact-with-accuracy (expt 10 500))) list)`,
			"(+inf.0 above)"},
	}
	for _, tc := range realCases {
		t.Run("real/"+tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}

	complexCases := []struct {
		name string
		code string
		want string
	}{
		{"complex 3+4i",
			`(call-with-values (lambda () (inexact-with-accuracy 3+4i)) list)`,
			"(3.0+4.0i exact exact)"},
		{"complex 1/3+1/7i",
			`(call-with-values (lambda () (inexact-with-accuracy (make-rectangular 1/3 1/7))) list)`,
			"(0.3333333333333333+0.14285714285714285i below below)"},
	}
	for _, tc := range complexCases {
		t.Run("complex/"+tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestComplexInexactWithAccuracy verifies the uniform 3-value variant:
// always returns (values inexact-c real-acc imag-acc) regardless of
// whether the input is real or complex.
func TestComplexInexactWithAccuracy(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"real input 7",
			`(call-with-values (lambda () (complex-inexact-with-accuracy 7)) list)`,
			"(7.0+0.0i exact exact)"},
		{"real input 1/3 (real lossy, imag exact)",
			`(call-with-values (lambda () (complex-inexact-with-accuracy 1/3)) list)`,
			"(0.3333333333333333+0.0i below exact)"},
		{"complex 3+4i",
			`(call-with-values (lambda () (complex-inexact-with-accuracy 3+4i)) list)`,
			"(3.0+4.0i exact exact)"},
		{"bigcomplex per-component lossy",
			`(call-with-values (lambda () (complex-inexact-with-accuracy (make-rectangular 1/3 1/7))) list)`,
			"(0.3333333333333333+0.14285714285714285i below below)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestPolymorphicReturnArity is the regression test the design plan
// flagged explicitly: polymorphic value-count is the whole point of
// these primitives, so arity must be asserted directly. A bug where
// (inexact-accuracy 3+4i) emits 1 value instead of 2 would render
// identically as a printed list but fails here.
func TestPolymorphicReturnArity(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want string
	}{
		// inexact-accuracy: 1 for real, 2 for complex.
		{"inexact-accuracy real -> 1",
			`(call-with-values (lambda () (inexact-accuracy 7)) (lambda args (length args)))`, "1"},
		{"inexact-accuracy complex -> 2",
			`(call-with-values (lambda () (inexact-accuracy 3+4i)) (lambda args (length args)))`, "2"},

		// inexact-with-accuracy: 2 for real, 3 for complex.
		{"inexact-with-accuracy real -> 2",
			`(call-with-values (lambda () (inexact-with-accuracy 7)) (lambda args (length args)))`, "2"},
		{"inexact-with-accuracy complex -> 3",
			`(call-with-values (lambda () (inexact-with-accuracy 3+4i)) (lambda args (length args)))`, "3"},

		// complex-inexact-with-accuracy: ALWAYS 3.
		{"complex-inexact-with-accuracy real -> 3",
			`(call-with-values (lambda () (complex-inexact-with-accuracy 7)) (lambda args (length args)))`, "3"},
		{"complex-inexact-with-accuracy complex -> 3",
			`(call-with-values (lambda () (complex-inexact-with-accuracy 3+4i)) (lambda args (length args)))`, "3"},

		// inexact-lossless?: always 1 (no polymorphism).
		{"inexact-lossless? real -> 1",
			`(call-with-values (lambda () (inexact-lossless? 7)) (lambda args (length args)))`, "1"},
		{"inexact-lossless? complex -> 1",
			`(call-with-values (lambda () (inexact-lossless? 3+4i)) (lambda args (length args)))`, "1"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestLossSignalPrimitiveErrors verifies the four primitives reject
// non-numeric inputs with werr.ErrNotANumber specifically — not just
// "some error". Each primitive wraps the sentinel via
// werr.WrapForeignErrorf, so errors.Is must match.
func TestLossSignalPrimitiveErrors(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"inexact-lossless? on string", `(inexact-lossless? "not a number")`},
		{"inexact-accuracy on bool", `(inexact-accuracy #t)`},
		{"inexact-with-accuracy on empty list", `(inexact-with-accuracy '())`},
		{"complex-inexact-with-accuracy on symbol", `(complex-inexact-with-accuracy 'foo)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := evalExpectError(t, engine, tc.code)
			c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue,
				qt.Commentf("expected ErrNotANumber, got: %v", err))
		})
	}
}

// TestLossSignalDiscoverability verifies the four primitives ship
// with workable Doc + Keywords fields by exercising the user-facing
// (apropos ...) and (procedure-documentation ...) surface. A typo in
// the Keywords slice would silently degrade discoverability — this
// test pins the contract.
func TestLossSignalDiscoverability(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// apropos finds each primitive by relevant keyword.
		{"apropos lossless -> inexact-lossless?",
			`(pair? (memq 'inexact-lossless? (apropos "lossless")))`,
			values.TrueValue},
		{"apropos accuracy -> inexact-accuracy",
			`(pair? (memq 'inexact-accuracy (apropos "accuracy")))`,
			values.TrueValue},
		{"apropos accuracy -> inexact-with-accuracy",
			`(pair? (memq 'inexact-with-accuracy (apropos "accuracy")))`,
			values.TrueValue},
		{"apropos accuracy -> complex-inexact-with-accuracy",
			`(pair? (memq 'complex-inexact-with-accuracy (apropos "accuracy")))`,
			values.TrueValue},

		// procedure-documentation returns a non-empty string for each.
		{"doc inexact-lossless? non-empty",
			`(positive? (string-length (procedure-documentation inexact-lossless?)))`,
			values.TrueValue},
		{"doc inexact-accuracy non-empty",
			`(positive? (string-length (procedure-documentation inexact-accuracy)))`,
			values.TrueValue},
		{"doc inexact-with-accuracy non-empty",
			`(positive? (string-length (procedure-documentation inexact-with-accuracy)))`,
			values.TrueValue},
		{"doc complex-inexact-with-accuracy non-empty",
			`(positive? (string-length (procedure-documentation complex-inexact-with-accuracy)))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}
