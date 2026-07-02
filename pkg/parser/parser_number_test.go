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

package parser

import (
	"context"
	"errors"
	"math"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// helper: parse a single datum and return its unwrapped value.
func parseSingle(t *testing.T, input string) values.Value {
	t.Helper()
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, false, strings.NewReader(input))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil, qt.Commentf("input: %s", input))
	c.Assert(syn, qt.IsNotNil, qt.Commentf("input: %s", input))
	return syn.Unwrap()
}

// helper: parse and expect an error.
func parseExpectError(t *testing.T, input string) error {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, false, strings.NewReader(input))
	_, err := p.ReadSyntax(context.TODO())
	if err == nil {
		t.Fatalf("expected error for input %q, got nil", input)
	}
	return err
}

// TestParseRational_ZeroDenominatorIsDivByZero pins the rational div-by-zero
// guards across both code paths in parseRationalWithBase. The committed fuzz
// corpus (#b0/0) takes the small-int path (parser_number.go:146); an over-range
// numerator over a zero denominator exercises the big-numerator path
// (parser_number.go:108), which the corpus never reaches. Each must surface a
// located *ParserError wrapping werr.ErrDivisionByZero.
func TestParseRational_ZeroDenominatorIsDivByZero(t *testing.T) {
	cases := []string{
		"#b0/0",                               // binary, small-int path
		"#o0/0",                               // octal, small-int path
		"#x0/0",                               // hex, small-int path
		"#x" + strings.Repeat("f", 19) + "/0", // over-range numerator -> big path
	}
	for _, src := range cases {
		err := parseExpectError(t, src)
		if !errors.Is(err, werr.ErrDivisionByZero) {
			t.Errorf("parse %q: error = %v; want errors.Is ErrDivisionByZero", src, err)
		}
		var perr *ParserError
		if !errors.As(err, &perr) {
			t.Errorf("parse %q: error %T is not a located *ParserError", src, err)
		}
	}
}

// ---------------------------------------------------------------------------
// Integer parsing
// ---------------------------------------------------------------------------

func TestParseNumber_Integers(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{name: "positive 42", input: "42", expect: values.NewInteger(42)},
		{name: "negative 7", input: "-7", expect: values.NewInteger(-7)},
		{name: "zero", input: "0", expect: values.NewInteger(0)},
		{name: "positive zero", input: "+0", expect: values.NewInteger(0)},
		{name: "negative zero", input: "-0", expect: values.NewInteger(0)},
		{name: "large positive", input: "999999", expect: values.NewInteger(999999)},
		{name: "single digit", input: "7", expect: values.NewInteger(7)},
		{name: "explicit positive", input: "+42", expect: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			c.Assert(v, valuestest.SchemeEquals, tc.expect)
		})
	}
}

// ---------------------------------------------------------------------------
// Float parsing
// ---------------------------------------------------------------------------

func TestParseNumber_Floats(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected float64
	}{
		{name: "3.14", input: "3.14", expected: 3.14},
		{name: "negative 0.5", input: "-0.5", expected: -0.5},
		{name: "leading dot", input: ".5", expected: 0.5},
		{name: "trailing dot", input: "1.", expected: 1.0},
		{name: "zero point zero", input: "0.0", expected: 0.0},
		{name: "large decimal", input: "123.456", expected: 123.456},
		{name: "signed positive decimal", input: "+1.5", expected: 1.5},
		{name: "signed negative decimal", input: "-1.5", expected: -1.5},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			f, ok := v.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(f.Value, qt.Equals, tc.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Scientific notation
// ---------------------------------------------------------------------------

func TestParseNumber_Scientific(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected float64
	}{
		{name: "1e10", input: "1e10", expected: 1e10},
		{name: "positive mantissa", input: "+2e-5", expected: 2e-5},
		{name: "negative mantissa", input: "-3e2", expected: -300.0},
		{name: "decimal mantissa", input: "1.5e3", expected: 1500.0},
		{name: "uppercase E", input: "1E10", expected: 1e10},
		{name: "negative exponent", input: "5e-3", expected: 0.005},
		{name: "positive sign exponent", input: "1e+2", expected: 100.0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			f, ok := v.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(f.Value, qt.Equals, tc.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Rational numbers
// ---------------------------------------------------------------------------

func TestParseNumber_Rationals(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		expectNum int64
		expectDen int64
	}{
		{name: "3/4", input: "3/4", expectNum: 3, expectDen: 4},
		{name: "negative 1/3", input: "-1/3", expectNum: -1, expectDen: 3},
		{name: "positive 1/2", input: "+1/2", expectNum: 1, expectDen: 2},
		{name: "22/7", input: "22/7", expectNum: 22, expectDen: 7},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			r := new(big.Rat).SetFrac64(tc.expectNum, tc.expectDen)
			expected := values.Simplify(values.NewRationalFromRat(r))
			c.Assert(v, valuestest.SchemeEquals, expected)
		})
	}
}

// Rationals that simplify to integers
func TestParseNumber_RationalSimplification(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{name: "10/2 simplifies to 5", input: "10/2", expect: values.NewInteger(5)},
		{name: "6/3 simplifies to 2", input: "6/3", expect: values.NewInteger(2)},
		{name: "4/4 simplifies to 1", input: "4/4", expect: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			c.Assert(v, valuestest.SchemeEquals, tc.expect)
		})
	}
}

// ---------------------------------------------------------------------------
// Complex numbers
// ---------------------------------------------------------------------------

func TestParseNumber_Complex(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		checkReal func(float64) bool
		checkImag func(float64) bool
	}{
		{
			name:      "1+2i exact BigComplex",
			input:     "1+2i",
			checkReal: func(f float64) bool { return f == 1.0 },
			checkImag: func(f float64) bool { return f == 2.0 },
		},
		{
			name:      "3-4i exact BigComplex",
			input:     "3-4i",
			checkReal: func(f float64) bool { return f == 3.0 },
			checkImag: func(f float64) bool { return f == -4.0 },
		},
		{
			name:      "1.5+2.5i inexact Complex",
			input:     "1.5+2.5i",
			checkReal: func(f float64) bool { return f == 1.5 },
			checkImag: func(f float64) bool { return f == 2.5 },
		},
		{
			name:      "0+1i pure imaginary via complex syntax",
			input:     "0+1i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == 1.0 },
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			num, ok := v.(values.Number)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			re, im := getComplexParts(num)
			c.Assert(tc.checkReal(re), qt.IsTrue, qt.Commentf("real=%v", re))
			c.Assert(tc.checkImag(im), qt.IsTrue, qt.Commentf("imag=%v", im))
		})
	}
}

// Complex with exact zero imaginary collapses to Float per R7RS
func TestParseNumber_ComplexZeroImaginary(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "-2.5+0i")
	f, ok := v.(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v; expected Float (exact zero imag collapse)", v, v))
	c.Assert(f.Value, qt.Equals, -2.5)
}

// R7RS §6.2.1: an EXACT zero imaginary part makes the number real, so an exact
// complex literal with a zero imaginary part reads as its exact real part —
// (real? 2+0i) => #t and the value is eqv? to the same real written directly.
// An INEXACT zero imaginary part ("2+0.0i") does NOT collapse ((real? 2+0.0i)
// => #f). This matches make-rectangular, which already collapses exact zeros.
func TestParseNumber_ExactZeroImaginaryCollapsesToReal(t *testing.T) {
	c := qt.New(t)

	// Exact integer real part → Integer (demoted from BigInteger via Simplify).
	v := parseSingle(t, "2+0i")
	iv, ok := v.(*values.Integer)
	c.Assert(ok, qt.IsTrue, qt.Commentf("2+0i: got %T: %v; expected *Integer", v, v))
	c.Assert(iv.Value, qt.Equals, int64(2))
	c.Assert(v.SchemeString(), qt.Equals, "2")

	// Pure imaginary with a zero coefficient is real 0.
	v = parseSingle(t, "0i")
	iv, ok = v.(*values.Integer)
	c.Assert(ok, qt.IsTrue, qt.Commentf("0i: got %T: %v; expected *Integer", v, v))
	c.Assert(iv.Value, qt.Equals, int64(0))

	// Exact rational real part → Rational (stays exact, not collapsed away).
	v = parseSingle(t, "3/4+0i")
	rv, ok := v.(*values.Rational)
	c.Assert(ok, qt.IsTrue, qt.Commentf("3/4+0i: got %T: %v; expected *Rational", v, v))
	c.Assert(rv.SchemeString(), qt.Equals, "3/4")

	// INEXACT zero imaginary must NOT collapse — stays an inexact Complex.
	v = parseSingle(t, "2+0.0i")
	_, ok = v.(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("2+0.0i: got %T: %v; expected *Complex (no collapse)", v, v))
}

// Pure imaginary numbers
func TestParseNumber_PureImaginary(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		checkReal func(float64) bool
		checkImag func(float64) bool
	}{
		{
			name:      "+i",
			input:     "+i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == 1.0 },
		},
		{
			name:      "-i",
			input:     "-i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == -1.0 },
		},
		{
			name:      "+3i",
			input:     "+3i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == 3.0 },
		},
		{
			name:      "-7i",
			input:     "-7i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == -7.0 },
		},
		{
			name:      "+2.5i",
			input:     "+2.5i",
			checkReal: func(f float64) bool { return f == 0.0 },
			checkImag: func(f float64) bool { return f == 2.5 },
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			num, ok := v.(values.Number)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			re, im := getComplexParts(num)
			c.Assert(tc.checkReal(re), qt.IsTrue, qt.Commentf("real=%v", re))
			c.Assert(tc.checkImag(im), qt.IsTrue, qt.Commentf("imag=%v", im))
		})
	}
}

// A pure imaginary with a rational coefficient is exact per R7RS §6.2.5
// (the coefficient is an exact <ureal R>). Before the number-parser
// unification the reader rejected these outright while string->number
// accepted them as inexact — both wrong, in different ways.
func TestParseNumber_PureImaginaryRationalIsExact(t *testing.T) {
	tcs := []struct {
		input string
		want  string
	}{
		{"+3/4i", "0+3/4i"},
		{"-3/4i", "0-3/4i"},
		{"+1/2i", "0+1/2i"},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			// Assert the semantic exactness property (IsExact), not merely the
			// representation: a *BigComplex CAN be inexact (BigFloat parts), so a
			// type assertion alone does not prove exactness. Mirrors the oracle at
			// parser_test.go:1156-1158.
			bc, ok := v.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v; expected *BigComplex", v, v))
			c.Assert(bc.IsExact(), qt.IsTrue, qt.Commentf("%s should be exact", tc.input))
			c.Assert(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// The single-source-of-truth invariant (reader and string->number agree on the
// imaginary/complex grammar) is enforced in the SOURCE by delegation: both
// parseImaginary/parseComplex call the very functions checked here. So for every
// input where the reader delegates, the two sides run identical code and can only
// diverge on tokenizer→dispatch routing. The load-bearing rows are the ones where
// the reader does NOT delegate: pure-imaginary +inf.0i / -inf.0i / +nan.0i, which
// the reader routes through dedicated tokens (parseImaginaryInf/Nan) rather than
// the pure function — this test confirms that separate path still agrees.
// SchemeString is compared (not EqualTo) so NaN forms match as text rather than
// failing IEEE self-inequality.
func TestParseNumber_ReaderAgreesWithStringParsers(t *testing.T) {
	imags := []string{
		"+3i", "-2i", "+i", "-i", "+3/4i", "-3/4i", "1.5i", "+2.0i", "1e3i",
		"+inf.0i", "-inf.0i", "+nan.0i",
	}
	cplx := []string{
		"3+4i", "1.5-2.5i", "1+i", "5-i", "0+3/4i", "3/4+1/2i",
		"1+inf.0i", "3+nan.0i", "-2.5+0i", "2+0i",
	}
	for _, s := range imags {
		t.Run("imag/"+s, func(t *testing.T) {
			c := qt.New(t)
			pure, err := ParseImaginaryStringNumber(s)
			c.Assert(err, qt.IsNil)
			rdr := parseSingle(t, s)
			c.Assert(rdr.SchemeString(), qt.Equals, pure.SchemeString())
		})
	}
	for _, s := range cplx {
		t.Run("complex/"+s, func(t *testing.T) {
			c := qt.New(t)
			pure, err := ParseComplexStringNumber(s)
			c.Assert(err, qt.IsNil)
			rdr := parseSingle(t, s)
			c.Assert(rdr.SchemeString(), qt.Equals, pure.SchemeString())
		})
	}
}

// ---------------------------------------------------------------------------
// Exactness prefixes (#e, #i)
// ---------------------------------------------------------------------------

func TestParseNumber_ExactPrefix(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "#e1.5 -> exact rational 3/2",
			input: "#e1.5",
			check: func(c *qt.C, v values.Value) {
				rat, ok := v.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				r := new(big.Rat).SetFrac64(3, 2)
				c.Assert(rat.Rat().Cmp(r) == 0, qt.IsTrue)
			},
		},
		{
			name:  "#e1.0 -> exact integer 1",
			input: "#e1.0",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(1))
			},
		},
		{
			name:  "#e42 -> exact integer (already exact, pass-through)",
			input: "#e42",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(42))
			},
		},
		{
			name:  "#e3/2 -> exact rational (already exact)",
			input: "#e3/2",
			check: func(c *qt.C, v values.Value) {
				rat, ok := v.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				r := new(big.Rat).SetFrac64(3, 2)
				c.Assert(rat.Rat().Cmp(r) == 0, qt.IsTrue)
			},
		},
		{
			name:  "#e1e2 -> exact integer 100",
			input: "#e1e2",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
				c.Assert(i.Value, qt.Equals, int64(100))
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			tc.check(c, v)
		})
	}
}

func TestParseNumber_InexactPrefix(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected float64
	}{
		{name: "#i5 -> 5.0", input: "#i5", expected: 5.0},
		{name: "#i3/2 -> 1.5", input: "#i3/2", expected: 1.5},
		{name: "#i42 -> 42.0", input: "#i42", expected: 42.0},
		{name: "#i-7 -> -7.0", input: "#i-7", expected: -7.0},
		{name: "#i1.5 -> 1.5 (already inexact)", input: "#i1.5", expected: 1.5},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			f, ok := v.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(f.Value, qt.Equals, tc.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Base prefixes (#b, #o, #x)
// ---------------------------------------------------------------------------

func TestParseNumber_BasePrefixes(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		expect values.Value
	}{
		// Binary
		{name: "#b101 -> 5", input: "#b101", expect: values.NewInteger(5)},
		{name: "#b-101 -> -5", input: "#b-101", expect: values.NewInteger(-5)},
		{name: "#b0 -> 0", input: "#b0", expect: values.NewInteger(0)},
		{name: "#b11111111 -> 255", input: "#b11111111", expect: values.NewInteger(255)},

		// Octal
		{name: "#o77 -> 63", input: "#o77", expect: values.NewInteger(63)},
		{name: "#o-77 -> -63", input: "#o-77", expect: values.NewInteger(-63)},
		{name: "#o17 -> 15", input: "#o17", expect: values.NewInteger(15)},
		{name: "#o377 -> 255", input: "#o377", expect: values.NewInteger(255)},

		// Hexadecimal
		{name: "#xFF -> 255", input: "#xFF", expect: values.NewInteger(255)},
		{name: "#x-ff -> -255", input: "#x-ff", expect: values.NewInteger(-255)},
		{name: "#xAB -> 171", input: "#xAB", expect: values.NewInteger(171)},
		{name: "#x0 -> 0", input: "#x0", expect: values.NewInteger(0)},
		{name: "#x1A -> 26", input: "#x1A", expect: values.NewInteger(26)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			c.Assert(v, valuestest.SchemeEquals, tc.expect)
		})
	}
}

// ---------------------------------------------------------------------------
// Special values: +inf.0, -inf.0, +nan.0, -nan.0
// ---------------------------------------------------------------------------

func TestParseNumber_SpecialValues(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "positive infinity",
			input: "+inf.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsInf(f.Value, 1), qt.IsTrue)
			},
		},
		{
			name:  "negative infinity",
			input: "-inf.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsInf(f.Value, -1), qt.IsTrue)
			},
		},
		{
			name:  "positive NaN",
			input: "+nan.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsNaN(f.Value), qt.IsTrue)
			},
		},
		{
			name:  "negative NaN",
			input: "-nan.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsNaN(f.Value), qt.IsTrue)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			tc.check(c, v)
		})
	}
}

// Imaginary inf/nan
func TestParseNumber_ImaginaryInfNan(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		checkImag func(float64) bool
	}{
		{
			name:      "+inf.0i",
			input:     "+inf.0i",
			checkImag: func(f float64) bool { return math.IsInf(f, 1) },
		},
		{
			name:      "-inf.0i",
			input:     "-inf.0i",
			checkImag: func(f float64) bool { return math.IsInf(f, -1) },
		},
		{
			name:      "+nan.0i",
			input:     "+nan.0i",
			checkImag: math.IsNaN,
		},
		{
			name:      "-nan.0i",
			input:     "-nan.0i",
			checkImag: math.IsNaN,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			cx, ok := v.(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(cx.Real(), qt.Equals, 0.0)
			c.Assert(tc.checkImag(cx.Imag()), qt.IsTrue, qt.Commentf("imag=%v", cx.Imag()))
		})
	}
}

// ---------------------------------------------------------------------------
// Combined exactness + base prefixes (#e#b, #x#i, etc.)
// ---------------------------------------------------------------------------

func TestParseNumber_CombinedPrefixes(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "#e#b101 -> exact 5",
			input: "#e#b101",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(5))
			},
		},
		{
			name:  "#b#e101 -> exact 5 (reverse order)",
			input: "#b#e101",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(5))
			},
		},
		{
			name:  "#i#xFF -> inexact 255.0",
			input: "#i#xFF",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(f.Value, qt.Equals, 255.0)
			},
		},
		{
			name:  "#x#iFF -> inexact 255.0 (reverse order)",
			input: "#x#iFF",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(f.Value, qt.Equals, 255.0)
			},
		},
		{
			name:  "#e#o77 -> exact 63",
			input: "#e#o77",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(63))
			},
		},
		{
			name:  "#i#b1100 -> inexact 12.0",
			input: "#i#b1100",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(f.Value, qt.Equals, 12.0)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			tc.check(c, v)
		})
	}
}

// ---------------------------------------------------------------------------
// Hash digits (#e override of hash-digit inexactness)
// ---------------------------------------------------------------------------

func TestParseNumber_HashDigits(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{name: "1## -> inexact 100.0", input: "1##", expect: values.NewFloat(100.0)},
		{name: "-1## -> inexact -100.0", input: "-1##", expect: values.NewFloat(-100.0)},
		{name: "1##.## -> inexact 100.0", input: "1##.##", expect: values.NewFloat(100.0)},
		{name: ".5## -> inexact 0.5", input: ".5##", expect: values.NewFloat(0.5)},
		{name: "1##/3 -> inexact ~33.33", input: "1##/3", expect: values.NewFloat(100.0 / 3.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			c.Assert(v, valuestest.SchemeEquals, tc.expect)
		})
	}
}

// #e overrides hash-digit-induced inexactness
func TestParseNumber_ExactOverrideHashDigits(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "#e1##")
	i, ok := v.(*values.Integer)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
	c.Assert(i.Value, qt.Equals, int64(100))
}

// ---------------------------------------------------------------------------
// Polar complex numbers
// ---------------------------------------------------------------------------

func TestParseNumber_PolarComplex(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		checkReal func(float64) bool
		checkImag func(float64) bool
	}{
		{
			name:      "1@0 -> 1+0i",
			input:     "1@0",
			checkReal: func(f float64) bool { return math.Abs(f-1.0) < 1e-10 },
			checkImag: func(f float64) bool { return math.Abs(f) < 1e-10 },
		},
		{
			name:  "2@1.5708 -> ~0+2i (pi/2)",
			input: "2@1.5708",
			checkReal: func(f float64) bool {
				return math.Abs(f) < 0.01
			},
			checkImag: func(f float64) bool {
				return math.Abs(f-2.0) < 0.01
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			cx, ok := v.(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(tc.checkReal(cx.Real()), qt.IsTrue, qt.Commentf("real=%v", cx.Real()))
			c.Assert(tc.checkImag(cx.Imag()), qt.IsTrue, qt.Commentf("imag=%v", cx.Imag()))
		})
	}
}

// ---------------------------------------------------------------------------
// Base-specific rationals
// ---------------------------------------------------------------------------

func TestParseNumber_BaseRationals(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		expectNum int64
		expectDen int64
	}{
		{name: "#b101/10 -> 5/2", input: "#b101/10", expectNum: 5, expectDen: 2},
		{name: "#o7/3 -> 7/3", input: "#o7/3", expectNum: 7, expectDen: 3},
		{name: "#x10/8 -> 16/8 -> 2", input: "#x10/8", expectNum: 2, expectDen: 1},
		{name: "#b-110/11 -> -6/3 -> -2", input: "#b-110/11", expectNum: -2, expectDen: 1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			r := new(big.Rat).SetFrac64(tc.expectNum, tc.expectDen)
			expected := values.Simplify(values.NewRationalFromRat(r))
			c.Assert(v, valuestest.SchemeEquals, expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Very large numbers (int64 overflow -> BigInteger)
// ---------------------------------------------------------------------------

func TestParseNumber_BigIntegers(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "99999999999999999999")
	bi, ok := v.(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
	expected := new(big.Int)
	expected.SetString("99999999999999999999", 10)
	c.Assert(bi.BigInt().Cmp(expected), qt.Equals, 0)
}

func TestParseNumber_NegativeBigInteger(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "-99999999999999999999")
	bi, ok := v.(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
	expected := new(big.Int)
	expected.SetString("-99999999999999999999", 10)
	c.Assert(bi.BigInt().Cmp(expected), qt.Equals, 0)
}

// ---------------------------------------------------------------------------
// Complex with inf/nan in both parts
// ---------------------------------------------------------------------------

func TestParseNumber_ComplexInfNan(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		checkReal func(float64) bool
		checkImag func(float64) bool
	}{
		{
			name:      "1.0+inf.0i",
			input:     "1.0+inf.0i",
			checkReal: func(f float64) bool { return f == 1.0 },
			checkImag: func(f float64) bool { return math.IsInf(f, 1) },
		},
		{
			name:      "1.0-inf.0i",
			input:     "1.0-inf.0i",
			checkReal: func(f float64) bool { return f == 1.0 },
			checkImag: func(f float64) bool { return math.IsInf(f, -1) },
		},
		{
			name:      "1.0+nan.0i",
			input:     "1.0+nan.0i",
			checkReal: func(f float64) bool { return f == 1.0 },
			checkImag: math.IsNaN,
		},
		{
			name:      "+inf.0+inf.0i",
			input:     "+inf.0+inf.0i",
			checkReal: func(f float64) bool { return math.IsInf(f, 1) },
			checkImag: func(f float64) bool { return math.IsInf(f, 1) },
		},
		{
			name:      "-inf.0-inf.0i",
			input:     "-inf.0-inf.0i",
			checkReal: func(f float64) bool { return math.IsInf(f, -1) },
			checkImag: func(f float64) bool { return math.IsInf(f, -1) },
		},
		{
			name:      "+inf.0+nan.0i",
			input:     "+inf.0+nan.0i",
			checkReal: func(f float64) bool { return math.IsInf(f, 1) },
			checkImag: math.IsNaN,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			num, ok := v.(values.Number)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			re, im := getComplexParts(num)
			c.Assert(tc.checkReal(re), qt.IsTrue, qt.Commentf("real=%v", re))
			c.Assert(tc.checkImag(im), qt.IsTrue, qt.Commentf("imag=%v", im))
		})
	}
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestParseNumber_ExactInfError(t *testing.T) {
	err := parseExpectError(t, "#e+inf.0")
	c := qt.New(t)
	c.Assert(strings.Contains(err.Error(), "cannot convert"), qt.IsTrue,
		qt.Commentf("error: %v", err))
}

func TestParseNumber_ExactNanError(t *testing.T) {
	err := parseExpectError(t, "#e+nan.0")
	c := qt.New(t)
	c.Assert(strings.Contains(err.Error(), "cannot convert"), qt.IsTrue,
		qt.Commentf("error: %v", err))
}

func TestParseNumber_ExactComplexInfError(t *testing.T) {
	// #e on a complex with inf should error
	_ = parseExpectError(t, "#e1.0+inf.0i")
}

// ---------------------------------------------------------------------------
// Inexact complex numbers
// ---------------------------------------------------------------------------

func TestParseNumber_InexactComplex(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "#i1+2i")
	cx, ok := v.(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
	c.Assert(cx.Real(), qt.Equals, 1.0)
	c.Assert(cx.Imag(), qt.Equals, 2.0)
}

// ---------------------------------------------------------------------------
// Exactness with rational complex parts
// ---------------------------------------------------------------------------

func TestParseNumber_ExactRationalComplex(t *testing.T) {
	c := qt.New(t)
	// 1/2+3/4i -> exact BigComplex
	v := parseSingle(t, "1/2+3/4i")
	bc, ok := v.(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
	re := bc.RealAsBigFloat().Float64Truncated()
	im := bc.ImagAsBigFloat().Float64Truncated()
	c.Assert(math.Abs(re-0.5) < 1e-10, qt.IsTrue, qt.Commentf("real=%v", re))
	c.Assert(math.Abs(im-0.75) < 1e-10, qt.IsTrue, qt.Commentf("imag=%v", im))
}

// ---------------------------------------------------------------------------
// Inexact BigInteger
// ---------------------------------------------------------------------------

func TestParseNumber_InexactBigInteger(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "#i999999999999999999")
	f, ok := v.(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
	c.Assert(f.Value > 0, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Decimal prefix (#d) -- transparent pass-through
// ---------------------------------------------------------------------------

func TestParseNumber_DecimalPrefix(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "#d42")
	c.Assert(v, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestParseNumber_DecimalPrefixNegative(t *testing.T) {
	c := qt.New(t)
	v := parseSingle(t, "#d-7")
	c.Assert(v, valuestest.SchemeEquals, values.NewInteger(-7))
}

// ---------------------------------------------------------------------------
// R7RS exponent markers (s, f, d, l)
// ---------------------------------------------------------------------------

func TestParseNumber_AlternateExponentMarkers(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected float64
	}{
		{name: "short s marker", input: "1.5s3", expected: 1500.0},
		{name: "float f marker", input: "1.5f3", expected: 1500.0},
		{name: "double d marker", input: "1.5d3", expected: 1500.0},
		{name: "long l marker", input: "1.5l3", expected: 1500.0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			f, ok := v.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(f.Value, qt.Equals, tc.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Hash digits in base-specific numbers
// ---------------------------------------------------------------------------

func TestParseNumber_HashDigitsBasePrefixes(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected float64
	}{
		{name: "#b1# -> 2.0", input: "#b1#", expected: 2.0},
		{name: "#xf# -> 240.0", input: "#xf#", expected: 240.0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			v := parseSingle(t, tc.input)
			f, ok := v.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", v, v))
			c.Assert(f.Value, qt.Equals, tc.expected)
		})
	}
}
