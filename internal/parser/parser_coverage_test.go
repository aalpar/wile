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
	"math"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/tokenizer"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// ---------------------------------------------------------------------------
// ParserError value methods (error.go)
// ---------------------------------------------------------------------------

func makeTestToken(src string) tokenizer.Token {
	si := syntax.NewSourceIndexes(0, 0, 0)
	ei := syntax.NewSourceIndexes(len(src), len(src), 0)
	return tokenizer.NewSimpleToken(tokenizer.TokenizerStateSymbol, src, src, &si, &ei, false, 0, false)
}

func TestParserError_SchemeString(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("foo")
	pe := NewParserError(tok, "bad thing")
	got := pe.SchemeString()
	c.Assert(strings.Contains(got, "bad thing"), qt.IsTrue)
	c.Assert(strings.Contains(got, "ParserError"), qt.IsTrue)
}

func TestParserError_IsVoid(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("x")
	pe := NewParserError(tok, "msg")
	c.Assert(pe.IsVoid(), qt.IsFalse)

	var nilPE *ParserError
	c.Assert(nilPE.IsVoid(), qt.IsTrue)
}

func TestParserError_EqualTo(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("a")
	pe1 := NewParserError(tok, "msg")
	pe2 := NewParserError(tok, "msg")
	pe3 := NewParserError(tok, "other")

	c.Assert(pe1.EqualTo(pe2), qt.IsTrue)
	c.Assert(pe1.EqualTo(pe3), qt.IsFalse)
	// Different type
	c.Assert(pe1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// ---------------------------------------------------------------------------
// Non-decimal integer parsing (parseIntegerWithBase)
// ---------------------------------------------------------------------------

func TestCoverage_BinaryIntegers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{"binary 101", "#b101", values.NewInteger(5)},
		{"binary negative", "#b-101", values.NewInteger(-5)},
		{"binary positive", "#b+1100", values.NewInteger(12)},
		{"binary zero", "#b0", values.NewInteger(0)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.Unwrap(), valuestest.SchemeEquals, tt.expect)
		})
	}
}

func TestCoverage_OctalIntegers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{"octal 17", "#o17", values.NewInteger(15)},
		{"octal negative", "#o-77", values.NewInteger(-63)},
		{"octal positive", "#o+10", values.NewInteger(8)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.Unwrap(), valuestest.SchemeEquals, tt.expect)
		})
	}
}

func TestCoverage_HexIntegers(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{"hex 1f", "#x1f", values.NewInteger(31)},
		{"hex negative ff", "#x-ff", values.NewInteger(-255)},
		{"hex uppercase", "#xAB", values.NewInteger(171)},
		{"hex zero", "#x0", values.NewInteger(0)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.Unwrap(), valuestest.SchemeEquals, tt.expect)
		})
	}
}

// ---------------------------------------------------------------------------
// Non-decimal rational parsing (parseRationalWithBase)
// ---------------------------------------------------------------------------

func TestCoverage_BinaryRationals(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name      string
		input     string
		expectNum int64
		expectDen int64
	}{
		{"binary 101/10", "#b101/10", 5, 2},
		{"binary negative", "#b-110/11", -2, 1},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			v := syn.Unwrap()
			r := new(big.Rat).SetFrac64(tt.expectNum, tt.expectDen)
			expected := values.Simplify(values.NewRationalFromRat(r))
			c.Assert(v, valuestest.SchemeEquals, expected)
		})
	}
}

func TestCoverage_OctalRationals(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("#o7/3"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	r := new(big.Rat).SetFrac64(7, 3)
	expected := values.Simplify(values.NewRationalFromRat(r))
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, expected)
}

func TestCoverage_HexRationals(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("#x10/8"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	// #x10 = 16, #x8 = 8 => 16/8 = 2
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, values.NewInteger(2))
}

// ---------------------------------------------------------------------------
// Exactness prefixes (makeExact, makeInexact)
// ---------------------------------------------------------------------------

func TestCoverage_ExactPrefix(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
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
			name:  "#e42 -> exact integer (already exact)",
			input: "#e42",
			check: func(c *qt.C, v values.Value) {
				i, ok := v.(*values.Integer)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(i.Value, qt.Equals, int64(42))
			},
		},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			tt.check(c, syn.Unwrap())
		})
	}
}

func TestCoverage_ExactInfError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("#e+inf.0"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "cannot convert"), qt.IsTrue)
}

func TestCoverage_ExactNanError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("#e+nan.0"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "cannot convert"), qt.IsTrue)
}

func TestCoverage_InexactPrefix(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name     string
		input    string
		expected float64
	}{
		{"#i5 -> 5.0", "#i5", 5.0},
		{"#i3/2 -> 1.5", "#i3/2", 1.5},
		{"#i42 -> 42.0", "#i42", 42.0},
		{"#i-7 -> -7.0", "#i-7", -7.0},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			f, ok := syn.Unwrap().(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
			c.Assert(f.Value, qt.Equals, tt.expected)
		})
	}
}

func TestCoverage_InexactFloat(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on an already-inexact float should pass through unchanged
	p := NewParser(env, false, strings.NewReader("#i1.5"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, 1.5)
}

// ---------------------------------------------------------------------------
// numberToInexact paths
// ---------------------------------------------------------------------------

func TestCoverage_HashDigitInteger(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 1## -> inexact 100.0 (already tested in hash_digit_test but exercises numberToInexact)
	p := NewParser(env, false, strings.NewReader("1##"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, 100.0)
}

// ---------------------------------------------------------------------------
// Datum labels (readLabeledList)
// ---------------------------------------------------------------------------

func TestCoverage_DatumLabelList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=(1 2 3) -> labeled list
	p := NewParser(env, true, strings.NewReader("#0=(1 2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	// Should be a SyntaxDatumLabelAssignment
	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(dla.Label, qt.Equals, 0)
}

func TestCoverage_DatumLabelCircular(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=(a . #0#) -> circular pair
	p := NewParser(env, true, strings.NewReader("#0=(a . #0#)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)

	// The value should be a pair whose cdr is the same pair (circular)
	pair, ok := dla.Value.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", dla.Value))
	// Car should be a symbol 'a'
	c.Assert(pair.Car(), qt.IsNotNil)
	// Cdr should point back to the same pair (circular reference)
	c.Assert(pair.Cdr(), qt.Equals, pair)
}

func TestCoverage_DatumLabelEmptyList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=() -> labeled empty list
	p := NewParser(env, true, strings.NewReader("#0=()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)
}

func TestCoverage_DatumLabelSingleElement(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=(42) -> labeled single-element list
	p := NewParser(env, true, strings.NewReader("#0=(42)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)
}

func TestCoverage_DatumLabelImproperMulti(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=(a b . c) -> labeled improper list with multiple elements
	p := NewParser(env, true, strings.NewReader("#0=(a b . c)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)
}

func TestCoverage_DatumLabelAtom(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=42 -> labeled atom (non-compound)
	p := NewParser(env, true, strings.NewReader("#0=42"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)
}

func TestCoverage_DatumLabelReference(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Read two datums: #0=hello then #0# (reference to label 0)
	p := NewParser(env, true, strings.NewReader("#0=hello #0#"))
	syn1, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn1, qt.IsNotNil)

	syn2, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn2, qt.IsNotNil)
}

func TestCoverage_DatumLabelVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #0=#(1 2 3) -> labeled vector
	p := NewParser(env, true, strings.NewReader("#0=#(1 2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)

	dla, ok := syn.(*syntax.SyntaxDatumLabelAssignment)
	c.Assert(ok, qt.IsTrue)
	c.Assert(dla.Label, qt.Equals, 0)
}

// ---------------------------------------------------------------------------
// Inf/NaN parsing (parseFloatOrInfnan)
// ---------------------------------------------------------------------------

func TestCoverage_InfNanStandalone(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "+inf.0",
			input: "+inf.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsInf(f.Value, 1), qt.IsTrue)
			},
		},
		{
			name:  "-inf.0",
			input: "-inf.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsInf(f.Value, -1), qt.IsTrue)
			},
		},
		{
			name:  "+nan.0",
			input: "+nan.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsNaN(f.Value), qt.IsTrue)
			},
		},
		{
			name:  "-nan.0",
			input: "-nan.0",
			check: func(c *qt.C, v values.Value) {
				f, ok := v.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(math.IsNaN(f.Value), qt.IsTrue)
			},
		},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			tt.check(c, syn.Unwrap())
		})
	}
}

func TestCoverage_InfNanImaginary(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "+inf.0i",
			input: "+inf.0i",
			check: func(c *qt.C, v values.Value) {
				cx, ok := v.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(cx.Real(), qt.Equals, 0.0)
				c.Assert(math.IsInf(cx.Imag(), 1), qt.IsTrue)
			},
		},
		{
			name:  "-inf.0i",
			input: "-inf.0i",
			check: func(c *qt.C, v values.Value) {
				cx, ok := v.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(cx.Real(), qt.Equals, 0.0)
				c.Assert(math.IsInf(cx.Imag(), -1), qt.IsTrue)
			},
		},
		{
			name:  "+nan.0i",
			input: "+nan.0i",
			check: func(c *qt.C, v values.Value) {
				cx, ok := v.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(cx.Real(), qt.Equals, 0.0)
				c.Assert(math.IsNaN(cx.Imag()), qt.IsTrue)
			},
		},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			tt.check(c, syn.Unwrap())
		})
	}
}

// ---------------------------------------------------------------------------
// ReadSyntax additional paths
// ---------------------------------------------------------------------------

func TestCoverage_MultipleReads(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("1 2 3"))
	syn1, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn1.Unwrap(), valuestest.SchemeEquals, values.NewInteger(1))

	syn2, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn2.Unwrap(), valuestest.SchemeEquals, values.NewInteger(2))

	syn3, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn3.Unwrap(), valuestest.SchemeEquals, values.NewInteger(3))
}

func TestCoverage_FoldCaseDirective(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #!fold-case should cause subsequent symbols to be lowercased
	p := NewParser(env, true, strings.NewReader("#!fold-case FOO"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	sym, ok := syn.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(sym.Datum().Key, qt.Equals, "foo")
}

func TestCoverage_NoFoldCaseDirective(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #!fold-case followed by #!no-fold-case should restore case sensitivity
	p := NewParser(env, true, strings.NewReader("#!fold-case #!no-fold-case FOO"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	sym, ok := syn.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(sym.Datum().Key, qt.Equals, "FOO")
}

// ---------------------------------------------------------------------------
// Complex number inf/nan in real + imaginary parts
// ---------------------------------------------------------------------------

func TestCoverage_ComplexInfNan(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
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
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			num, ok := syn.Unwrap().(values.Number)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
			rel, iam := getComplexParts(num)
			c.Assert(tt.checkReal(rel), qt.IsTrue, qt.Commentf("real=%v", rel))
			c.Assert(tt.checkImag(iam), qt.IsTrue, qt.Commentf("imag=%v", iam))
		})
	}
}

// ---------------------------------------------------------------------------
// Exactness with complex numbers
// ---------------------------------------------------------------------------

func TestCoverage_ExactComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a complex with inf should error
	p := NewParser(env, false, strings.NewReader("#e1.0+inf.0i"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

func TestCoverage_InexactComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on an exact complex number like 1+2i
	p := NewParser(env, false, strings.NewReader("#i1+2i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(cx.Real(), qt.Equals, 1.0)
	c.Assert(cx.Imag(), qt.Equals, 2.0)
}

// ---------------------------------------------------------------------------
// Polar complex numbers (parsePolarComplex via parseFloatOrInfnan)
// ---------------------------------------------------------------------------

func TestCoverage_PolarComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 1@0 -> 1+0i (angle 0 means pure real)
	p := NewParser(env, false, strings.NewReader("1@0"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(math.Abs(cx.Real()-1.0) < 1e-10, qt.IsTrue)
	c.Assert(math.Abs(cx.Imag()-0.0) < 1e-10, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Rational with inf/nan in parseFloatOrInfnan
// ---------------------------------------------------------------------------

func TestCoverage_PolarNonZeroAngle(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 2@1.5708 (approximately pi/2) -> real near 0, imag near 2
	p := NewParser(env, false, strings.NewReader("2@1.5708"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(math.Abs(cx.Imag()-2.0) < 0.01, qt.IsTrue, qt.Commentf("imag=%v", cx.Imag()))
}

// ---------------------------------------------------------------------------
// Scientific notation
// ---------------------------------------------------------------------------

func TestCoverage_ScientificNotation(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name     string
		input    string
		expected float64
	}{
		{"1e10", "1e10", 1e10},
		{"+2e-5", "+2e-5", 2e-5},
		{"-3e2", "-3e2", -300.0},
		{"1.5e3", "1.5e3", 1500.0},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			f, ok := syn.Unwrap().(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
			c.Assert(f.Value, qt.Equals, tt.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Exact with scientific notation
// ---------------------------------------------------------------------------

func TestCoverage_ExactScientific(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e1e2 -> exact integer 100
	p := NewParser(env, false, strings.NewReader("#e1e2"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	i, ok := syn.Unwrap().(*values.Integer)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
	c.Assert(i.Value, qt.Equals, int64(100))
}

// ---------------------------------------------------------------------------
// Close and error states
// ---------------------------------------------------------------------------

func TestCoverage_CloseParser(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("42"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	err = p.Close()
	c.Assert(err, qt.IsNil)

	// Closing again should error
	err = p.Close()
	c.Assert(err, qt.IsNotNil)
}

func TestCoverage_UnexpectedCloseParen(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader(")"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "unexpected close )"), qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Comment skipping in compound structures
// ---------------------------------------------------------------------------

func TestCoverage_CommentSkipInList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// List with inline comments that should be skipped
	p := NewParser(env, true, strings.NewReader("(1 ; a comment\n 2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

func TestCoverage_DatumCommentSkip(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #; datum comment should skip the next datum
	p := NewParser(env, true, strings.NewReader("#;foo bar"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	sym, ok := syn.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(sym.Datum().Key, qt.Equals, "bar")
}

func TestCoverage_DatumCommentInList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Datum comment inside a list
	p := NewParser(env, true, strings.NewReader("(1 #;2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// ParserError wrapping
// ---------------------------------------------------------------------------

func TestParserError_Is(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("x")
	pe := NewParserError(tok, "msg")
	c.Assert(pe.Is(&ParserError{}), qt.IsTrue)
	c.Assert(pe.Is(werr.ErrNotAnInteger), qt.IsFalse)
}

func TestParserError_Unwrap(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("x")
	inner := werr.ErrNotAnInteger
	pe := NewParserErrorWithWrap(inner, tok, "wrapping")
	c.Assert(pe.Unwrap(), qt.Equals, inner)

	pe2 := NewParserError(tok, "no wrap")
	c.Assert(pe2.Unwrap(), qt.IsNil)
}

func TestParserErrorf(t *testing.T) {
	c := qt.New(t)
	tok := makeTestToken("x")
	pe := NewParserErrorf(tok, "got %d items", 5)
	c.Assert(pe.Error(), qt.Equals, "got 5 items")

	pe2 := NewParserErrorWithWrapf(werr.ErrNotAnInteger, tok, "wrap %s", "test")
	c.Assert(pe2.Error(), qt.Equals, "wrap test")
	c.Assert(pe2.Unwrap(), qt.Equals, werr.ErrNotAnInteger)
}

// ---------------------------------------------------------------------------
// Exactness prefix with BigInteger
// ---------------------------------------------------------------------------

func TestCoverage_InexactBigInteger(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on a big integer
	p := NewParser(env, false, strings.NewReader("#i999999999999999999"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(f.Value > 0, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Block comments
// ---------------------------------------------------------------------------

func TestCoverage_BlockComment(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Block comment followed by a datum
	p := NewParser(env, true, strings.NewReader("#| block comment |# 42"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, values.NewInteger(42))
}

// ---------------------------------------------------------------------------
// Bytevector parsing
// ---------------------------------------------------------------------------

func TestCoverage_Bytevector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#u8(1 2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	bv, ok := syn.Unwrap().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(len(*bv), qt.Equals, 3)
}

func TestCoverage_EmptyBytevector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#u8()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	bv, ok := syn.Unwrap().(*values.ByteVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(len(*bv), qt.Equals, 0)
}

// ---------------------------------------------------------------------------
// Characters
// ---------------------------------------------------------------------------

func TestCoverage_Characters(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name     string
		input    string
		expected rune
	}{
		{"graphic a", "#\\a", 'a'},
		{"mnemonic newline", "#\\newline", '\n'},
		{"mnemonic space", "#\\space", ' '},
		{"mnemonic tab", "#\\tab", '\t'},
		{"mnemonic alarm", "#\\alarm", '\a'},
		{"mnemonic backspace", "#\\backspace", '\b'},
		{"mnemonic delete", "#\\delete", 127},
		{"mnemonic escape", "#\\escape", 27},
		{"mnemonic null", "#\\null", 0},
		{"mnemonic return", "#\\return", '\r'},
		{"hex escape", "#\\x41", 'A'},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			ch, ok := syn.Unwrap().(*values.Character)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
			c.Assert(ch.Value, qt.Equals, tt.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Vectors
// ---------------------------------------------------------------------------

func TestCoverage_Vector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#(1 2 3)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	vec, ok := syn.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(len(vec.Values), qt.Equals, 3)
}

func TestCoverage_EmptyVector(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	vec, ok := syn.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(len(vec.Values), qt.Equals, 0)
}

// ---------------------------------------------------------------------------
// Quote forms
// ---------------------------------------------------------------------------

func TestCoverage_QuoteForms(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name  string
		input string
	}{
		{"quote", "'foo"},
		{"quasiquote", "`foo"},
		{"unquote", ",foo"},
		{"unquote-splicing", ",@foo"},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn, qt.IsNotNil)
		})
	}
}

// ---------------------------------------------------------------------------
// Dotted pair parsing
// ---------------------------------------------------------------------------

func TestCoverage_DottedPair(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("(1 . 2)"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	pair, ok := syn.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
	c.Assert(pair.Car(), qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// Directive without skipComment
// ---------------------------------------------------------------------------

func TestCoverage_DirectiveNoSkip(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// When not skipping comments, directives are returned as syntax values
	p := NewParser(env, false, strings.NewReader("#!fold-case"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.(*syntax.SyntaxDirective)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn))
}

// ---------------------------------------------------------------------------
// makeExact with BigFloat
// ---------------------------------------------------------------------------

func TestCoverage_ExactBigFloat(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a BigFloat (arbitrary precision)
	p := NewParser(env, false, strings.NewReader("#e#m1.5"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	// Should be converted to exact rational
	_, ok := syn.Unwrap().(*values.Rational)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
}

func TestCoverage_ExactBigFloatInt(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a BigFloat that is an integer
	p := NewParser(env, false, strings.NewReader("#e#m42.0"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	// Should be converted to exact BigInteger
	_, ok := syn.Unwrap().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
}

// ---------------------------------------------------------------------------
// Hash digit in non-decimal bases (parseIntegerWithBase hash digit paths)
// ---------------------------------------------------------------------------

func TestCoverage_BinaryHashDigit(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #b1# -> binary with hash digit, should be inexact
	p := NewParser(env, false, strings.NewReader("#b1#"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	// #b10 = 2 in decimal
	c.Assert(f.Value, qt.Equals, 2.0)
}

func TestCoverage_OctalHashDigit(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #o7## -> octal with hash digits, should be inexact
	p := NewParser(env, false, strings.NewReader("#o7##"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	// #o700 = 7*64 = 448
	c.Assert(f.Value, qt.Equals, 448.0)
}

func TestCoverage_HexHashDigit(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #xf# -> hex with hash digit, should be inexact
	p := NewParser(env, false, strings.NewReader("#xf#"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	// #xf0 = 240
	c.Assert(f.Value, qt.Equals, 240.0)
}

// ---------------------------------------------------------------------------
// numberToInexact with Rational
// ---------------------------------------------------------------------------

func TestCoverage_NumberToInexactRational(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on a rational number -> should convert to float
	p := NewParser(env, false, strings.NewReader("#i1/3"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(math.Abs(f.Value-1.0/3.0) < 1e-10, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// makeExact with inexact Complex
// ---------------------------------------------------------------------------

func TestCoverage_ExactInexactComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on an inexact complex like 1.5+2.5i -> exact BigComplex
	p := NewParser(env, false, strings.NewReader("#e1.5+2.5i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
}

// ---------------------------------------------------------------------------
// makeInexact with BigComplex
// ---------------------------------------------------------------------------

func TestCoverage_InexactBigComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on an exact complex number 1+2i -> inexact Complex
	p := NewParser(env, false, strings.NewReader("#i1+2i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(cx.Real(), qt.Equals, 1.0)
	c.Assert(cx.Imag(), qt.Equals, 2.0)
}

// ---------------------------------------------------------------------------
// makeExact with exact BigComplex (pass-through)
// ---------------------------------------------------------------------------

func TestCoverage_ExactExactBigComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on an already exact complex 1+2i -> should stay exact BigComplex
	p := NewParser(env, false, strings.NewReader("#e1+2i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T: %v", syn.Unwrap(), syn.Unwrap()))
}

// ---------------------------------------------------------------------------
// parseFloatOrInfnan with regular floats and rationals (via polar)
// ---------------------------------------------------------------------------

func TestCoverage_PolarWithFloats(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 2.0@0.0 -> 2+0i (exercises parseFloatOrInfnan with regular float)
	p := NewParser(env, false, strings.NewReader("2.0@0.0"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(math.Abs(cx.Real()-2.0) < 1e-10, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// ReadSyntax: skip comments at top level
// ---------------------------------------------------------------------------

func TestCoverage_SkipTopLevelComment(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Line comment at top level followed by datum
	p := NewParser(env, true, strings.NewReader("; comment\n42"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, values.NewInteger(42))
}

// ---------------------------------------------------------------------------
// ReadSyntax: error after first read (cached error)
// ---------------------------------------------------------------------------

func TestCoverage_CachedError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Parse something that causes an error, then try again
	p := NewParser(env, true, strings.NewReader(")"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)

	// Second read should return the cached error
	_, err2 := p.ReadSyntax(context.TODO())
	c.Assert(err2, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// numberToInexact with BigComplex (exact complex with hash digits)
// ---------------------------------------------------------------------------

func TestCoverage_HashDigitRational(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Rational with hash digits should force inexact
	p := NewParser(env, false, strings.NewReader("1#/3"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	// 10/3 as float
	c.Assert(math.Abs(f.Value-10.0/3.0) < 1e-10, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Syntax quote forms (#' #` #, #,@)
// ---------------------------------------------------------------------------

func TestCoverage_SyntaxQuoteForms(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name  string
		input string
	}{
		{"syntax", "#'foo"},
		{"quasisyntax", "#`foo"},
		{"unsyntax", "#,foo"},
		{"unsyntax-splicing", "#,@foo"},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn, qt.IsNotNil)
		})
	}
}

// ---------------------------------------------------------------------------
// Base-10 explicit prefix (#d)
// ---------------------------------------------------------------------------

func TestCoverage_Base10Prefix(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader("#d42"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, values.NewInteger(42))
}

// ---------------------------------------------------------------------------
// Empty list
// ---------------------------------------------------------------------------

func TestCoverage_EmptyList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("()"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// Signed decimal fractions
// ---------------------------------------------------------------------------

func TestCoverage_SignedDecimal(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name     string
		input    string
		expected float64
	}{
		{"-3.14", "-3.14", -3.14},
		{"+2.5", "+2.5", 2.5},
		{"0.0", "0.0", 0.0},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			f, ok := syn.Unwrap().(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
			c.Assert(f.Value, qt.Equals, tt.expected)
		})
	}
}

// ---------------------------------------------------------------------------
// Signed and unsigned rational fractions
// ---------------------------------------------------------------------------

func TestCoverage_SignedRational(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name   string
		input  string
		expect values.Value
	}{
		{"unsigned 3/4", "3/4", values.Simplify(values.NewRationalFromRat(new(big.Rat).SetFrac64(3, 4)))},
		{"signed -1/2", "-1/2", values.Simplify(values.NewRationalFromRat(new(big.Rat).SetFrac64(-1, 2)))},
		{"signed +1/3", "+1/3", values.Simplify(values.NewRationalFromRat(new(big.Rat).SetFrac64(1, 3)))},
		{"reduces 10/2", "10/2", values.NewInteger(5)},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			c.Assert(syn.Unwrap(), valuestest.SchemeEquals, tt.expect)
		})
	}
}

// ---------------------------------------------------------------------------
// makeExact on non-number should error
// ---------------------------------------------------------------------------

func TestCoverage_ExactNonNumber(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a symbol - should produce an error
	p := NewParser(env, false, strings.NewReader("#efoo"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// makeInexact on non-number should error
// ---------------------------------------------------------------------------

func TestCoverage_InexactNonNumber(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on a symbol - should produce an error
	p := NewParser(env, false, strings.NewReader("#ifoo"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------------

func TestCoverage_Strings(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, false, strings.NewReader(`"hello world"`))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	s, ok := syn.Unwrap().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", syn.Unwrap()))
	c.Assert(s.Value, qt.Equals, "hello world")
}

// ---------------------------------------------------------------------------
// makeExact with exact BigComplex with inf (error)
// ---------------------------------------------------------------------------

func TestCoverage_ExactBigFloatInf(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on -inf.0 should error
	p := NewParser(env, false, strings.NewReader("#e-inf.0"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// Imaginary number paths
// ---------------------------------------------------------------------------

func TestCoverage_PureImaginary(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	tests := []struct {
		name  string
		input string
		check func(c *qt.C, v values.Value)
	}{
		{
			name:  "+i",
			input: "+i",
			check: func(c *qt.C, v values.Value) {
				bc, ok := v.(*values.BigComplex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(bc.Real().IsZero(), qt.IsTrue)
			},
		},
		{
			name:  "-i",
			input: "-i",
			check: func(c *qt.C, v values.Value) {
				bc, ok := v.(*values.BigComplex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(bc.Real().IsZero(), qt.IsTrue)
			},
		},
		{
			name:  "+3i",
			input: "+3i",
			check: func(c *qt.C, v values.Value) {
				bc, ok := v.(*values.BigComplex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(bc.Real().IsZero(), qt.IsTrue)
			},
		},
		{
			name:  "+2.5i (float imaginary)",
			input: "+2.5i",
			check: func(c *qt.C, v values.Value) {
				cx, ok := v.(*values.Complex)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
				c.Assert(cx.Real(), qt.Equals, 0.0)
				c.Assert(cx.Imag(), qt.Equals, 2.5)
			},
		},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			p := NewParser(env, false, strings.NewReader(tt.input))
			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			tt.check(c, syn.Unwrap())
		})
	}
}

// ---------------------------------------------------------------------------
// ParserWithFile
// ---------------------------------------------------------------------------

func TestCoverage_ParserWithFile(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParserWithFile(env, true, strings.NewReader("42"), "test.scm")
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), valuestest.SchemeEquals, values.NewInteger(42))
}

// ---------------------------------------------------------------------------
// Additional coverage: makeInexact paths
// ---------------------------------------------------------------------------

func TestCoverage_InexactRational(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#i1/3"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	// 1/3 as float64 should be approximately 0.333...
	c.Assert(f.Value > 0.33 && f.Value < 0.34, qt.IsTrue)
}

func TestCoverage_InexactBigComplexMakeInexact(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on an exact complex number exercises makeInexact BigComplex path
	p := NewParser(env, true, strings.NewReader("#i1+2i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cplx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(cplx.Real(), qt.Equals, 1.0)
	c.Assert(cplx.Imag(), qt.Equals, 2.0)
}

func TestCoverage_ExactComplexToExact(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on inexact complex exercises makeExact Complex → BigComplex path
	p := NewParser(env, true, strings.NewReader("#e1.0+2.0i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	// Should convert to exact BigComplex
	_, ok := syn.Unwrap().(*values.BigComplex)
	c.Assert(ok, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Additional coverage: parseIntegerWithBase overflow path
// ---------------------------------------------------------------------------

func TestCoverage_BinaryIntegerOverflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// 65-bit binary number overflows int64
	bigBin := "#b1" + strings.Repeat("0", 64)
	p := NewParser(env, true, strings.NewReader(bigBin))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_HexIntegerOverflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Hex number that overflows int64
	p := NewParser(env, true, strings.NewReader("#xFFFFFFFFFFFFFFFF1"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Additional coverage: parseRationalWithBase overflow paths
// ---------------------------------------------------------------------------

func TestCoverage_BinaryRationalOverflowNumerator(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Binary rational with overflow numerator
	bigBin := "#b" + strings.Repeat("1", 65) + "/10"
	p := NewParser(env, true, strings.NewReader(bigBin))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

func TestCoverage_HexRationalOverflowDenominator(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Hex rational with overflow denominator
	p := NewParser(env, true, strings.NewReader("#x1/FFFFFFFFFFFFFFFF1"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

func TestCoverage_SignedBinaryRationalOverflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Signed binary rational with overflow
	bigBin := "#b-" + strings.Repeat("1", 65) + "/10"
	p := NewParser(env, true, strings.NewReader(bigBin))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

// ---------------------------------------------------------------------------
// Additional coverage: parseFloatOrInfnan rational path
// ---------------------------------------------------------------------------

func TestCoverage_PolarWithRationalMagnitude(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Polar complex with non-zero angle to exercise cos/sin paths
	p := NewParser(env, true, strings.NewReader("2@1"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cplx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	// 2*cos(1) ≈ 1.0806, 2*sin(1) ≈ 1.6829
	c.Assert(cplx.Real() > 1.0 && cplx.Real() < 1.2, qt.IsTrue)
	c.Assert(cplx.Imag() > 1.6 && cplx.Imag() < 1.8, qt.IsTrue)
}

// ---------------------------------------------------------------------------
// Additional coverage: readSyntax token branches
// ---------------------------------------------------------------------------

func TestCoverage_UnsignedImaginary(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Test +nan.0i and -nan.0i
	p := NewParser(env, true, strings.NewReader("+nan.0i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cplx, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsNaN(cplx.Imag()), qt.IsTrue)

	p = NewParser(env, true, strings.NewReader("-nan.0i"))
	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	cplx, ok = syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(math.IsNaN(cplx.Imag()), qt.IsTrue)
}

func TestCoverage_SignedDecimalFraction(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Positive signed decimal fraction
	p := NewParser(env, true, strings.NewReader("+3.14"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, 3.14)

	// Negative decimal fraction
	p = NewParser(env, true, strings.NewReader("-2.5"))
	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok = syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, -2.5)
}

func TestCoverage_Boolean(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	p := NewParser(env, true, strings.NewReader("#t #f #true #false"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), qt.Equals, values.TrueValue)

	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), qt.Equals, values.FalseValue)

	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), qt.Equals, values.TrueValue)

	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn.Unwrap(), qt.Equals, values.FalseValue)
}

func TestCoverage_ExactFloat(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a float that is a whole number → Integer
	p := NewParser(env, true, strings.NewReader("#e5.0"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	i, ok := syn.Unwrap().(*values.Integer)
	c.Assert(ok, qt.IsTrue)
	c.Assert(i.Value, qt.Equals, int64(5))

	// #e on a float that is NOT a whole number → Rational
	p = NewParser(env, true, strings.NewReader("#e0.1"))
	syn, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	r, ok := syn.Unwrap().(*values.Rational)
	c.Assert(ok, qt.IsTrue)
	c.Assert(r, qt.IsNotNil)
}

func TestCoverage_InexactInteger(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on an integer
	p := NewParser(env, true, strings.NewReader("#i42"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
	c.Assert(f.Value, qt.Equals, float64(42))
}

func TestCoverage_ConsError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// A standalone '.' at top-level should produce an error
	p := NewParser(env, true, strings.NewReader("."))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
}

func TestCoverage_NestedList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Nested list (1 (2 3))
	p := NewParser(env, true, strings.NewReader("(1 (2 3))"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

func TestCoverage_ExactBigInteger(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a BigInteger - should pass through
	bigNum := "#e99999999999999999999"
	p := NewParser(env, true, strings.NewReader(bigNum))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigInteger)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_ExactRational(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #e on a rational - should pass through
	p := NewParser(env, true, strings.NewReader("#e1/3"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.Rational)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_NumberToInexactBigComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Parse exact complex 1+2i (becomes BigComplex), then apply #i
	// This exercises numberToInexact BigComplex path
	p := NewParser(env, true, strings.NewReader("#i1+2i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.Complex)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_BigFloatBasic(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #m prefix for BigFloat
	p := NewParser(env, true, strings.NewReader("#m3.14159265358979323846"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigFloat)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_MakeInexactBigInteger(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// #i on a BigInteger exercises makeInexact BigInteger path
	p := NewParser(env, true, strings.NewReader("#i99999999999999999999"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue)
}

func TestCoverage_SignedRationalFraction(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Positive signed rational
	p := NewParser(env, true, strings.NewReader("+3/4"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	c.Assert(syn, qt.IsNotNil)
}

func TestCoverage_UnsignedComplexExact(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()

	// Exact complex with integer parts
	p := NewParser(env, true, strings.NewReader("3+4i"))
	syn, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	_, ok := syn.Unwrap().(*values.BigComplex)
	c.Assert(ok, qt.IsTrue)
}
