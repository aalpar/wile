// Copyright 2025 Aaron Alpar
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

package tokenizer

import (
	"fmt"
	"io"
	"math"
	"strings"
	"testing"
	"unicode/utf8"

	"wile/syntax"

	qt "github.com/frankban/quicktest"
)

func TestTokenizer_curr(t *testing.T) {
	p := NewTokenizer(strings.NewReader("this"), false)
	r := p.curr()
	qt.Check(t, r, qt.Equals, rune('t'))
	qt.Check(t, p.err, qt.IsNil)
	p.next()
	qt.Check(t, p.err, qt.IsNil)
	r = p.curr()
	qt.Check(t, r, qt.Equals, 'h')
	qt.Check(t, p.err, qt.IsNil)
}

func TestTokenizer_next(t *testing.T) {
	p := NewTokenizer(strings.NewReader("this"), false)
	qt.Check(t, p.err, qt.IsNil)
	qt.Check(t, p.curr(), qt.Equals, 't')
	qt.Check(t, p.cur, qt.Equals, 't')
	p.next()
	qt.Check(t, p.err, qt.IsNil)
	qt.Check(t, p.curr(), qt.Equals, 'h')
	qt.Check(t, p.cur, qt.Equals, 'h')
	p.next()
	qt.Check(t, p.err, qt.IsNil)
	qt.Check(t, p.curr(), qt.Equals, 'i')
	qt.Check(t, p.cur, qt.Equals, 'i')
	p.next()
	qt.Check(t, p.err, qt.IsNil)
	qt.Check(t, p.curr(), qt.Equals, 's')
	qt.Check(t, p.cur, qt.Equals, 's')
	p.next()
	qt.Check(t, p.err, qt.ErrorIs, io.EOF)
}

func TestTokenizer_scan(t *testing.T) {
	n := 0
	//
	tcs := []struct {
		in     string
		scan   string
		erris0 error
		erris1 error
		nis    int
		cur    rune
	}{
		{
			in:   "this",
			scan: "th",
			nis:  0,
			cur:  'i',
		},
		{
			in:     "this",
			scan:   "this",
			nis:    0,
			erris1: io.EOF,
			cur:    utf8.RuneError,
		},
		{
			in:   "thiss",
			scan: "this",
			nis:  0,
			cur:  's',
		},
		{
			in:   "tiss",
			scan: "this",
			nis:  3,
			cur:  'i',
		},
		{
			in:   "tiss",
			scan: "xis",
			nis:  3,
			cur:  't',
		},
		{
			in:   "xiss",
			scan: "this",
			nis:  4,
			cur:  'x',
		},
		{
			in:     "",
			scan:   "this",
			nis:    4,
			erris0: io.EOF,
			erris1: io.EOF,
			cur:    utf8.RuneError,
		},
		{
			in:     "t",
			scan:   "this",
			nis:    3,
			erris1: io.EOF,
			cur:    utf8.RuneError,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.in), false)
			c.Check(p.err, qt.ErrorIs, tc.erris0)
			n = p.scan([]byte(tc.scan))
			c.Check(p.err, qt.ErrorIs, tc.erris1)
			c.Check(n, qt.Equals, tc.nis)
			c.Check(p.cur, qt.Equals, tc.cur, qt.Commentf("want '%c' but got '%c' instead", tc.cur, p.cur))
		})
	}
}

func TestTokenizerOverRead(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("'"), false)
	//
	// initial state should equal unknown and curr == nil
	c.Check(p.state, qt.Equals, TokenizerStateFailed)
	c.Check(p.err, qt.IsNil)
	c.Check(p.cur, qt.Equals, '\'')
	r := p.curr()
	c.Check(p.state, qt.Equals, TokenizerStateFailed)
	c.Check(p.err, qt.IsNil)
	c.Check(p.cur, qt.Equals, '\'')
	c.Check(r, qt.Equals, '\'')
	//
	// after next() character should be '\'' without EOF
	p.next()
	c.Check(p.state, qt.Equals, TokenizerStateFailed)
	c.Check(p.err, qt.Equals, io.EOF)
	c.Check(p.cur, qt.Equals, utf8.RuneError)
	r = p.curr()
	c.Check(r, qt.Equals, utf8.RuneError)
	//
	// curr == "'" - parser should read quote token
	p.read()
	c.Check(p.err, qt.ErrorIs, io.EOF)
	c.Check(p.state, qt.Equals, TokenizerStateFailed)
	r = p.curr()
	c.Check(r, qt.Equals, utf8.RuneError)
}

func TestTokenizer_Next(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("thisthat"), false)
	a, err := p.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(a.Type(), qt.Equals, TokenizerStateSymbol)
	_, err = p.Next()
	c.Assert(err, qt.IsNotNil)
}

func TestTokenizer_TokenIterator(t *testing.T) {
	tcs := []struct {
		bs     string
		tokens []SimpleToken
		err0   error
		err1   error
	}{
		{
			bs:     "''",
			tokens: []SimpleToken{{typ: TokenizerStateQuote}, {typ: TokenizerStateQuote}},
			err0:   io.EOF,
			err1:   nil,
		},
		{
			bs:     "1@1.5708",
			tokens: []SimpleToken{{typ: TokenizerStateUnsignedComplexPolar}},
			err0:   io.EOF,
			err1:   nil,
		},
		{
			bs:     "+1@1.5708",
			tokens: []SimpleToken{{typ: TokenizerStateSignedComplexPolar}},
			err0:   io.EOF,
			err1:   nil,
		},
		{
			bs:     "-1@1.5708",
			tokens: []SimpleToken{{typ: TokenizerStateSignedComplexPolar}},
			err0:   io.EOF,
			err1:   nil,
		},
		{
			bs:     "1+1.5708i",
			tokens: []SimpleToken{{typ: TokenizerStateUnsignedComplex}},
			err0:   io.EOF,
			err1:   nil,
		},

		{
			bs:     "+1+1.5708i",
			tokens: []SimpleToken{{typ: TokenizerStateSignedComplex}},
			err0:   io.EOF,
			err1:   nil,
		},
		{
			bs:     "-1+1.5708i",
			tokens: []SimpleToken{{typ: TokenizerStateSignedComplex}},
			err0:   io.EOF,
			err1:   nil,
		},
	}
	c := qt.New(t)
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			j := 0
			tk, err := p.Next()
			for err == nil {
				qt.Assert(c, tk.Type(), qt.Equals, tc.tokens[j].typ)
				j++
				tk, err = p.Next()
			}
			c.Check(j, qt.Equals, len(tc.tokens))
			c.Check(err, qt.ErrorIs, tc.err0)
		})
	}
}

func TestTokenizer_TokenStream(t *testing.T) {
	tcs := []struct {
		in     string
		tokens []TokenizerState
		src    []string
		err    error
	}{
		{
			in:     "`",
			tokens: []TokenizerState{TokenizerStateQuasiquote},
			src:    []string{"`"},
			err:    io.EOF,
		},
		{
			in:     ",",
			tokens: []TokenizerState{TokenizerStateUnquote},
			src:    []string{","},
			err:    io.EOF,
		},
		{
			in:     ".",
			tokens: []TokenizerState{TokenizerStateCons},
			src:    []string{"."},
			err:    io.EOF,
		},
		{
			in:     "..",
			tokens: []TokenizerState{TokenizerStateSymbol},
			src:    []string{".."},
			err:    io.EOF,
		},
		{
			in:     "...",
			tokens: []TokenizerState{TokenizerStateSymbol},
			src:    []string{"..."},
			err:    io.EOF,
		},
		{
			in:     "....",
			tokens: []TokenizerState{TokenizerStateSymbol},
			src:    []string{"...."},
			err:    io.EOF,
		},
		{
			in:     ",@",
			tokens: []TokenizerState{TokenizerStateUnquoteSplicing},
			src:    []string{",@"},
			err:    io.EOF,
		},
		{
			in:     `#\c`,
			tokens: []TokenizerState{TokenizerStateCharGraphic},
			src:    []string{"#\\c"},
			err:    io.EOF,
		},
		{
			in:     `#\space`,
			tokens: []TokenizerState{TokenizerStateCharMnemonic},
			src:    []string{`#\space`},
			err:    io.EOF,
		},
		{
			in:     "'",
			tokens: []TokenizerState{TokenizerStateQuote},
			src:    []string{"'"},
			err:    io.EOF,
		},
		{
			in:     "4",
			tokens: []TokenizerState{TokenizerStateUnsignedInteger},
			src:    []string{"4"},
			err:    io.EOF,
		},
		{
			in:     "40",
			tokens: []TokenizerState{TokenizerStateUnsignedInteger},
			src:    []string{"40"},
			err:    io.EOF,
		},
		{
			in:     "-40",
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			src:    []string{"-40"},
			err:    io.EOF,
		},
		{
			in:     "+40",
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			src:    []string{"+40"},
			err:    io.EOF,
		},
		{
			in:     "+3.24",
			tokens: []TokenizerState{TokenizerStateSignedDecimalFraction},
			src:    []string{"+3.24"},
			err:    io.EOF,
		},
		{
			in:     "-3.24",
			tokens: []TokenizerState{TokenizerStateSignedDecimalFraction},
			src:    []string{"-3.24"},
			err:    io.EOF,
		},
		{
			in:     "3.24",
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			src:    []string{"3.24"},
			err:    io.EOF,
		},
		{
			in:     ".24",
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			src:    []string{".24"},
			err:    io.EOF,
		},
		{
			in:     ".024",
			src:    []string{".024"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "0.024",
			src:    []string{"0.024"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "0.024e+10",
			src:    []string{"0.024e+10"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		// R7RS conformance: signed integers with exponents
		{
			in:     "+1e10",
			src:    []string{"+1e10"},
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			err:    io.EOF,
		},
		{
			in:     "-1e10",
			src:    []string{"-1e10"},
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			err:    io.EOF,
		},
		{
			in:     "+1E10",
			src:    []string{"+1E10"},
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			err:    io.EOF,
		},
		{
			in:     "-1e+10",
			src:    []string{"-1e+10"},
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			err:    io.EOF,
		},
		{
			in:     "+1e-10",
			src:    []string{"+1e-10"},
			tokens: []TokenizerState{TokenizerStateSignedInteger},
			err:    io.EOF,
		},
		// R7RS conformance: trailing dot with exponent
		{
			in:     "1.e10",
			src:    []string{"1.e10"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "1.e+10",
			src:    []string{"1.e+10"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "1.e-10",
			src:    []string{"1.e-10"},
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "+1.e10",
			src:    []string{"+1.e10"},
			tokens: []TokenizerState{TokenizerStateSignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     "-1.e10",
			src:    []string{"-1.e10"},
			tokens: []TokenizerState{TokenizerStateSignedDecimalFraction},
			err:    io.EOF,
		},
		{
			in:     ".a",
			tokens: []TokenizerState{TokenizerStateSymbol},
			src:    []string{".a"},
			err:    io.EOF,
		},
		{
			in:     "#x10",
			tokens: []TokenizerState{TokenizerStateMarkerBase16, TokenizerStateUnsignedIntegerBase16},
			src:    []string{"#x", "10"},
			err:    io.EOF,
		},
		{
			in:     "''",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateQuote},
			src:    []string{"'", "'"},
			err:    io.EOF,
		},
		{
			in:     "#i4",
			tokens: []TokenizerState{TokenizerStateMarkerNumberInexact, TokenizerStateUnsignedInteger},
			src:    []string{"#i", "4"},
			err:    io.EOF,
		},
		{
			in:     "#e4",
			tokens: []TokenizerState{TokenizerStateMarkerNumberExact, TokenizerStateUnsignedInteger},
			src:    []string{"#e", "4"},
			err:    io.EOF,
		},
		{
			in:     "#x4",
			tokens: []TokenizerState{TokenizerStateMarkerBase16, TokenizerStateUnsignedIntegerBase16},
			src:    []string{"#x", "4"},
			err:    io.EOF,
		},
		{
			in:     "((",
			tokens: []TokenizerState{TokenizerStateOpenParen, TokenizerStateOpenParen},
			src:    []string{"(", "("},
			err:    io.EOF,
		},
		{
			in:     "()",
			tokens: []TokenizerState{TokenizerStateEmptyList},
			src:    []string{"()"},
			err:    io.EOF,
		},
		{
			in: "( #t )",
			tokens: []TokenizerState{
				TokenizerStateOpenParen,
				TokenizerStateMarkerBooleanTrue,
				TokenizerStateCloseParen,
			},
			src: []string{"(", "#t", ")"},
			err: io.EOF,
		},
		{
			in: "( #t . #f )",
			tokens: []TokenizerState{
				TokenizerStateOpenParen,
				TokenizerStateMarkerBooleanTrue,
				TokenizerStateCons,
				TokenizerStateMarkerBooleanFalse,
				TokenizerStateCloseParen,
			},
			src: []string{"(", "#t", ".", "#f", ")"},
			err: io.EOF,
		},
		{
			in: "( . #f )",
			tokens: []TokenizerState{
				TokenizerStateOpenParen,
				TokenizerStateCons,
				TokenizerStateMarkerBooleanFalse,
				TokenizerStateCloseParen,
			},
			src: []string{"(", ".", "#f", ")"},
			err: io.EOF,
		},
		{
			in: "( . )",
			tokens: []TokenizerState{
				TokenizerStateOpenParen,
				TokenizerStateCons,
				TokenizerStateCloseParen,
			},
			src: []string{"(", ".", ")"},
			err: io.EOF,
		},
		{
			in:     "'hello",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateSymbol},
			src:    []string{"'", "hello"},
			err:    io.EOF,
		},
		{
			in:     "#t #f",
			tokens: []TokenizerState{TokenizerStateMarkerBooleanTrue, TokenizerStateMarkerBooleanFalse},
			src:    []string{"#t", "#f"},
			err:    io.EOF,
		},
		{
			in:     "#fart",
			tokens: []TokenizerState{TokenizerStateMarker},
			src:    []string{"#fart"},
			err:    io.EOF,
		},
		{
			in:     "#(1 2)",
			tokens: []TokenizerState{TokenizerStateOpenVector, TokenizerStateUnsignedInteger, TokenizerStateUnsignedInteger, TokenizerStateCloseParen},
			src:    []string{"#(", "1", "2", ")"},
			err:    io.EOF,
		},

		{
			in:     "(())",
			tokens: []TokenizerState{TokenizerStateOpenParen, TokenizerStateEmptyList, TokenizerStateCloseParen},
			src:    []string{"(", "()", ")"},
			err:    io.EOF,
		},
		{
			in:     "'(hello this)",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateSymbol, TokenizerStateCloseParen},
			src:    []string{"'", "(", "hello", "this", ")"},
			err:    io.EOF,
		},
		{
			in:     "'(hello 1.20)",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateUnsignedDecimalFraction, TokenizerStateCloseParen},
			src:    []string{"'", "(", "hello", "1.20", ")"},
			err:    io.EOF,
		},
		{
			in:     ".20",
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			src:    []string{".20"},
			err:    io.EOF,
		},
		{
			in:     ".02",
			tokens: []TokenizerState{TokenizerStateUnsignedDecimalFraction},
			src:    []string{".02"},
			err:    io.EOF,
		},
		{
			in:     "'(+ 1.20 2)",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateUnsignedDecimalFraction, TokenizerStateUnsignedInteger, TokenizerStateCloseParen},
			src:    []string{"'", "(", "+", "1.20", "2", ")"},
			err:    io.EOF,
		},
		{
			in:     ``,
			tokens: []TokenizerState{},
			src:    []string{},
			err:    io.EOF,
		},
		{
			in:     `;`,
			tokens: []TokenizerState{TokenizerStateLineCommentBody},
			src:    []string{";"},
			err:    io.EOF,
		},
		{
			in:     `;;;`,
			tokens: []TokenizerState{TokenizerStateLineCommentBody},
			src:    []string{";;;"},
			err:    io.EOF,
		},
		{
			in:     `; this that`,
			tokens: []TokenizerState{TokenizerStateLineCommentBody},
			src:    []string{"; this that"},
			err:    io.EOF,
		},
		{
			in: `; this
                 ; that`,
			tokens: []TokenizerState{TokenizerStateLineCommentBody, TokenizerStateLineCommentBody},
			src:    []string{"; this", "; that"},
			err:    io.EOF,
		},
		{
			in:     `#!fold`,
			tokens: []TokenizerState{TokenizerStateDirective},
			src:    []string{"#!fold"},
			err:    io.EOF,
		},
		{
			in:     `#| foo |#`,
			tokens: []TokenizerState{TokenizerStateBlockCommentBody},
			src:    []string{"#| foo |#"},
			err:    io.EOF,
		},
		{
			in:     `#'foo`,
			tokens: []TokenizerState{TokenizerStateSyntax, TokenizerStateSymbol},
			src:    []string{"#'", "foo"},
			err:    io.EOF,
		},
		{
			in:     "#`foo",
			tokens: []TokenizerState{TokenizerStateQuasisyntax, TokenizerStateSymbol},
			src:    []string{"#`", "foo"},
			err:    io.EOF,
		},
		{
			in:     "#,foo",
			tokens: []TokenizerState{TokenizerStateUnsyntax, TokenizerStateSymbol},
			src:    []string{"#,", "foo"},
			err:    io.EOF,
		},
		{
			in:     "#,@foo",
			tokens: []TokenizerState{TokenizerStateUnsyntaxSplicing, TokenizerStateSymbol},
			src:    []string{"#,@", "foo"},
			err:    io.EOF,
		},
		{
			in:     "#10='foo",
			tokens: []TokenizerState{TokenizerStateLabelAssignment, TokenizerStateQuote, TokenizerStateSymbol},
			src:    []string{"#10=", "'", "foo"},
			err:    io.EOF,
		},
		{
			in:     "#10#",
			tokens: []TokenizerState{TokenizerStateLabelReference},
			src:    []string{"#10#"},
			err:    io.EOF,
		},
		{
			in:     "#;",
			tokens: []TokenizerState{TokenizerStateDatumCommentBegin},
			src:    []string{"#;"},
			err:    io.EOF,
		},
		{
			in:     `"hello"`,
			tokens: []TokenizerState{TokenizerStateString},
			src:    []string{`"hello"`},
			err:    io.EOF,
		},
		{
			in:     `"hello \"there\"!"`,
			tokens: []TokenizerState{TokenizerStateString},
			src:    []string{`"hello \"there\"!"`},
			err:    io.EOF,
		},
		{
			in:     `"Use #\\Control-q to quit."`,
			tokens: []TokenizerState{TokenizerStateString},
			src:    []string{`"Use #\\Control-q to quit."`},
			err:    io.EOF,
		},
		// Pure imaginary numbers
		{
			in:     "+i",
			tokens: []TokenizerState{TokenizerStateSignedImaginary},
			src:    []string{"+i"},
			err:    io.EOF,
		},
		{
			in:     "-i",
			tokens: []TokenizerState{TokenizerStateSignedImaginary},
			src:    []string{"-i"},
			err:    io.EOF,
		},
		{
			in:     "+3i",
			tokens: []TokenizerState{TokenizerStateSignedImaginary},
			src:    []string{"+3i"},
			err:    io.EOF,
		},
		{
			in:     "-3.5i",
			tokens: []TokenizerState{TokenizerStateSignedImaginary},
			src:    []string{"-3.5i"},
			err:    io.EOF,
		},
		// Imaginary infinity and NaN
		{
			in:     "+inf.0i",
			tokens: []TokenizerState{TokenizerStateSignedImaginaryInf},
			src:    []string{"+inf.0i"},
			err:    io.EOF,
		},
		{
			in:     "-inf.0i",
			tokens: []TokenizerState{TokenizerStateSignedImaginaryInf},
			src:    []string{"-inf.0i"},
			err:    io.EOF,
		},
		{
			in:     "+nan.0i",
			tokens: []TokenizerState{TokenizerStateSignedImaginaryNan},
			src:    []string{"+nan.0i"},
			err:    io.EOF,
		},
		{
			in:     "-nan.0i",
			tokens: []TokenizerState{TokenizerStateSignedImaginaryNan},
			src:    []string{"-nan.0i"},
			err:    io.EOF,
		},
		// Real infinity and NaN
		{
			in:     "+inf.0",
			tokens: []TokenizerState{TokenizerStateSignedInf},
			src:    []string{"+inf.0"},
			err:    io.EOF,
		},
		{
			in:     "-inf.0",
			tokens: []TokenizerState{TokenizerStateSignedInf},
			src:    []string{"-inf.0"},
			err:    io.EOF,
		},
		{
			in:     "+nan.0",
			tokens: []TokenizerState{TokenizerStateSignedNan},
			src:    []string{"+nan.0"},
			err:    io.EOF,
		},
		{
			in:     "-nan.0",
			tokens: []TokenizerState{TokenizerStateSignedNan},
			src:    []string{"-nan.0"},
			err:    io.EOF,
		},
		// Complex numbers (rectangular form) - unsigned real
		{
			in:     "1+2i",
			tokens: []TokenizerState{TokenizerStateUnsignedComplex},
			src:    []string{"1+2i"},
			err:    io.EOF,
		},
		{
			in:     "3-4i",
			tokens: []TokenizerState{TokenizerStateUnsignedComplex},
			src:    []string{"3-4i"},
			err:    io.EOF,
		},
		{
			in:     "1.5+2.5i",
			tokens: []TokenizerState{TokenizerStateUnsignedComplex},
			src:    []string{"1.5+2.5i"},
			err:    io.EOF,
		},
		{
			in:     "1+i",
			tokens: []TokenizerState{TokenizerStateUnsignedComplex},
			src:    []string{"1+i"},
			err:    io.EOF,
		},
		{
			in:     "5-i",
			tokens: []TokenizerState{TokenizerStateUnsignedComplex},
			src:    []string{"5-i"},
			err:    io.EOF,
		},
		// Complex numbers (rectangular form) - signed real
		{
			in:     "-1+2i",
			tokens: []TokenizerState{TokenizerStateSignedComplex},
			src:    []string{"-1+2i"},
			err:    io.EOF,
		},
		{
			in:     "+1+2i",
			tokens: []TokenizerState{TokenizerStateSignedComplex},
			src:    []string{"+1+2i"},
			err:    io.EOF,
		},
		{
			in:     "-3-4i",
			tokens: []TokenizerState{TokenizerStateSignedComplex},
			src:    []string{"-3-4i"},
			err:    io.EOF,
		},
		{
			in:     "-1+i",
			tokens: []TokenizerState{TokenizerStateSignedComplex},
			src:    []string{"-1+i"},
			err:    io.EOF,
		},
		{
			in:     "+5-i",
			tokens: []TokenizerState{TokenizerStateSignedComplex},
			src:    []string{"+5-i"},
			err:    io.EOF,
		},
		// Complex numbers (polar form) - unsigned magnitude
		{
			in:     "1@1.5708",
			tokens: []TokenizerState{TokenizerStateUnsignedComplexPolar},
			src:    []string{"1@1.5708"},
			err:    io.EOF,
		},
		{
			in:     "3.5@0.785",
			tokens: []TokenizerState{TokenizerStateUnsignedComplexPolar},
			src:    []string{"3.5@0.785"},
			err:    io.EOF,
		},
		{
			in:     "1@0",
			tokens: []TokenizerState{TokenizerStateUnsignedComplexPolar},
			src:    []string{"1@0"},
			err:    io.EOF,
		},
		// Complex numbers (polar form) - signed magnitude
		{
			in:     "-1@1.5708",
			tokens: []TokenizerState{TokenizerStateSignedComplexPolar},
			src:    []string{"-1@1.5708"},
			err:    io.EOF,
		},
		{
			in:     "+1@1.5708",
			tokens: []TokenizerState{TokenizerStateSignedComplexPolar},
			src:    []string{"+1@1.5708"},
			err:    io.EOF,
		},
		{
			in:     "-3.5@0.785",
			tokens: []TokenizerState{TokenizerStateSignedComplexPolar},
			src:    []string{"-3.5@0.785"},
			err:    io.EOF,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.in, func(t *testing.T) {
			c := qt.New(t)
			ts, err := Tokenize(tc.in, false)
			c.Check(err, qt.ErrorIs, tc.err)
			qt.Assert(c, len(ts), qt.Equals, len(tc.tokens))
			qt.Assert(c, len(ts), qt.Equals, len(tc.src))
			for i := range ts {
				qt.Assert(c, ts[i].(*SimpleToken).typ, qt.Equals, tc.tokens[i])
				qt.Assert(c, ts[i].(*SimpleToken).String(), qt.Equals, tc.src[i])
			}
		})
	}
}

func TestTokenizer_read(t *testing.T) {
	tcs := []struct {
		bs    string
		ci    bool
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			bs:    "'",
			scan:  "'",
			err0:  io.EOF,
			state: TokenizerStateQuote,
		},
		{
			bs:    "(",
			scan:  "(",
			err0:  io.EOF,
			state: TokenizerStateOpenParen,
		},
		{
			bs:    "()",
			scan:  "()",
			err0:  io.EOF,
			state: TokenizerStateEmptyList,
		},
		{
			bs:    ")",
			scan:  ")",
			err0:  io.EOF,
			state: TokenizerStateCloseParen,
		},
		{
			// character constant 'c'
			bs:    `#\c`,
			scan:  `#\c`,
			err0:  io.EOF,
			state: TokenizerStateCharGraphic,
		},
		{
			// character constant '^H'
			bs:    `#\backspace`,
			scan:  `#\backspace`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// character constant '^H'
			bs:    `#\back-space`,
			scan:  `#\back-space`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// character constant 'λ'
			bs:    `#\x03BB`,
			scan:  `#\x03BB`,
			err0:  io.EOF,
			state: TokenizerStateCharHexEscape,
		},
		{
			// unsigned byte array
			bs:    `#!zz(`,
			scan:  `#!zz`,
			state: TokenizerStateDirective,
		},
		{
			// unsigned byte array
			bs:    `#u8(`,
			scan:  `#u8(`,
			err0:  io.EOF,
			state: TokenizerStateOpenVectorUnsignedByteMarker,
		},
		{
			// inexact
			bs:    `#i`,
			scan:  `#i`,
			err0:  io.EOF,
			state: TokenizerStateMarkerNumberInexact,
		},
		{
			// exact
			bs:    `#e`,
			scan:  `#e`,
			err0:  io.EOF,
			state: TokenizerStateMarkerNumberExact,
		},
		{
			// pragma
			bs:    `#!e`,
			scan:  `#!e`,
			err0:  io.EOF,
			state: TokenizerStateDirective,
		},
		{
			// decimal
			bs:    `#d`,
			scan:  `#d`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBase10,
		},
		{
			// octal
			bs:    `#o`,
			scan:  `#o`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBase8,
		},
		{
			// binary
			bs:    `#b`,
			scan:  `#b`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBase2,
		},
		{
			// hex
			bs:    `#x`,
			scan:  `#x`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBase16,
		},
		{
			// true
			bs:    `#t`,
			scan:  `#t`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanTrue,
		},
		{
			// true
			bs:    `#true`,
			scan:  `#true`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanTrue,
		},
		{
			// false
			bs:    `#f`,
			scan:  `#f`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanFalse,
		},
		{
			// false
			bs:    `#false`,
			scan:  `#false`,
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanFalse,
		},
		{
			// label reference
			bs:    `#2#`,
			scan:  `#2#`,
			err0:  io.EOF,
			state: TokenizerStateLabelReference,
		},
		{
			// label define
			bs:    `#2=10`,
			scan:  `#2=`,
			state: TokenizerStateLabelAssignment,
		},
		{
			bs:    "this that",
			scan:  "this",
			state: TokenizerStateSymbol,
		},
		{
			bs:    "this",
			scan:  "this",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			bs:    "(abc",
			scan:  "(",
			state: TokenizerStateOpenParen,
		},
		{
			bs:    ")abc",
			scan:  ")",
			state: TokenizerStateCloseParen,
		},
		{
			bs:    "'abc",
			scan:  "'",
			state: TokenizerStateQuote,
		},
		{
			bs:    "1234",
			scan:  "1234",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "1.2",
			scan:  "1.2",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    ".2",
			scan:  ".2",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "2/10",
			scan:  "2/10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedRationalFraction,
		},
		{
			bs:    "2/-10",
			scan:  "2/",
			err0:  &TokenizerError{mess: MessageExpectingNumber},
			state: TokenizerStateUnsignedRationalFraction,
		},
		{
			bs:    "+2/10",
			scan:  "+2/10",
			err0:  io.EOF,
			state: TokenizerStateSignedRationalFraction,
		},
		{
			bs:    "-2/10",
			scan:  "-2/10",
			err0:  io.EOF,
			state: TokenizerStateSignedRationalFraction,
		},
		{
			bs:    "2/10.10",
			scan:  "2/10",
			state: TokenizerStateUnsignedRationalFraction,
		},
		{
			// R7RS: <decimal 10> → <digit 10>+ . <digit 10>* allows zero digits after dot
			// "2." is valid decimal fraction, "/10" is next token (identifier)
			bs:    "2./10",
			scan:  "2.",
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "0.0034",
			scan:  "0.0034",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "0.0034e10",
			scan:  "0.0034e10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "34e10",
			scan:  "34e10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "-34e10",
			scan:  "-34e10",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    "+34e10",
			scan:  "+34e10",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    ".34e10",
			scan:  ".34e10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    ".34e10+inf.0i",
			scan:  ".34e10",
			err0:  nil,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "0.0034e+10",
			scan:  "0.0034e+10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "34e+10",
			scan:  "34e+10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    ".34e+10",
			scan:  ".34e+10",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    ".34e+10+inf.0i",
			scan:  ".34e+10",
			err0:  nil,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "123a",
			scan:  "123",
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "-nan,0",
			scan:  "-nan",
			err0:  &TokenizerError{mess: "expected decimal fraction"},
			state: TokenizerStateSymbol,
		},
		{
			bs:    "-ifoo",
			scan:  "-i",
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "-i",
			scan:  "-i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "+i",
			scan:  "+i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "-inf.0",
			scan:  "-inf.0",
			err0:  io.EOF,
			state: TokenizerStateSignedInf,
		},
		{
			bs:    "+inf.0",
			scan:  "+inf.0",
			err0:  io.EOF,
			state: TokenizerStateSignedInf,
		},
		{
			bs:    "-nan.0",
			scan:  "-nan.0",
			err0:  io.EOF,
			state: TokenizerStateSignedNan,
		},
		{
			bs:    "+nan.0",
			scan:  "+nan.0",
			err0:  io.EOF,
			state: TokenizerStateSignedNan,
		},
		{
			bs:    "-nan.0i",
			scan:  "-nan.0i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginaryNan,
		},
		{
			bs:    "+inf.0i",
			scan:  "+inf.0i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginaryInf,
		},
		{
			bs:    "+inf.0izz",
			scan:  "+inf.0i",
			state: TokenizerStateSignedImaginaryInf,
		},
		{
			bs:    "+nan.0izz",
			scan:  "+nan.0i",
			state: TokenizerStateSignedImaginaryNan,
		},
		// Quotation tokens
		{
			bs:    ",",
			scan:  ",",
			err0:  io.EOF,
			state: TokenizerStateUnquote,
		},
		{
			bs:    ",@",
			scan:  ",@",
			err0:  io.EOF,
			state: TokenizerStateUnquoteSplicing,
		},
		{
			bs:    "`",
			scan:  "`",
			err0:  io.EOF,
			state: TokenizerStateQuasiquote,
		},
		{
			// syntax quote
			bs:    "#'",
			scan:  "#'",
			err0:  io.EOF,
			state: TokenizerStateSyntax,
		},
		{
			// unsyntax
			bs:    "#,",
			scan:  "#,",
			err0:  io.EOF,
			state: TokenizerStateUnsyntax,
		},
		{
			// unsyntax-splicing
			bs:    "#,@",
			scan:  "#,@",
			err0:  io.EOF,
			state: TokenizerStateUnsyntaxSplicing,
		},
		{
			// quasisyntax
			bs:    "#`",
			scan:  "#`",
			err0:  io.EOF,
			state: TokenizerStateQuasisyntax,
		},
		// Signed numbers
		{
			// signed positive integer
			bs:    "+123",
			scan:  "+123",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			// signed negative integer
			bs:    "-456",
			scan:  "-456",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			// signed positive decimal
			bs:    "+1.5",
			scan:  "+1.5",
			err0:  io.EOF,
			state: TokenizerStateSignedDecimalFraction,
		},
		{
			// signed negative decimal
			bs:    "-2.5",
			scan:  "-2.5",
			err0:  io.EOF,
			state: TokenizerStateSignedDecimalFraction,
		},
		// Complex numbers (rectangular)
		{
			// unsigned complex
			bs:    "1+2i",
			scan:  "1+2i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		{
			// unsigned complex with negative imaginary
			bs:    "3-4i",
			scan:  "3-4i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		{
			// signed complex (positive real)
			bs:    "+1+2i",
			scan:  "+1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// signed complex (negative real)
			bs:    "-1+2i",
			scan:  "-1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		// Complex numbers (polar)
		{
			// unsigned polar
			bs:    "1@1.5708",
			scan:  "1@1.5708",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplexPolar,
		},
		{
			// signed polar (positive)
			bs:    "+1@1.5708",
			scan:  "+1@1.5708",
			err0:  io.EOF,
			state: TokenizerStateSignedComplexPolar,
		},
		{
			// signed polar (negative)
			bs:    "-1@1.5708",
			scan:  "-1@1.5708",
			err0:  io.EOF,
			state: TokenizerStateSignedComplexPolar,
		},
		// Dot (cons)
		{
			// dot in improper list context
			bs:    ". ",
			scan:  ".",
			state: TokenizerStateCons,
		},
		// String
		{
			// complete string
			bs:    `"hello"`,
			scan:  `"hello"`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		{
			// string with escape
			bs:    `"hello\nworld"`,
			scan:  `"hello\nworld"`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// Vector
		{
			// vector literal
			bs:    "#(",
			scan:  "#(",
			err0:  io.EOF,
			state: TokenizerStateOpenVector,
		},
		// Comments
		{
			// block comment
			bs:    "#| comment |#",
			scan:  "#| comment |#",
			err0:  io.EOF,
			state: TokenizerStateBlockCommentBody,
		},
		{
			// datum comment
			bs:    "#;",
			scan:  "#;",
			err0:  io.EOF,
			state: TokenizerStateDatumCommentBegin,
		},
		{
			// line comment
			bs:    "; comment\n",
			scan:  "; comment",
			state: TokenizerStateLineCommentBody,
		},
		// === EDGE CASES ===
		// Numbers with leading zeros
		{
			bs:    "007",
			scan:  "007",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "00.5",
			scan:  "00.5",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		// Trailing dot (decimal with no fractional part)
		{
			bs:    "1.",
			scan:  "1.",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		{
			bs:    "+1.",
			scan:  "+1.",
			err0:  io.EOF,
			state: TokenizerStateSignedDecimalFraction,
		},
		{
			bs:    "-1.",
			scan:  "-1.",
			err0:  io.EOF,
			state: TokenizerStateSignedDecimalFraction,
		},
		// Complex with infnan parts
		{
			bs:    "1+inf.0i",
			scan:  "1+inf.0i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		{
			bs:    "1-inf.0i",
			scan:  "1-inf.0i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		{
			bs:    "1+nan.0i",
			scan:  "1+nan.0i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		// R7RS: complex with infnan real and imaginary parts
		{
			bs:    "+inf.0+inf.0i",
			scan:  "+inf.0+inf.0i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			bs:    "-inf.0-inf.0i",
			scan:  "-inf.0-inf.0i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		// Pure imaginary with coefficient
		{
			bs:    "+2i",
			scan:  "+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "-3i",
			scan:  "-3i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "+2.5i",
			scan:  "+2.5i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		{
			bs:    "-3.5i",
			scan:  "-3.5i",
			err0:  io.EOF,
			state: TokenizerStateSignedImaginary,
		},
		// Exponents edge cases
		{
			bs:    "1e0",
			scan:  "1e0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "1e-0",
			scan:  "1e-0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "1e+0",
			scan:  "1e+0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedInteger,
		},
		{
			bs:    "-1e-0",
			scan:  "-1e-0",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    "-1e+0",
			scan:  "-1e+0",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    "+1e-0",
			scan:  "+1e-0",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    "+1e+0",
			scan:  "+1e+0",
			err0:  io.EOF,
			state: TokenizerStateSignedInteger,
		},
		{
			bs:    ".5e0",
			scan:  ".5e0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedDecimalFraction,
		},
		// Empty string
		{
			bs:    `""`,
			scan:  `""`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// String with only escape sequences
		{
			bs:    `"\n\t\r"`,
			scan:  `"\n\t\r"`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// String with hex escape
		{
			bs:    `"\x41;"`,
			scan:  `"\x41;"`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// String with embedded quote
		{
			bs:    `"say \"hello\""`,
			scan:  `"say \"hello\""`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// String with backslash escape
		{
			bs:    `"path\\to\\file"`,
			scan:  `"path\\to\\file"`,
			err0:  io.EOF,
			state: TokenizerStateString,
		},
		// Character edge cases
		{
			// space character
			bs:    `#\space`,
			scan:  `#\space`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// newline character
			bs:    `#\newline`,
			scan:  `#\newline`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// tab character
			bs:    `#\tab`,
			scan:  `#\tab`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// return character
			bs:    `#\return`,
			scan:  `#\return`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// null character
			bs:    `#\null`,
			scan:  `#\null`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// alarm/bell character
			bs:    `#\alarm`,
			scan:  `#\alarm`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// delete character
			bs:    `#\delete`,
			scan:  `#\delete`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// escape character
			bs:    `#\escape`,
			scan:  `#\escape`,
			err0:  io.EOF,
			state: TokenizerStateCharMnemonic,
		},
		{
			// hex character code 0
			bs:    `#\x00`,
			scan:  `#\x00`,
			err0:  io.EOF,
			state: TokenizerStateCharHexEscape,
		},
		{
			// hex character code max ascii
			bs:    `#\x7F`,
			scan:  `#\x7F`,
			err0:  io.EOF,
			state: TokenizerStateCharHexEscape,
		},
		// Sym edge cases
		{
			// peculiar identifier: just +
			bs:    "+ ",
			scan:  "+",
			state: TokenizerStateSymbol,
		},
		{
			// peculiar identifier: just -
			bs:    "- ",
			scan:  "-",
			state: TokenizerStateSymbol,
		},
		{
			// peculiar identifier: ...
			bs:    "...",
			scan:  "...",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// R7RS peculiar identifier: -> followed by subsequent chars
			bs:    "->foo",
			scan:  "->foo",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// R7RS: @ is a valid <special subsequent> (can appear after initial)
			bs:    "foo@bar",
			scan:  "foo@bar",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// symbol with numbers
			bs:    "foo123",
			scan:  "foo123",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// symbol with special chars
			bs:    "foo-bar!",
			scan:  "foo-bar!",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// symbol with question mark
			bs:    "null?",
			scan:  "null?",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		// Rational edge cases
		{
			// large rational
			bs:    "999999/1000000",
			scan:  "999999/1000000",
			err0:  io.EOF,
			state: TokenizerStateUnsignedRationalFraction,
		},
		{
			// rational with numerator 0
			bs:    "0/1",
			scan:  "0/1",
			err0:  io.EOF,
			state: TokenizerStateUnsignedRationalFraction,
		},
		// Polar edge cases
		{
			// polar with 0 angle
			bs:    "1@0",
			scan:  "1@0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplexPolar,
		},
		{
			// polar with negative angle
			bs:    "1@-3.14",
			scan:  "1@-3.14",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplexPolar,
		},
		{
			// polar with positive angle
			bs:    "1@+3.14",
			scan:  "1@+3.14",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplexPolar,
		},
		// Comment edge cases
		{
			// empty line comment
			bs:    ";\n",
			scan:  ";",
			state: TokenizerStateLineCommentBody,
		},
		{
			// empty block comment
			bs:    "#||#",
			scan:  "#||#",
			err0:  io.EOF,
			state: TokenizerStateBlockCommentBody,
		},
		{
			// nested block comment
			bs:    "#| outer #| inner |# outer |#",
			scan:  "#| outer #| inner |# outer |#",
			err0:  io.EOF,
			state: TokenizerStateBlockCommentBody,
		},
		// Boolean edge cases - R7RS requires case-insensitive booleans
		// (tokenizer handles this regardless of ci flag)
		{
			// uppercase TRUE
			bs:    "#TRUE",
			scan:  "#TRUE",
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanTrue,
		},
		{
			// uppercase FALSE
			bs:    "#FALSE",
			scan:  "#FALSE",
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanFalse,
		},
		{
			// uppercase T
			bs:    "#T",
			scan:  "#T",
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanTrue,
		},
		{
			// uppercase F
			bs:    "#F",
			scan:  "#F",
			err0:  io.EOF,
			state: TokenizerStateMarkerBooleanFalse,
		},
		// Radix with number edge cases
		{
			// binary 0
			bs:    "#b0",
			scan:  "#b",
			state: TokenizerStateMarkerBase2,
		},
		{
			// octal with max digit
			bs:    "#o7",
			scan:  "#o",
			state: TokenizerStateMarkerBase8,
		},
		{
			// hex with letters
			bs:    "#xABCDEF",
			scan:  "#x",
			state: TokenizerStateMarkerBase16,
		},
		{
			// hex with mixed case
			bs:    "#xAbCdEf",
			scan:  "#x",
			state: TokenizerStateMarkerBase16,
		},
		// Complex with exponents
		{
			bs:    "1e2+3e4i",
			scan:  "1e2+3e4i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		{
			bs:    "1.5e2+2.5e3i",
			scan:  "1.5e2+2.5e3i",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplex,
		},
		// Polar with exponents
		{
			bs:    "1e2@3e4",
			scan:  "1e2@3e4",
			err0:  io.EOF,
			state: TokenizerStateUnsignedComplexPolar,
		},
		// Delimiter boundary tests
		{
			// number followed by paren
			bs:    "123(",
			scan:  "123",
			state: TokenizerStateUnsignedInteger,
		},
		{
			// number followed by close paren
			bs:    "123)",
			scan:  "123",
			state: TokenizerStateUnsignedInteger,
		},
		{
			// symbol followed by quote
			bs:    "foo'",
			scan:  "foo",
			state: TokenizerStateSymbol,
		},
		{
			// symbol followed by comma
			bs:    "foo,",
			scan:  "foo",
			state: TokenizerStateSymbol,
		},
		{
			// symbol followed by backtick
			bs:    "foo`",
			scan:  "foo",
			state: TokenizerStateSymbol,
		},
		// Unicode - R7RS allows Unicode letters in identifiers
		{
			// Unicode symbol (lambda)
			bs:    "λ",
			scan:  "λ",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			// unicode character constant (lambda)
			bs:    `#\λ`,
			scan:  `#\λ`,
			err0:  io.EOF,
			state: TokenizerStateCharGraphic,
		},
		// Label edge cases
		{
			// label 0
			bs:    "#0#",
			scan:  "#0#",
			err0:  io.EOF,
			state: TokenizerStateLabelReference,
		},
		{
			// large label
			bs:    "#999#",
			scan:  "#999#",
			err0:  io.EOF,
			state: TokenizerStateLabelReference,
		},
		{
			// label assignment 0
			bs:    "#0=",
			scan:  "#0=",
			err0:  io.EOF,
			state: TokenizerStateLabelAssignment,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.bs, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.bs), tc.ci)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.span()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.scan)
		})
	}
}

func TestTokenizer_Text(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("hello"), false)
	_, _ = p.Next()
	c.Assert(p.Text(), qt.Equals, "hello")
}

func TestTokenizer_Reader(t *testing.T) {
	c := qt.New(t)
	rdr := strings.NewReader("test")
	p := NewTokenizer(rdr, false)
	c.Assert(p.Reader(), qt.Equals, rdr)
}

func TestTokenizer_Close(t *testing.T) {
	c := qt.New(t)
	// strings.Reader doesn't implement Close, so Close should handle gracefully
	p := NewTokenizer(strings.NewReader("test"), false)
	err := p.Close()
	c.Assert(err, qt.IsNil)
	c.Assert(p.rdr, qt.IsNil)
}

func TestSimpleToken_StartEnd(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("hello world"), false)
	tok, err := p.Next()
	c.Assert(err, qt.IsNil)
	st := tok.(*SimpleToken)
	start := st.Start()
	end := st.End()
	c.Assert(start.Index(), qt.Equals, 0)
	c.Assert(end.Index(), qt.Equals, 5)
}

func TestSimpleToken_SchemeString(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("foo"), false)
	tok, _ := p.Next()
	st := tok.(*SimpleToken)
	ss := st.SchemeString()
	c.Assert(ss, qt.Contains, "foo")
	c.Assert(ss, qt.Contains, "simple-token")
}

func TestSimpleToken_IsVoid(t *testing.T) {
	c := qt.New(t)

	// Non-nil token
	p := NewTokenizer(strings.NewReader("test"), false)
	tok, _ := p.Next()
	st := tok.(*SimpleToken)
	c.Assert(st.IsVoid(), qt.IsFalse)

	// Nil token
	var nilTok *SimpleToken = nil
	c.Assert(nilTok.IsVoid(), qt.IsTrue)
}

func TestSimpleToken_EqualTo(t *testing.T) {
	c := qt.New(t)

	// Equal tokens
	p1 := NewTokenizer(strings.NewReader("foo"), false)
	tok1, _ := p1.Next()
	p2 := NewTokenizer(strings.NewReader("foo"), false)
	tok2, _ := p2.Next()
	c.Assert(tok1.(*SimpleToken).EqualTo(tok2.(*SimpleToken)), qt.IsTrue)

	// Different source
	p3 := NewTokenizer(strings.NewReader("bar"), false)
	tok3, _ := p3.Next()
	c.Assert(tok1.(*SimpleToken).EqualTo(tok3.(*SimpleToken)), qt.IsFalse)

	// Different type (non-SimpleToken)
	c.Assert(tok1.(*SimpleToken).EqualTo(nil), qt.IsFalse)
}

func TestTokenizerError_Is(t *testing.T) {
	c := qt.New(t)
	err1 := NewTokenizerError("test error", syntax.SourceIndexes{}, syntax.SourceIndexes{})
	err2 := NewTokenizerError("another error", syntax.SourceIndexes{}, syntax.SourceIndexes{})

	c.Assert(err1.Is(err2), qt.IsTrue)    // Both are TokenizerError
	c.Assert(err1.Is(io.EOF), qt.IsFalse) // Not a TokenizerError
}

func TestTokenizerError_Unwrap(t *testing.T) {
	c := qt.New(t)

	// Error without wrap
	err1 := NewTokenizerError("test", syntax.SourceIndexes{}, syntax.SourceIndexes{})
	c.Assert(err1.Unwrap(), qt.IsNil)

	// Error with wrap
	wrapped := io.EOF
	err2 := NewTokenizerErrorWithWrap(wrapped, "wrapped", syntax.SourceIndexes{}, syntax.SourceIndexes{})
	c.Assert(err2.Unwrap(), qt.Equals, wrapped)
}

func TestTokenizerError_Error(t *testing.T) {
	c := qt.New(t)
	err := NewTokenizerError("my error message", syntax.SourceIndexes{}, syntax.SourceIndexes{})
	c.Assert(err.Error(), qt.Equals, "my error message")
}

func TestSimpleToken_EqualTo_DifferentType(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("42"), false)
	tok1, _ := p.Next()

	p2 := NewTokenizer(strings.NewReader("foo"), false)
	tok2, _ := p2.Next()

	// Different token types
	c.Assert(tok1.(*SimpleToken).EqualTo(tok2.(*SimpleToken)), qt.IsFalse)
}

func TestSimpleToken_EqualTo_DifferentPositions(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("foo bar"), false)
	tok1, _ := p.Next()
	tok2, _ := p.Next()

	// Different positions (start/end differ)
	c.Assert(tok1.(*SimpleToken).EqualTo(tok2.(*SimpleToken)), qt.IsFalse)
}

func TestTokenizer_scanLineEnding(t *testing.T) {
	tcs := []struct {
		name     string
		input    string
		expected bool
	}{
		{"newline", "\nrest", true},
		{"carriage_return", "\rrest", true},
		{"crlf", "\r\nrest", true},
		{"no_line_ending", "abc", false},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.input), false)
			ok := p.scanLineEnding()
			c.Assert(ok, qt.Equals, tc.expected)
		})
	}
}

func TestTokenizer_escape(t *testing.T) {
	tcs := []struct {
		input    rune
		expected rune
	}{
		{'0', 0},
		{'a', '\a'},
		{'b', '\b'},
		{'t', '\t'},
		{'n', '\n'},
		{'r', '\r'},
		{'x', 0}, // unknown escape
	}
	for _, tc := range tcs {
		qt.New(t).Run(string(tc.input), func(c *qt.C) {
			p := &Tokenizer{cur: tc.input}
			result := p.escape()
			c.Assert(result, qt.Equals, tc.expected)
		})
	}
}

func TestTokenizer_reset(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("test"), false)
	p.tokenEnd = syntax.SourceIndexes{}
	p.tokenEnd.Inc(5)
	p.err = io.EOF

	p.reset()

	c.Assert(p.err, qt.IsNil)
	c.Assert(p.tokenStart, qt.Equals, p.tokenEnd)
}

func TestTokenizer_isEOF(t *testing.T) {
	c := qt.New(t)

	// Not at EOF
	p := NewTokenizer(strings.NewReader("test"), false)
	c.Assert(p.isEOF(), qt.IsFalse)

	// At EOF
	p2 := NewTokenizer(strings.NewReader(""), false)
	c.Assert(p2.isEOF(), qt.IsTrue)
}

func TestTokenizer_this(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader("abc"), false)
	r := p.this()
	c.Assert(r, qt.Equals, 'a')
}

// Additional tokenization edge cases
func TestTokenizer_TokenStream_EdgeCases(t *testing.T) {
	tcs := []struct {
		in     string
		tokens []TokenizerState
	}{
		// Peculiar identifiers
		{"+", []TokenizerState{TokenizerStateSymbol}},
		{"-", []TokenizerState{TokenizerStateSymbol}},
		// Various radix markers
		{"#b101", []TokenizerState{TokenizerStateMarkerBase2, TokenizerStateUnsignedIntegerBase2}},
		{"#o777", []TokenizerState{TokenizerStateMarkerBase8, TokenizerStateUnsignedIntegerBase8}},
		{"#d123", []TokenizerState{TokenizerStateMarkerBase10, TokenizerStateUnsignedIntegerBase10}},
		// Whitespace handling
		{"  \t\n  foo", []TokenizerState{TokenizerStateSymbol}},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.in, func(c *qt.C) {
			ts, _ := Tokenize(tc.in, false)
			c.Assert(len(ts), qt.Equals, len(tc.tokens))
			for i := range ts {
				c.Assert(ts[i].(*SimpleToken).typ, qt.Equals, tc.tokens[i])
			}
		})
	}
}

// Test helper functions
func TestIsLetter(t *testing.T) {
	c := qt.New(t)
	c.Assert(isUnicodeLetter('a'), qt.IsTrue)
	c.Assert(isUnicodeLetter('z'), qt.IsTrue)
	c.Assert(isUnicodeLetter('A'), qt.IsTrue)
	c.Assert(isUnicodeLetter('Z'), qt.IsTrue)
	c.Assert(isUnicodeLetter('0'), qt.IsFalse)
	c.Assert(isUnicodeLetter('-'), qt.IsFalse)
}

func TestIsDigit(t *testing.T) {
	c := qt.New(t)
	// Binary
	c.Assert(isDigit(2, '0'), qt.IsTrue)
	c.Assert(isDigit(2, '1'), qt.IsTrue)
	c.Assert(isDigit(2, '2'), qt.IsFalse)
	// Octal
	c.Assert(isDigit(8, '7'), qt.IsTrue)
	c.Assert(isDigit(8, '8'), qt.IsFalse)
	// Decimal
	c.Assert(isDigit(10, '9'), qt.IsTrue)
	c.Assert(isDigit(10, 'a'), qt.IsFalse)
	// Hex
	c.Assert(isDigit(16, '9'), qt.IsTrue)
	c.Assert(isDigit(16, 'a'), qt.IsTrue)
	c.Assert(isDigit(16, 'f'), qt.IsTrue)
	c.Assert(isDigit(16, 'A'), qt.IsTrue)
	c.Assert(isDigit(16, 'F'), qt.IsTrue)
	c.Assert(isDigit(16, 'g'), qt.IsFalse)
}

func TestIsDelimiter(t *testing.T) {
	c := qt.New(t)
	c.Assert(isDelimiter(' '), qt.IsTrue)
	c.Assert(isDelimiter('\t'), qt.IsTrue)
	c.Assert(isDelimiter('|'), qt.IsTrue)
	c.Assert(isDelimiter('\n'), qt.IsTrue)
	c.Assert(isDelimiter('\r'), qt.IsTrue)
	c.Assert(isDelimiter('a'), qt.IsFalse)
}

func TestIsSpecialInitial(t *testing.T) {
	c := qt.New(t)
	specials := []rune{'!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~'}
	for _, s := range specials {
		c.Assert(isSpecialInitial(s), qt.IsTrue)
	}
	c.Assert(isSpecialInitial('a'), qt.IsFalse)
	c.Assert(isSpecialInitial('1'), qt.IsFalse)
}

func TestIsExplicitSign(t *testing.T) {
	c := qt.New(t)
	c.Assert(isExplicitSign('+'), qt.IsTrue)
	c.Assert(isExplicitSign('-'), qt.IsTrue)
	c.Assert(isExplicitSign('*'), qt.IsFalse)
}

func TestIsNumberInitial(t *testing.T) {
	c := qt.New(t)
	c.Assert(isNumberInitial(10, '+'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '-'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '.'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '5'), qt.IsTrue)
	c.Assert(isNumberInitial(10, 'a'), qt.IsFalse)
	c.Assert(isNumberInitial(16, 'a'), qt.IsTrue)
	c.Assert(isNumberInitial(16, 'F'), qt.IsTrue)
	c.Assert(isNumberInitial(2, '0'), qt.IsTrue)
	c.Assert(isNumberInitial(2, '2'), qt.IsFalse)
	c.Assert(isNumberInitial(8, '7'), qt.IsTrue)
}

func TestDigit(t *testing.T) {
	tcs := []struct {
		radix    int
		char     rune
		expected int
	}{
		// Radix 2
		{radix: 2, char: '0', expected: 0},
		{radix: 2, char: '1', expected: 1},
		// Radix 8
		{radix: 8, char: '0', expected: 0},
		{radix: 8, char: '7', expected: 7},
		// Radix 10
		{radix: 10, char: '0', expected: 0},
		{radix: 10, char: '5', expected: 5},
		// Radix 16 - digits (c < '9' branch)
		{radix: 16, char: '0', expected: 0},
		{radix: 16, char: '8', expected: 8},
		// Radix 16 - uppercase hex A-E (c < 'F' branch)
		{radix: 16, char: 'A', expected: 10},
		{radix: 16, char: 'E', expected: 14},
		// Radix 16 - lowercase hex a-e (c < 'f' branch)
		{radix: 16, char: 'a', expected: 10},
		{radix: 16, char: 'e', expected: 14},
		// Unicode characters (c >= utf8.RuneSelf returns -1)
		{radix: 10, char: '©', expected: -1},
		{radix: 16, char: 'é', expected: -1},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: radix=%d char=%q", i, tc.radix, tc.char), func(c *qt.C) {
			c.Check(digit(tc.radix, tc.char), qt.Equals, tc.expected)
		})
	}
}

func TestIsLineEnding(t *testing.T) {
	c := qt.New(t)
	c.Assert(isLineEnding('\n'), qt.IsTrue)
	c.Assert(isLineEnding('\r'), qt.IsTrue)
	c.Assert(isLineEnding(' '), qt.IsFalse)
}

func TestIsSymbolInitial(t *testing.T) {
	c := qt.New(t)
	c.Assert(isSymbolInitial('a'), qt.IsTrue)
	c.Assert(isSymbolInitial('!'), qt.IsTrue)
	c.Assert(isSymbolInitial('1'), qt.IsFalse)
}

func TestIsSubsequent(t *testing.T) {
	c := qt.New(t)
	c.Assert(isSubsequent('a'), qt.IsTrue)
	c.Assert(isSubsequent('1'), qt.IsTrue)
	c.Assert(isSubsequent('.'), qt.IsTrue)
	c.Assert(isSubsequent('+'), qt.IsTrue)
	c.Assert(isSubsequent('@'), qt.IsTrue)
}

// Tests for complex tokenization scenarios to improve coverage

func TestPeculiarIdentifiers(t *testing.T) {
	c := qt.New(t)

	// Ellipsis
	tok := NewTokenizer(strings.NewReader("..."), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateSymbol)
	c.Assert(token.String(), qt.Equals, "...")

	// Plus and minus as identifiers
	tok2 := NewTokenizer(strings.NewReader("(+ -)"), false)
	_, _ = tok2.Next() // (
	plus, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(plus.String(), qt.Equals, "+")
	minus, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(minus.String(), qt.Equals, "-")
}

func TestRadixPrefixes(t *testing.T) {
	c := qt.New(t)

	// Binary number produces two tokens: marker + integer
	tok := NewTokenizer(strings.NewReader("#b101"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateMarkerBase2)
	token1b, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1b.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase2)
	c.Assert(token1b.String(), qt.Equals, "101")

	// Octal number
	tok2 := NewTokenizer(strings.NewReader("#o77"), false)
	token2, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateMarkerBase8)
	token2b, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2b.String(), qt.Equals, "77")

	// Hex number
	tok3 := NewTokenizer(strings.NewReader("#xAB"), false)
	token3, err := tok3.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token3.Type(), qt.Equals, TokenizerStateMarkerBase16)
	token3b, err := tok3.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token3b.String(), qt.Equals, "AB")
}

func TestBytevectorLiteral(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#u8(1 2 3)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateOpenVectorUnsignedByteMarker)
}

func TestNamedCharactersExtra(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// Named character: newline
		{bs: `#\newline`, state: TokenizerStateCharMnemonic},
		// Named character: tab
		{bs: `#\tab`, state: TokenizerStateCharMnemonic},
		// Named character: return
		{bs: `#\return`, state: TokenizerStateCharMnemonic},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestStringEscapes(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Hex escape
		{bs: `"\x41;"`, scan: `"\x41;"`, state: TokenizerStateString},
		// Tab escape \t
		{bs: `"\t"`, scan: `"\t"`, state: TokenizerStateString},
		// Newline escape \n
		{bs: `"\n"`, scan: `"\n"`, state: TokenizerStateString},
		// Return escape \r
		{bs: `"\r"`, scan: `"\r"`, state: TokenizerStateString},
		// Backslash escape \\
		{bs: `"\\"`, scan: `"\\"`, state: TokenizerStateString},
		// Quote escape \"
		{bs: `"\""`, scan: `"\""`, state: TokenizerStateString},
		// Bell escape \a
		{bs: `"\a"`, scan: `"\a"`, state: TokenizerStateString},
		// Backspace escape \b
		{bs: `"\b"`, scan: `"\b"`, state: TokenizerStateString},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestDatumLabels(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// Datum label definition
		{bs: "#0=", state: TokenizerStateLabelAssignment},
		// Datum label reference
		{bs: "#0#", state: TokenizerStateLabelReference},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestExactnessMarkers(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// Exact marker alone
		{bs: "#e", state: TokenizerStateMarkerNumberExact},
		// Inexact marker alone
		{bs: "#i", state: TokenizerStateMarkerNumberInexact},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestDirective(t *testing.T) {
	c := qt.New(t)

	// Directive pragma
	tok := NewTokenizer(strings.NewReader("#!fold-case"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateDirective)
}

func TestSyntaxQuote(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// Syntax quote #'
		{bs: "#'foo", state: TokenizerStateSyntax},
		// Quasisyntax #`
		{bs: "#`foo", state: TokenizerStateQuasisyntax},
		// Unsyntax #,
		{bs: "#,foo", state: TokenizerStateUnsyntax},
		// Unsyntax-splicing #,@
		{bs: "#,@foo", state: TokenizerStateUnsyntaxSplicing},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestDatumComment(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#;"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
}

func TestVector(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#(1 2 3)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateOpenVector)
}

func TestComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// +inf.0
		{bs: "+inf.0", state: TokenizerStateSignedInf},
		// -nan.0
		{bs: "-nan.0", state: TokenizerStateSignedNan},
		// +inf.0i (imaginary inf)
		{bs: "+inf.0i", state: TokenizerStateSignedImaginaryInf},
		// -nan.0i (imaginary nan)
		{bs: "-nan.0i", state: TokenizerStateSignedImaginaryNan},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

// ============================================================================
// Comment Tokenization Tests (emitComments=true)
// ============================================================================

func TestLineCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Simple line comment: ; comment
	tok := NewTokenizerWithComments(strings.NewReader("; this is a comment\n"), false)

	// Should get LineCommentBody (the comment content)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, "; this is a comment")

	// Should get EOF
	_, err = tok.Next()
	c.Assert(err, qt.Equals, io.EOF)
}

func TestLineCommentMultipleSemicolons(t *testing.T) {
	c := qt.New(t)

	// Multiple semicolons: ;;; comment
	tok := NewTokenizerWithComments(strings.NewReader(";;; triple\n"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, ";;; triple")
}

func TestLineCommentAtEOF(t *testing.T) {
	c := qt.New(t)

	// Comment without trailing newline (EOF terminates)
	tok := NewTokenizerWithComments(strings.NewReader("; no newline"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, "; no newline")

	// No End token at EOF - just returns EOF directly
	_, err = tok.Next()
	c.Assert(err, qt.Equals, io.EOF)
}

func TestLineCommentEmpty(t *testing.T) {
	c := qt.New(t)

	// Empty comment (just semicolon and newline)
	tok := NewTokenizerWithComments(strings.NewReader(";\n"), false)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token2.String(), qt.Equals, ";")
}

func TestBlockCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Simple block comment: #| comment |#
	tok := NewTokenizerWithComments(strings.NewReader("#| block comment |#"), false)

	// Should get BlockCommentBody (the content)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token2.String(), qt.Equals, "#| block comment |#")
}

func TestBlockCommentEmpty(t *testing.T) {
	c := qt.New(t)

	// Empty block comment: #||#
	tok := NewTokenizerWithComments(strings.NewReader("#||#"), false)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#||#")
}

func TestBlockCommentMultiline(t *testing.T) {
	c := qt.New(t)

	// Multiline block comment
	tok := NewTokenizerWithComments(strings.NewReader("#| line1\nline2\nline3 |#"), false)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| line1\nline2\nline3 |#")
}

func TestBlockCommentNested(t *testing.T) {
	c := qt.New(t)

	// Nested block comment: #| outer #| inner |# outer |#
	tok := NewTokenizerWithComments(strings.NewReader("#| outer #| inner |# outer |#"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| outer #| inner |# outer |#")
}

func TestBlockCommentUnclosed(t *testing.T) {
	c := qt.New(t)

	// Unclosed block comment (EOF before |#)
	tok := NewTokenizerWithComments(strings.NewReader("#| unclosed"), false)
	// Body ends at EOF, no End token
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| unclosed")

	// Next call should return EOF
	_, err = tok.Next()
	c.Assert(err, qt.Equals, io.EOF)
}

func TestDatumCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Datum comment: #; datum
	tok := NewTokenizerWithComments(strings.NewReader("#;42"), false)

	// Should get DatumCommentBegin
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
	c.Assert(token1.String(), qt.Equals, "#;")

	// The datum itself follows (parser would handle this)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateUnsignedInteger)
	c.Assert(token2.String(), qt.Equals, "42")
}

func TestCommentFollowedByCode(t *testing.T) {
	c := qt.New(t)

	// Comment followed by code
	tok := NewTokenizerWithComments(strings.NewReader("; comment\n42"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// Then the code
	token4, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token4.Type(), qt.Equals, TokenizerStateUnsignedInteger)
	c.Assert(token4.String(), qt.Equals, "42")
}

// ============================================================================
// Complex Number Tokenization Tests
// ============================================================================

func TestImaginaryWithCoefficient(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Positive integer imaginary
		{bs: "+3i", scan: "+3i", state: TokenizerStateSignedImaginary},
		// Negative integer imaginary
		{bs: "-2i", scan: "-2i", state: TokenizerStateSignedImaginary},
		// Positive decimal imaginary
		{bs: "+3.5i", scan: "+3.5i", state: TokenizerStateSignedImaginary},
		// Negative decimal imaginary
		{bs: "-2.5i", scan: "-2.5i", state: TokenizerStateSignedImaginary},
		// Decimal starting with dot
		{bs: "+.5i", scan: "+.5i", state: TokenizerStateSignedImaginary},
		// Scientific notation imaginary with decimal
		// Note: +3e2i doesn't work because signed integers don't parse exponents (pre-existing limitation)
		{bs: "+3.0e2i", scan: "+3.0e2i", state: TokenizerStateSignedImaginary},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestFullComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Simple complex
		{bs: "1+2i", scan: "1+2i", state: TokenizerStateUnsignedComplex},
		// Complex with negative imaginary
		{bs: "3-4i", scan: "3-4i", state: TokenizerStateUnsignedComplex},
		// Decimal complex
		{bs: "1.5+2.5i", scan: "1.5+2.5i", state: TokenizerStateUnsignedComplex},
		// Decimal complex with negative imaginary
		{bs: "3.5-4.5i", scan: "3.5-4.5i", state: TokenizerStateUnsignedComplex},
		// Complex with unit imaginary
		{bs: "1+i", scan: "1+i", state: TokenizerStateUnsignedComplex},
		// Complex with negative unit imaginary
		{bs: "5-i", scan: "5-i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumbersWithExponents(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Complex with exponent in real part
		{bs: "1e2+3i", scan: "1e2+3i", state: TokenizerStateUnsignedComplex},
		// Complex with exponent in imaginary part
		{bs: "1+3e2i", scan: "1+3e2i", state: TokenizerStateUnsignedComplex},
		// Complex with decimal and exponent
		{bs: "1.5e2+2.5e3i", scan: "1.5e2+2.5e3i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumberEdgeCases(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Zero real part with imaginary
		{bs: "0+3i", scan: "0+3i", state: TokenizerStateUnsignedComplex},
		// Zero imaginary coefficient would still need the 'i'
		{bs: "1+0i", scan: "1+0i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumbersInExpressions(t *testing.T) {
	c := qt.New(t)

	// Complex number followed by delimiter
	tok := NewTokenizer(strings.NewReader("1+2i)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	c.Assert(token.String(), qt.Equals, "1+2i")

	// Verify the delimiter is still there
	tokenParen, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tokenParen.Type(), qt.Equals, TokenizerStateCloseParen)

	// Multiple complex numbers in a list
	tok2 := NewTokenizer(strings.NewReader("(1+2i 3-4i)"), false)
	t1, _ := tok2.Next() // (
	c.Assert(t1.Type(), qt.Equals, TokenizerStateOpenParen)
	t2, _ := tok2.Next() // 1+2i
	c.Assert(t2.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	t3, _ := tok2.Next() // 3-4i
	c.Assert(t3.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	t4, _ := tok2.Next() // )
	c.Assert(t4.Type(), qt.Equals, TokenizerStateCloseParen)
}

func TestSignedRealComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			// Negative real with positive imaginary
			bs:    "-1+2i",
			scan:  "-1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Positive real with positive imaginary
			bs:    "+1+2i",
			scan:  "+1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Negative real with negative imaginary
			bs:    "-3-4i",
			scan:  "-3-4i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Positive real with negative imaginary
			bs:    "+5-6i",
			scan:  "+5-6i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed decimal real with imaginary
			bs:    "-1.5+2.5i",
			scan:  "-1.5+2.5i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed real with unit imaginary
			bs:    "-1+i",
			scan:  "-1+i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed real with negative unit imaginary
			bs:    "+5-i",
			scan:  "+5-i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			if tc.err0 == io.EOF {
				c.Check(err, qt.IsNil)
			} else {
				c.Check(err, qt.ErrorIs, tc.err0)
			}
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

// ============================================================================
// R7RS Compliance Bug Fix Tests
// ============================================================================

func TestEmptyExponentError(t *testing.T) {
	// Bug 2: Empty exponent should produce error, not be silently accepted
	// Uses read() directly to test error, matching existing test patterns
	tcs := []struct {
		name  string
		input string
	}{
		{"bare exponent", "1e"},
		{"exponent with plus", "1e+"},
		{"exponent with minus", "1e-"},
		{"decimal with exponent", "1.5e"},
		{"decimal with exponent plus", "1.5e+"},
		{"leading dot with exponent", ".5e"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Assert(p.err, qt.Not(qt.IsNil), qt.Commentf("expected error for input %q", tc.input))
			var tokErr *TokenizerError
			c.Assert(p.err, qt.ErrorAs, &tokErr)
		})
	}
}

func TestInvalidHashSequenceError(t *testing.T) {
	// Bug 3: Invalid # sequences should produce error, not panic
	// Uses read() directly to test error, matching existing test patterns
	tcs := []struct {
		name  string
		input string
	}{
		{"hash bracket", "#["},
		{"hash curly", "#{"},
		{"hash dollar", "#$"},
		{"hash percent", "#%"},
		{"hash caret", "#^"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Assert(p.err, qt.Not(qt.IsNil), qt.Commentf("expected error for input %q", tc.input))
			var tokErr *TokenizerError
			c.Assert(p.err, qt.ErrorAs, &tokErr)
		})
	}
}

func TestTrailingDotDecimals(t *testing.T) {
	// Bug 1: R7RS allows trailing dot decimals like "1."
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{"unsigned trailing dot", "1.", TokenizerStateUnsignedDecimalFraction},
		{"positive trailing dot", "+1.", TokenizerStateSignedDecimalFraction},
		{"negative trailing dot", "-1.", TokenizerStateSignedDecimalFraction},
		{"trailing dot with exponent", "1.e2", TokenizerStateUnsignedDecimalFraction},
		{"trailing dot followed by paren", "1.(", TokenizerStateUnsignedDecimalFraction},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil, qt.Commentf("unexpected error for input %q", tc.input))
			c.Assert(token.Type(), qt.Equals, tc.state)
		})
	}
}

// TestR7RSUnicodeIdentifiers tests R7RS Section 7.1.1 Unicode identifier support.
// R7RS specifies:
//   - <letter> includes Unicode categories Lu, Ll, Lt, Lm, Lo, and Nl
//   - <subsequent> additionally allows Nd, Mc, and Me categories
//
// These tests are expected to FAIL until Unicode category support is implemented.
func TestR7RSUnicodeIdentifiers(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
		value string // expected processed value
	}{
		// Category Nl (Number, Letter) - valid as initial per R7RS
		{
			name:  "Roman numeral XII as identifier",
			input: "Ⅻ", // U+216B ROMAN NUMERAL TWELVE (category Nl)
			state: TokenizerStateSymbol,
			value: "Ⅻ",
		},
		{
			name:  "Roman numeral as initial with letter subsequent",
			input: "Ⅻfoo", // Roman numeral followed by letters
			state: TokenizerStateSymbol,
			value: "Ⅻfoo",
		},
		// Category Nd (Number, Decimal Digit) - valid as subsequent per R7RS
		// Note: ASCII digits 0-9 already work; testing Unicode Nd
		{
			name:  "Arabic-Indic digit as subsequent",
			input: "foo٣", // U+0663 ARABIC-INDIC DIGIT THREE (category Nd)
			state: TokenizerStateSymbol,
			value: "foo٣",
		},
		{
			name:  "Devanagari digit as subsequent",
			input: "bar५", // U+096B DEVANAGARI DIGIT FIVE (category Nd)
			state: TokenizerStateSymbol,
			value: "bar५",
		},
		// Category Mc (Mark, Spacing Combining) - valid as subsequent per R7RS
		{
			name:  "Devanagari vowel sign as subsequent",
			input: "xा", // U+093E DEVANAGARI VOWEL SIGN AA (category Mc)
			state: TokenizerStateSymbol,
			value: "xा",
		},
		// Category Me (Mark, Enclosing) - valid as subsequent per R7RS
		{
			name:  "Combining enclosing circle as subsequent",
			input: "x⃝", // U+20DD COMBINING ENCLOSING CIRCLE (category Me)
			state: TokenizerStateSymbol,
			value: "x⃝",
		},
		// Combined test: Nl initial with Nd/Mc/Me subsequents
		{
			name:  "Complex Unicode identifier",
			input: "Ⅻ٣ा⃝", // Nl + Nd + Mc + Me
			state: TokenizerStateSymbol,
			value: "Ⅻ٣ा⃝",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil, qt.Commentf("unexpected error for input %q", tc.input))
			c.Assert(token.Type(), qt.Equals, tc.state, qt.Commentf("wrong token type for %q", tc.input))
			c.Assert(token.String(), qt.Equals, tc.value, qt.Commentf("wrong value for %q", tc.input))
		})
	}
}

// TestStringLineContinuation tests R7RS string line continuation escape sequences.
// R7RS Section 6.7 specifies that within strings:
//
//	\<intraline whitespace>*<line ending><intraline whitespace>*
//
// is replaced by nothing (allows splitting long strings across lines).
func TestStringLineContinuation(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		value string // expected processed value (escapes converted)
	}{
		{
			name:  "simple line continuation",
			input: "\"hello\\\nworld\"",
			value: "helloworld", // backslash-newline removed
		},
		{
			name:  "line continuation with trailing spaces before newline",
			input: "\"hello\\   \nworld\"",
			value: "helloworld", // backslash, spaces, newline all removed
		},
		{
			name:  "line continuation with leading spaces after newline",
			input: "\"hello\\\n   world\"",
			value: "helloworld", // backslash, newline, leading spaces removed
		},
		{
			name:  "line continuation with spaces on both sides",
			input: "\"hello\\  \n  world\"",
			value: "helloworld", // all continuation whitespace removed
		},
		{
			name:  "line continuation with CRLF",
			input: "\"hello\\\r\nworld\"",
			value: "helloworld", // works with CRLF line ending
		},
		{
			name:  "line continuation with CR only",
			input: "\"hello\\\rworld\"",
			value: "helloworld", // works with CR line ending
		},
		{
			name:  "line continuation with tabs",
			input: "\"hello\\\t\n\tworld\"",
			value: "helloworld", // tabs are intraline whitespace
		},
		{
			name:  "multiple line continuations",
			input: "\"one\\\ntwo\\\nthree\"",
			value: "onetwothree",
		},
		{
			name:  "line continuation preserves other content",
			input: "\"prefix\\\nsuffix more\"",
			value: "prefixsuffix more",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil, qt.Commentf("unexpected error for input %q", tc.input))
			c.Assert(token.Type(), qt.Equals, TokenizerStateString, qt.Commentf("expected string token for %q", tc.input))
			c.Assert(token.Value(), qt.Equals, tc.value, qt.Commentf("wrong processed value for %q", tc.input))
		})
	}
}

// TODO: Some test cases here are duplicated with TestTokenizer_readBaseNInteger.
// Since readBaseNInteger delegates to readUnsignedBaseNInteger, these tests
// should be refactored to avoid redundancy. Keep unsigned-specific cases here,
// move sign-handling cases to the signed test, and share common infrastructure.
func TestTokenizer_readUnsignedBaseNInteger(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		radix     int
		maxn      int
		wantVal   int64
		wantCount int
		wantErr   error
		remaining rune
	}{
		// Empty input / no digits
		{
			name:      "empty input",
			input:     "",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			wantErr:   io.EOF,
		},
		{
			name:      "non-digit first char",
			input:     "abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: 'a',
		},
		{
			name:      "whitespace only",
			input:     "   ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: ' ',
		},

		// Base 10 - basic cases (when input ends at EOF, function returns 0)
		{
			name:      "base10 single digit with trailing space",
			input:     "5 ",
			radix:     10,
			maxn:      0,
			wantVal:   5,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base10 single digit at EOF",
			input:     "5",
			radix:     10,
			maxn:      0,
			wantVal:   5,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "base10 multiple digits with trailing space",
			input:     "12345 ",
			radix:     10,
			maxn:      0,
			wantVal:   12345,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base10 multiple digits at EOF",
			input:     "12345",
			radix:     10,
			maxn:      0,
			wantVal:   12345,
			wantCount: 5,
			wantErr:   io.EOF,
		},
		{
			name:      "base10 leading zeros",
			input:     "00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base10 all zeros",
			input:     "0000 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base10 terminated by letter",
			input:     "123abc",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: 'a',
		},

		// Base 2 (binary)
		{
			name:      "base2 single digit 0",
			input:     "0 ",
			radix:     2,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base2 single digit 1",
			input:     "1 ",
			radix:     2,
			maxn:      0,
			wantVal:   1,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base2 binary pattern",
			input:     "10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   22,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base2 invalid digit 2 terminates",
			input:     "1012",
			radix:     2,
			maxn:      0,
			wantVal:   5,
			wantCount: 3,
			remaining: '2',
		},
		{
			name:      "base2 invalid digit 9 terminates",
			input:     "119",
			radix:     2,
			maxn:      0,
			wantVal:   3,
			wantCount: 2,
			remaining: '9',
		},

		// Base 8 (octal)
		{
			name:      "base8 single digit",
			input:     "7 ",
			radix:     8,
			maxn:      0,
			wantVal:   7,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base8 octal pattern",
			input:     "755 ",
			radix:     8,
			maxn:      0,
			wantVal:   493,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base8 invalid digit 8 terminates",
			input:     "128",
			radix:     8,
			maxn:      0,
			wantVal:   10,
			wantCount: 2,
			remaining: '8',
		},
		{
			name:      "base8 invalid digit 9 terminates",
			input:     "179",
			radix:     8,
			maxn:      0,
			wantVal:   15,
			wantCount: 2,
			remaining: '9',
		},

		// Base 16 (hex)
		{
			name:      "base16 digits only",
			input:     "123 ",
			radix:     16,
			maxn:      0,
			wantVal:   291,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 lowercase letters",
			input:     "abc ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 uppercase letters",
			input:     "ABC ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 mixed case",
			input:     "aBcDeF ",
			radix:     16,
			maxn:      0,
			wantVal:   11259375,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "base16 invalid letter g terminates",
			input:     "1fg",
			radix:     16,
			maxn:      0,
			wantVal:   31,
			wantCount: 2,
			remaining: 'g',
		},
		{
			name:      "base16 invalid letter G terminates",
			input:     "ABCG",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'G',
		},

		// maxn limiting (leaves remaining input, so no EOF)
		{
			name:      "maxn limits to 1 digit",
			input:     "12345",
			radix:     10,
			maxn:      1,
			wantVal:   1,
			wantCount: 1,
			remaining: '2',
		},
		{
			name:      "maxn limits to 3 digits",
			input:     "12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 3,
			remaining: '4',
		},
		{
			name:      "maxn exceeds available digits",
			input:     "12 ",
			radix:     10,
			maxn:      5,
			wantVal:   12,
			wantCount: 2,
			remaining: ' ',
		},
		{
			name:      "maxn exceeds available digits",
			input:     "12",
			radix:     10,
			maxn:      5,
			wantVal:   12,
			wantCount: 2,
			wantErr:   io.EOF,
		},
		{
			name:      "maxn zero means unlimited",
			input:     "1234567890 ",
			radix:     10,
			maxn:      0,
			wantVal:   1234567890,
			wantCount: 10,
			remaining: ' ',
		},
		{
			name:      "maxn negative means unlimited",
			input:     "12345 ",
			radix:     10,
			maxn:      -1,
			wantVal:   12345,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "maxn with base16",
			input:     "abcdef",
			radix:     16,
			maxn:      4,
			wantVal:   43981,
			wantCount: 4,
			remaining: 'e',
		},

		// Radix 0 acts like radix 10
		{
			name:      "radix 0 acts like 10",
			input:     "42 ",
			radix:     0,
			maxn:      0,
			wantVal:   42,
			wantCount: 2,
			remaining: ' ',
		},

		// Overflow cases (with trailing char to avoid EOF masking the overflow error)
		{
			name:      "max int64",
			input:     "9223372036854775807 ",
			radix:     10,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 19,
			remaining: ' ',
		},
		{
			name:      "overflow int64 at EOF",
			input:     "9223372036854775808",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 19,
			wantErr:   io.EOF,
		},
		{
			name:      "large overflow at EOF",
			input:     "99999999999999999999",
			radix:     10,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 20,
			wantErr:   io.EOF,
		},
		{
			name:      "hex max int64",
			input:     "7fffffffffffffff ",
			radix:     16,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 16,
			remaining: ' ',
		},
		{
			name:      "hex overflow at EOF",
			input:     "8000000000000000",
			radix:     16,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 16,
			wantErr:   io.EOF,
		},

		// Unicode / special characters
		{
			name:      "unicode after digits",
			input:     "123日本語",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '日',
		},

		// Overflow with trailing delimiter (tests parse error detection)
		// Note: strconv.ParseInt returns math.MaxInt64 on overflow, so value is non-zero
		{
			name:      "overflow int64 with trailing space",
			input:     "9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 19,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "large overflow with trailing space",
			input:     "99999999999999999999 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "hex overflow with trailing space",
			input:     "8000000000000000 ",
			radix:     16,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 16,
			wantErr:   &TokenizerError{},
		},

		// Binary boundary cases
		{
			name:      "binary max int64",
			input:     "111111111111111111111111111111111111111111111111111111111111111 ",
			radix:     2,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 63,
			remaining: ' ',
		},
		{
			name:      "binary overflow",
			input:     "1000000000000000000000000000000000000000000000000000000000000000 ",
			radix:     2,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 64,
			wantErr:   &TokenizerError{},
		},

		// Octal boundary cases
		{
			name:      "octal max int64",
			input:     "777777777777777777777 ",
			radix:     8,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 21,
			remaining: ' ',
		},
		{
			name:      "octal overflow",
			input:     "1000000000000000000000 ",
			radix:     8,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 22,
			wantErr:   &TokenizerError{},
		},

		// Zero handling
		{
			name:      "single zero base10",
			input:     "0 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "single zero base16",
			input:     "0 ",
			radix:     16,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},

		// Delimiter variations
		{
			name:      "terminated by paren",
			input:     "123)",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: ')',
		},
		{
			name:      "terminated by newline",
			input:     "123\n",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '\n',
		},
		{
			name:      "terminated by tab",
			input:     "123\t",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '\t',
		},

		// Edge cases for maxn
		{
			name:      "maxn exactly matches input length",
			input:     "12345",
			radix:     10,
			maxn:      5,
			wantVal:   12345,
			wantCount: 5,
			wantErr:   io.EOF,
		},
		{
			name:      "maxn is 1 with single digit",
			input:     "9",
			radix:     10,
			maxn:      1,
			wantVal:   9,
			wantCount: 1,
			wantErr:   io.EOF,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			val, count := tok.readUnsignedBaseNInteger(tc.radix, tc.maxn)

			c.Assert(count, qt.Equals, tc.wantCount, qt.Commentf("digit count mismatch"))
			c.Assert(val, qt.Equals, tc.wantVal, qt.Commentf("value mismatch"))

			switch tc.wantErr.(type) {
			case nil:
				c.Assert(tok.err, qt.IsNil, qt.Commentf("unexpected error: %v", tok.err))
			case *TokenizerError:
				var tokErr *TokenizerError
				c.Assert(tok.err, qt.ErrorAs, &tokErr, qt.Commentf("expected TokenizerError"))
			default:
				c.Assert(tok.err, qt.ErrorIs, tc.wantErr)
			}

			if tc.remaining != 0 {
				c.Assert(tok.curr(), qt.Equals, tc.remaining,
					qt.Commentf("remaining input mismatch"))
			}
		})
	}
}

// TODO: Some test cases here are duplicated with TestTokenizer_readUnsignedBaseNInteger.
// Since readBaseNInteger delegates to readUnsignedBaseNInteger, these tests
// should be refactored to avoid redundancy. Keep sign-handling cases here,
// and rely on the unsigned test for comprehensive digit/radix coverage.
func TestTokenizer_readBaseNInteger(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		radix     int
		maxn      int
		wantVal   int64
		wantCount int
		wantErr   error
		remaining rune
	}{
		// No sign - behaves like unsigned
		{
			name:      "no sign base10",
			input:     "123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "no sign base10 at EOF",
			input:     "123",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			wantErr:   io.EOF,
		},

		// Positive sign
		{
			name:      "positive sign base10",
			input:     "+123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "positive sign base10 at EOF",
			input:     "+123",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 4,
			wantErr:   io.EOF,
		},

		// Negative sign
		{
			name:      "negative sign base10",
			input:     "-123 ",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "negative sign base10 at EOF",
			input:     "-123",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			wantErr:   io.EOF,
		},
		{
			name:      "negative zero",
			input:     "-0 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 2,
			remaining: ' ',
		},

		// Sign only (no digits)
		{
			name:      "plus sign only at EOF",
			input:     "+",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "minus sign only at EOF",
			input:     "-",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "plus sign followed by non-digit",
			input:     "+abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: 'a',
		},
		{
			name:      "minus sign followed by non-digit",
			input:     "-xyz",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: 'x',
		},

		// Non-base character termination (base 10)
		{
			name:      "base10 terminated by A",
			input:     "10A",
			radix:     10,
			maxn:      0,
			wantVal:   10,
			wantCount: 2,
			remaining: 'A',
		},
		{
			name:      "base10 terminated by lowercase a",
			input:     "99a",
			radix:     10,
			maxn:      0,
			wantVal:   99,
			wantCount: 2,
			remaining: 'a',
		},
		{
			name:      "base10 signed terminated by letter",
			input:     "+10A",
			radix:     10,
			maxn:      0,
			wantVal:   10,
			wantCount: 3,
			remaining: 'A',
		},
		{
			name:      "base10 negative terminated by letter",
			input:     "-10A",
			radix:     10,
			maxn:      0,
			wantVal:   -10,
			wantCount: 3,
			remaining: 'A',
		},

		// Non-base character termination (base 2)
		{
			name:      "base2 terminated by 2",
			input:     "1012",
			radix:     2,
			maxn:      0,
			wantVal:   5,
			wantCount: 3,
			remaining: '2',
		},
		{
			name:      "base2 terminated by 9",
			input:     "1109",
			radix:     2,
			maxn:      0,
			wantVal:   6,
			wantCount: 3,
			remaining: '9',
		},
		{
			name:      "base2 signed terminated by invalid",
			input:     "+1102",
			radix:     2,
			maxn:      0,
			wantVal:   6,
			wantCount: 4,
			remaining: '2',
		},
		{
			name:      "base2 negative terminated by invalid",
			input:     "-1012",
			radix:     2,
			maxn:      0,
			wantVal:   -5,
			wantCount: 4,
			remaining: '2',
		},

		// Non-base character termination (base 8)
		{
			name:      "base8 terminated by 8",
			input:     "178",
			radix:     8,
			maxn:      0,
			wantVal:   15,
			wantCount: 2,
			remaining: '8',
		},
		{
			name:      "base8 terminated by 9",
			input:     "779",
			radix:     8,
			maxn:      0,
			wantVal:   63,
			wantCount: 2,
			remaining: '9',
		},
		{
			name:      "base8 terminated by A",
			input:     "77A",
			radix:     8,
			maxn:      0,
			wantVal:   63,
			wantCount: 2,
			remaining: 'A',
		},
		{
			name:      "base8 signed terminated by invalid",
			input:     "+128",
			radix:     8,
			maxn:      0,
			wantVal:   10,
			wantCount: 3,
			remaining: '8',
		},

		// Non-base character termination (base 16)
		{
			name:      "base16 terminated by G",
			input:     "ABCG",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'G',
		},
		{
			name:      "base16 terminated by lowercase g",
			input:     "abcg",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'g',
		},
		{
			name:      "base16 terminated by Z",
			input:     "FFZ",
			radix:     16,
			maxn:      0,
			wantVal:   255,
			wantCount: 2,
			remaining: 'Z',
		},
		{
			name:      "base16 signed terminated by invalid",
			input:     "+1FG",
			radix:     16,
			maxn:      0,
			wantVal:   31,
			wantCount: 3,
			remaining: 'G',
		},
		{
			name:      "base16 negative terminated by invalid",
			input:     "-FFG",
			radix:     16,
			maxn:      0,
			wantVal:   -255,
			wantCount: 3,
			remaining: 'G',
		},

		// Base 2 (binary) with signs
		{
			name:      "base2 positive",
			input:     "+10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   22,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "base2 negative",
			input:     "-10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   -22,
			wantCount: 6,
			remaining: ' ',
		},

		// Base 8 (octal) with signs
		{
			name:      "base8 positive",
			input:     "+755 ",
			radix:     8,
			maxn:      0,
			wantVal:   493,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base8 negative",
			input:     "-755 ",
			radix:     8,
			maxn:      0,
			wantVal:   -493,
			wantCount: 4,
			remaining: ' ',
		},

		// Base 16 (hex) with signs
		{
			name:      "base16 positive uppercase",
			input:     "+ABC ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base16 negative lowercase",
			input:     "-abc ",
			radix:     16,
			maxn:      0,
			wantVal:   -2748,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base16 mixed case",
			input:     "+aBcDeF ",
			radix:     16,
			maxn:      0,
			wantVal:   11259375,
			wantCount: 7,
			remaining: ' ',
		},

		// maxn with sign (maxn applies to digits only, not sign)
		{
			name:      "maxn with positive sign",
			input:     "+12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 4,
			remaining: '4',
		},
		{
			name:      "maxn with negative sign",
			input:     "-12345",
			radix:     10,
			maxn:      3,
			wantVal:   -123,
			wantCount: 4,
			remaining: '4',
		},
		{
			name:      "maxn without sign",
			input:     "12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 3,
			remaining: '4',
		},

		// Empty/whitespace input
		{
			name:      "empty input",
			input:     "",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			wantErr:   io.EOF,
		},
		{
			name:      "whitespace only",
			input:     "   ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: ' ',
		},
		{
			name:      "non-digit non-sign first",
			input:     "abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: 'a',
		},

		// Overflow cases
		{
			name:      "positive max int64",
			input:     "+9223372036854775807 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			remaining: ' ',
		},
		{
			name:      "negative overflow gives negated max",
			input:     "-9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   -9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "positive overflow gives max",
			input:     "+9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},

		// Delimiter variations
		{
			name:      "terminated by paren",
			input:     "-123)",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: ')',
		},
		{
			name:      "terminated by newline",
			input:     "+456\n",
			radix:     10,
			maxn:      0,
			wantVal:   456,
			wantCount: 4,
			remaining: '\n',
		},

		// Unicode termination
		{
			name:      "unicode after signed digits",
			input:     "-123日本語",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: '日',
		},

		// Radix 0 acts like radix 10
		{
			name:      "radix 0 with sign",
			input:     "-42 ",
			radix:     0,
			maxn:      0,
			wantVal:   -42,
			wantCount: 3,
			remaining: ' ',
		},

		// Leading zeros with sign
		{
			name:      "positive with leading zeros",
			input:     "+00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "negative with leading zeros",
			input:     "-00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 6,
			remaining: ' ',
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			val, count := tok.readBaseNInteger(tc.radix, tc.maxn)

			c.Assert(count, qt.Equals, tc.wantCount, qt.Commentf("digit count mismatch"))
			c.Assert(val, qt.Equals, tc.wantVal, qt.Commentf("value mismatch"))

			switch tc.wantErr.(type) {
			case nil:
				c.Assert(tok.err, qt.IsNil, qt.Commentf("unexpected error: %v", tok.err))
			case *TokenizerError:
				var tokErr *TokenizerError
				c.Assert(tok.err, qt.ErrorAs, &tokErr, qt.Commentf("expected TokenizerError"))
			default:
				c.Assert(tok.err, qt.ErrorIs, tc.wantErr)
			}

			if tc.remaining != 0 {
				c.Assert(tok.curr(), qt.Equals, tc.remaining,
					qt.Commentf("remaining input mismatch"))
			}
		})
	}
}

// TestIsCommentToken tests the isCommentToken function
func TestIsCommentToken(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "; comment\n123",
			expectedType: TokenizerStateLineCommentBody,
		},
		{
			input:        "#| comment |#456",
			expectedType: TokenizerStateBlockCommentBody,
		},
		{
			input:        "#;(datum)",
			expectedType: TokenizerStateDatumCommentBegin,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizerWithComments(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestMayReadExponentEdgeCases tests exponent edge cases
func TestMayReadExponentEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "1.0E10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.0e+5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.0e-5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.5E10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "3/4",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        "22/7",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        "1/2",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        ".5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        ".125",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}
