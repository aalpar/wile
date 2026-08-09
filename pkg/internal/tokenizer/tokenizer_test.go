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

package tokenizer

import (
	"fmt"
	"io"
	"strings"
	"testing"
	"unicode/utf8"

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
		// R7RS conformance: signed integers with exponents (scientific notation)
		{
			in:     "+1e10",
			src:    []string{"+1e10"},
			tokens: []TokenizerState{TokenizerStateSignedScientificNotation},
			err:    io.EOF,
		},
		{
			in:     "-1e10",
			src:    []string{"-1e10"},
			tokens: []TokenizerState{TokenizerStateSignedScientificNotation},
			err:    io.EOF,
		},
		{
			in:     "+1E10",
			src:    []string{"+1E10"},
			tokens: []TokenizerState{TokenizerStateSignedScientificNotation},
			err:    io.EOF,
		},
		{
			in:     "-1e+10",
			src:    []string{"-1e+10"},
			tokens: []TokenizerState{TokenizerStateSignedScientificNotation},
			err:    io.EOF,
		},
		{
			in:     "+1e-10",
			src:    []string{"+1e-10"},
			tokens: []TokenizerState{TokenizerStateSignedScientificNotation},
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
			tokens: []TokenizerState{TokenizerStateMarkerBase16, TokenizerStateUnsignedInteger},
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
			tokens: []TokenizerState{TokenizerStateMarkerBase16, TokenizerStateUnsignedInteger},
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
			state: TokenizerStateUnsignedScientificNotation,
		},
		{
			bs:    "-34e10",
			scan:  "-34e10",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
		},
		{
			bs:    "+34e10",
			scan:  "+34e10",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
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
			state: TokenizerStateUnsignedScientificNotation,
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
			// Was: scan "-nan" with MessageExpectingDecimalFraction. A keyword
			// that is not followed by ".0" is not a number, and the scanner no
			// longer leaves the diagnostic behind when it says so.
			bs:    "-nan,0",
			scan:  "-nan",
			state: TokenizerStateSymbol,
		},
		{
			// Was: scan "-i" as TokenizerStateSignedImaginary, splitting the
			// identifier into a number and `foo`. R7RS §7.1.1 exempts -i, not
			// -i<subsequent>*.
			bs:    "-ifoo",
			scan:  "-ifoo",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
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
		// The same implicit-termination rule as -ifoo above, on the <infnan>
		// spellings: these used to scan "+inf.0i" and leave "zz" behind as a
		// second datum. Both are single identifiers.
		{
			bs:    "+inf.0izz",
			scan:  "+inf.0izz",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
		},
		{
			bs:    "+nan.0izz",
			scan:  "+nan.0izz",
			err0:  io.EOF,
			state: TokenizerStateSymbol,
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
		// Exponents edge cases (scientific notation - parser determines int vs float)
		{
			bs:    "1e0",
			scan:  "1e0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedScientificNotation,
		},
		{
			bs:    "1e-0",
			scan:  "1e-0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedScientificNotation,
		},
		{
			bs:    "1e+0",
			scan:  "1e+0",
			err0:  io.EOF,
			state: TokenizerStateUnsignedScientificNotation,
		},
		{
			bs:    "-1e-0",
			scan:  "-1e-0",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
		},
		{
			bs:    "-1e+0",
			scan:  "-1e+0",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
		},
		{
			bs:    "+1e-0",
			scan:  "+1e-0",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
		},
		{
			bs:    "+1e+0",
			scan:  "+1e+0",
			err0:  io.EOF,
			state: TokenizerStateSignedScientificNotation,
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
			p.Text()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.scan)
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
	var nilTok *SimpleToken
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
	err1 := NewTokenizerError("test error")
	err2 := NewTokenizerError("another error")

	c.Assert(err1.Is(err2), qt.IsFalse)                           // different messages
	c.Assert(err1.Is(NewTokenizerError("test error")), qt.IsTrue) // same message
	c.Assert(err1.Is(io.EOF), qt.IsFalse)                         // not a TokenizerError
}

func TestTokenizerError_Unwrap(t *testing.T) {
	c := qt.New(t)

	// Error without wrap
	err1 := NewTokenizerError("test")
	c.Assert(err1.Unwrap(), qt.IsNil)

	// Error with wrap
	wrapped := io.EOF
	err2 := NewTokenizerErrorWithWrap(wrapped, "wrapped")
	c.Assert(err2.Unwrap(), qt.Equals, wrapped)
}

func TestTokenizerError_Error(t *testing.T) {
	c := qt.New(t)
	err := NewTokenizerError("my error message")
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
		{"#b101", []TokenizerState{TokenizerStateMarkerBase2, TokenizerStateUnsignedInteger}},
		{"#o777", []TokenizerState{TokenizerStateMarkerBase8, TokenizerStateUnsignedInteger}},
		{"#d123", []TokenizerState{TokenizerStateMarkerBase10, TokenizerStateUnsignedInteger}},
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

// --- Token Value method tests ---

// TestTokenValue tests the wrt() method on SimpleToken
func TestTokenValue(t *testing.T) {
	tcs := []struct {
		input        string
		expectedVal  string
		expectedType TokenizerState
	}{
		{
			input:        `"hello"`,
			expectedVal:  "hello",
			expectedType: TokenizerStateString,
		},
		{
			input:        `""`,
			expectedVal:  "",
			expectedType: TokenizerStateString,
		},
		{
			input:        `"test\nstring"`,
			expectedVal:  "test\nstring",
			expectedType: TokenizerStateString,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.Value(), qt.Equals, tc.expectedVal)
		})
	}
}

// TestEqualTo tests the EqualTo method on tokens
func TestEqualTo(t *testing.T) {
	tok1 := NewTokenizer(strings.NewReader("123"), false)
	token1, err1 := tok1.Next()
	qt.Check(t, err1, qt.IsNil)
	st1, ok1 := token1.(*SimpleToken)
	qt.Check(t, ok1, qt.IsTrue)

	tok2 := NewTokenizer(strings.NewReader("123"), false)
	token2, err2 := tok2.Next()
	qt.Check(t, err2, qt.IsNil)
	st2, ok2 := token2.(*SimpleToken)
	qt.Check(t, ok2, qt.IsTrue)

	tok3 := NewTokenizer(strings.NewReader("456"), false)
	token3, err3 := tok3.Next()
	qt.Check(t, err3, qt.IsNil)
	st3, ok3 := token3.(*SimpleToken)
	qt.Check(t, ok3, qt.IsTrue)

	// Test that equal tokens are equal
	qt.Check(t, st1.EqualTo(st2), qt.IsTrue)

	// Test that different tokens are not equal
	qt.Check(t, st1.EqualTo(st3), qt.IsFalse)

	// Test equality with different types
	tok4 := NewTokenizer(strings.NewReader("abc"), false)
	token4, err4 := tok4.Next()
	qt.Check(t, err4, qt.IsNil)
	st4, ok4 := token4.(*SimpleToken)
	qt.Check(t, ok4, qt.IsTrue)
	qt.Check(t, st1.EqualTo(st4), qt.IsFalse)
}

// TestNewTokenizerEdgeCases tests edge cases in NewTokenizer
func TestNewTokenizerEdgeCases(t *testing.T) {
	// Test case insensitive mode
	tcs := []struct {
		input        string
		caseInsens   bool
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "ABC",
			caseInsens:   false,
			expectedType: TokenizerStateSymbol,
			expectedStr:  "ABC",
		},
		{
			input:        "abc",
			caseInsens:   true,
			expectedType: TokenizerStateSymbol,
			expectedStr:  "abc",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q ci=%v", i, tc.input, tc.caseInsens), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), tc.caseInsens)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestClose tests the Close method
func TestClose(t *testing.T) {
	tok := NewTokenizer(strings.NewReader("123"), false)

	// Read token first
	_, err := tok.Next()
	qt.Check(t, err, qt.IsNil)

	// Now close
	err = tok.Close()
	qt.Check(t, err, qt.IsNil)
}

// TestScanLineEnding tests scanLineEnding coverage
func TestScanLineEnding(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{input: "\n"},
		{input: "\r"},
		{input: "\r\n"},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			// Read a symbol followed by the line ending
			fullInput := "x" + tc.input + "y"
			tok := NewTokenizer(strings.NewReader(fullInput), false)

			// First token: x
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.String(), qt.Equals, "x")

			// Second token: y (line ending was consumed)
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.String(), qt.Equals, "y")
		})
	}
}

// Test Token.Value() branches
func TestToken_Value_StringEnd(t *testing.T) {
	// Test empty string - should return val even when empty
	p := NewTokenizer(strings.NewReader(`""`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
	qt.Assert(t, tok.Value(), qt.Equals, "")
}

func TestToken_Value_WithVal(t *testing.T) {
	// Test string with escape sequences - val should contain processed value
	p := NewTokenizer(strings.NewReader(`"hello\nworld"`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
	// val should have actual newline, src has raw escape
	qt.Assert(t, tok.Value(), qt.Contains, "\n")
}

func TestToken_Value_NoVal(t *testing.T) {
	// Test symbol - should return src when val is empty
	p := NewTokenizer(strings.NewReader(`hello`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateSymbol)
	qt.Assert(t, tok.Value(), qt.Equals, "hello")
}

// Test line ending variations
func TestTokenizer_LineEndingVariations(t *testing.T) {
	// CRLF line ending
	p := NewTokenizer(strings.NewReader("; comment\r\nfoo"), false)
	tok, _ := p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// CR only line ending
	p = NewTokenizer(strings.NewReader("; comment\rfoo"), false)
	tok, _ = p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateLineCommentBody)
}

// TestWhitespaceHandling tests various whitespace scenarios
func TestWhitespaceHandling(t *testing.T) {
	tcs := []struct {
		name   string
		input  string
		tokens int
	}{
		{name: "spaces_between_tokens", input: "1 2 3", tokens: 3},
		{name: "tabs_between_tokens", input: "1\t2\t3", tokens: 3},
		{name: "newlines_between_tokens", input: "1\n2\n3", tokens: 3},
		{name: "mixed_whitespace", input: "1 \t\n 2", tokens: 2},
		{name: "leading_whitespace", input: "  123", tokens: 1},
		{name: "trailing_whitespace", input: "123  ", tokens: 1},
		{name: "only_whitespace", input: "   \t\n  ", tokens: 0},
		{name: "crlf_line_endings", input: "1\r\n2", tokens: 2},
		{name: "cr_only_line_endings", input: "1\r2", tokens: 2},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			count := 0
			for {
				_, err := p.Next()
				if err == io.EOF {
					break
				}
				if err != nil {
					c.Fatalf("unexpected error: %v", err)
				}
				count++
			}
			c.Check(count, qt.Equals, tc.tokens)
		})
	}
}

// TestEmptyInput tests behavior with empty input
func TestEmptyInput(t *testing.T) {
	c := qt.New(t)
	p := NewTokenizer(strings.NewReader(""), false)
	_, err := p.Next()
	c.Check(err, qt.Equals, io.EOF)
}

// TestVeryLongTokens tests handling of very long tokens
func TestVeryLongTokens(t *testing.T) {
	t.Run("long_symbol", func(t *testing.T) {
		c := qt.New(t)
		longSym := strings.Repeat("a", 10000)
		p := NewTokenizer(strings.NewReader(longSym), false)
		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(len(tok.(*SimpleToken).src), qt.Equals, 10000)
	})

	t.Run("long_number", func(t *testing.T) {
		c := qt.New(t)
		longNum := strings.Repeat("1", 1000)
		p := NewTokenizer(strings.NewReader(longNum), false)
		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(len(tok.(*SimpleToken).src), qt.Equals, 1000)
	})

	t.Run("long_string", func(t *testing.T) {
		c := qt.New(t)
		longStr := `"` + strings.Repeat("a", 10000) + `"`
		p := NewTokenizer(strings.NewReader(longStr), false)
		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.Type(), qt.Equals, TokenizerStateString)
	})
}

// TestEOFDuringToken tests behavior when EOF occurs mid-token
func TestEOFDuringToken(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
		err   error
	}{
		{
			name:  "eof_after_hash",
			input: "#",
			state: TokenizerStateFailed,
			err:   io.EOF,
		},
		{
			name:  "eof_after_hash_backslash",
			input: `#\`,
			state: TokenizerStateCharMnemonicOrHexEscape,
			err:   io.EOF,
		},
		{
			name:  "eof_after_sign",
			input: "+",
			state: TokenizerStateSymbol,
			err:   io.EOF,
		},
		{
			name:  "eof_after_dot",
			input: ".",
			state: TokenizerStateCons,
			err:   io.EOF,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.err, qt.ErrorIs, tc.err)
		})
	}
}

// TestPeculiarIdentifierSpace drives every sign-prefixed spelling the number
// scanner speculates on, three ways: at end of input, before a delimiter, and
// before a non-delimiter. End of input and a delimiter must agree — the scanner
// used to accept a truncated <infnan> keyword only at end of input, so `+in`
// read as +inf.0 there and errored everywhere else.
//
// A non-delimiter always extends the run into one identifier, for every row:
// R7RS §7.1.1 exempts only +i, -i and <infnan> from <peculiar identifier>, so
// `+i2`, `+ifoo` and `+inf.0x` are single identifiers rather than a number
// followed by a second datum.
//
// Token.Value() is asserted alongside the state: the arms that fall back to a
// symbol used to mint it from the scanner's position rather than the token's
// start, so `+.abc` was the symbol `bc` and `+nabc` was the empty symbol —
// which made distinct identifiers eq?.
func TestPeculiarIdentifierSpace(t *testing.T) {
	tcs := []struct {
		src string
		// state and value are the first token's type and Token.Value() both at
		// end of input and when a delimiter follows. value is "" for numbers,
		// which never populate the value buffer.
		state TokenizerState
		value string
	}{
		// The ten truncated <infnan> spellings.
		{src: "+in", state: TokenizerStateSymbol, value: "+in"},
		{src: "+n", state: TokenizerStateSymbol, value: "+n"},
		{src: "-n", state: TokenizerStateSymbol, value: "-n"},
		{src: "-na", state: TokenizerStateSymbol, value: "-na"},
		{src: "-nan", state: TokenizerStateSymbol, value: "-nan"},
		{src: "+na", state: TokenizerStateSymbol, value: "+na"},
		{src: "+nan", state: TokenizerStateSymbol, value: "+nan"},
		{src: "-in", state: TokenizerStateSymbol, value: "-in"},
		{src: "-inf", state: TokenizerStateSymbol, value: "-inf"},
		{src: "+inf", state: TokenizerStateSymbol, value: "+inf"},
		// Sign-dot and prefix-mismatch identifiers.
		{src: "+.abc", state: TokenizerStateSymbol, value: "+.abc"},
		{src: "-.f", state: TokenizerStateSymbol, value: "-.f"},
		{src: "+nabc", state: TokenizerStateSymbol, value: "+nabc"},
		{src: "+node", state: TokenizerStateSymbol, value: "+node"},
		{src: "+nan_x", state: TokenizerStateSymbol, value: "+nan_x"},
		// Unit-imaginary prefixes.
		{src: "+i2", state: TokenizerStateSymbol, value: "+i2"},
		{src: "-ibar", state: TokenizerStateSymbol, value: "-ibar"},
		{src: "+ifoo", state: TokenizerStateSymbol, value: "+ifoo"},
		// Bare signs.
		{src: "+", state: TokenizerStateSymbol, value: "+"},
		{src: "-", state: TokenizerStateSymbol, value: "-"},
		// The numbers the exemption does cover; these must stay numbers.
		{src: "+i", state: TokenizerStateSignedImaginary},
		{src: "-i", state: TokenizerStateSignedImaginary},
		{src: "+inf.0", state: TokenizerStateSignedInf},
		{src: "-inf.0", state: TokenizerStateSignedInf},
		{src: "+nan.0", state: TokenizerStateSignedNan},
		{src: "-nan.0", state: TokenizerStateSignedNan},
		{src: "+inf.0i", state: TokenizerStateSignedImaginaryInf},
		{src: "+nan.0i", state: TokenizerStateSignedImaginaryNan},
	}
	for _, tc := range tcs {
		checkFirstToken := func(c *qt.C, src string, state TokenizerState, text, value string, err error) {
			p := NewTokenizer(strings.NewReader(src), false)
			tok, nexterr := p.Next()
			c.Assert(nexterr, qt.IsNil)
			c.Check(tok.Type(), qt.Equals, state)
			c.Check(tok.String(), qt.Equals, text)
			c.Check(tok.Value(), qt.Equals, value)
			c.Check(p.Err(), qt.ErrorIs, err)
		}
		t.Run(tc.src+"/eof", func(t *testing.T) {
			c := qt.New(t)
			checkFirstToken(c, tc.src, tc.state, tc.src, tc.value, io.EOF)
		})
		t.Run(tc.src+"/delimiter", func(t *testing.T) {
			c := qt.New(t)
			checkFirstToken(c, tc.src+")", tc.state, tc.src, tc.value, nil)
		})
		t.Run(tc.src+"/nondelimiter", func(t *testing.T) {
			c := qt.New(t)
			ext := tc.src + "x"
			checkFirstToken(c, ext, TokenizerStateSymbol, ext, ext, io.EOF)
		})
	}
}

// tokenizerTestCase is the common struct for table-driven tokenizer tests.
// Used across multiple test files in this package.
// tokenizerTestCase is the common struct for table-driven tokenizer tests
// that check if an input string tokenizes to a specific state.
type tokenizerTestCase struct {
	in    string
	state TokenizerState
}
