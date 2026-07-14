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
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestBytevectorLiteral(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#u8(1 2 3)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateOpenVectorUnsignedByteMarker)
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

func TestVector(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#(1 2 3)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateOpenVector)
}

// TestTokenizer_Brackets tests R7RS §2.1 square bracket support.
// Square brackets [ and ] are equivalent to ( and ) but must match.
func TestTokenizer_Brackets(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		in     string
		tokens []TokenizerState
		src    []string
	}{
		{
			name:   "open bracket",
			in:     "[",
			tokens: []TokenizerState{TokenizerStateOpenBracket},
			src:    []string{"["},
		},
		{
			name:   "close bracket",
			in:     "]",
			tokens: []TokenizerState{TokenizerStateCloseBracket},
			src:    []string{"]"},
		},
		{
			name:   "empty bracket list",
			in:     "[]",
			tokens: []TokenizerState{TokenizerStateEmptyList},
			src:    []string{"[]"},
		},
		{
			name:   "bracket list with elements",
			in:     "[1 2 3]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateUnsignedInteger, TokenizerStateUnsignedInteger, TokenizerStateUnsignedInteger, TokenizerStateCloseBracket},
			src:    []string{"[", "1", "2", "3", "]"},
		},
		{
			name:   "bracket improper list",
			in:     "[a . b]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCons, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"[", "a", ".", "b", "]"},
		},
		{
			name:   "nested brackets",
			in:     "[[a] [b]]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateCloseBracket},
			src:    []string{"[", "[", "a", "]", "[", "b", "]", "]"},
		},
		{
			name:   "mixed parens and brackets",
			in:     "([a] (b))",
			tokens: []TokenizerState{TokenizerStateOpenParen, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateCloseParen, TokenizerStateCloseParen},
			src:    []string{"(", "[", "a", "]", "(", "b", ")", ")"},
		},
		{
			name:   "quote with bracket",
			in:     "'[a b]",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"'", "[", "a", "b", "]"},
		},
		{
			name:   "quasiquote with bracket",
			in:     "`[,a ,@b]",
			tokens: []TokenizerState{TokenizerStateQuasiquote, TokenizerStateOpenBracket, TokenizerStateUnquote, TokenizerStateSymbol, TokenizerStateUnquoteSplicing, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"`", "[", ",", "a", ",@", "b", "]"},
		},
		{
			name:   "nested empty list",
			in:     "[[]]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateEmptyList, TokenizerStateCloseBracket},
			src:    []string{"[", "[]", "]"},
		},
		{
			name:   "bracket with paren inside",
			in:     "[(a)]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateCloseParen, TokenizerStateCloseBracket},
			src:    []string{"[", "(", "a", ")", "]"},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			var tokens []TokenizerState
			var srcs []string
			for {
				tok, err := p.Next()
				if err != nil {
					c.Assert(err, qt.ErrorIs, io.EOF)
					break
				}
				tokens = append(tokens, tok.Type())
				srcs = append(srcs, tok.String())
			}
			c.Assert(tokens, qt.DeepEquals, tc.tokens)
			c.Assert(srcs, qt.DeepEquals, tc.src)
		})
	}
}

// TestConsDot tests the cons dot tokenization
func TestConsDot(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        ". ",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
		},
		{
			input:        ".)",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
		},
		{
			input:        ".\n",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestDirectives tests directive tokenization
func TestDirectives(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "#!fold-case",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!fold-case",
		},
		{
			input:        "#!no-fold-case",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!no-fold-case",
		},
		{
			input:        "#!r7rs",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!r7rs",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestVectorMarker tests vector marker tokenization
func TestVectorMarker(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "#(",
			expectedType: TokenizerStateOpenVector,
			expectedStr:  "#(",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestTypedArrayVariations tests typed array marker variations
func TestTypedArrayVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#u8(", expectedType: TokenizerStateOpenVectorUnsignedByteMarker},
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

// TestBooleanVariations tests boolean constant variations
func TestBooleanVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#t", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#T", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#true", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#True", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#TRUE", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#f", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#F", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#false", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#False", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#FALSE", expectedType: TokenizerStateMarkerBooleanFalse},
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

// TestQuotingAndQuasiquoting tests quote-like tokens
func TestQuotingAndQuasiquoting(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "'", expectedType: TokenizerStateQuote},
		{input: "`", expectedType: TokenizerStateQuasiquote},
		{input: ",", expectedType: TokenizerStateUnquote},
		{input: ",@", expectedType: TokenizerStateUnquoteSplicing},
		{input: "#'", expectedType: TokenizerStateSyntax},
		{input: "#`", expectedType: TokenizerStateQuasisyntax},
		{input: "#,", expectedType: TokenizerStateUnsyntax},
		{input: "#,@", expectedType: TokenizerStateUnsyntaxSplicing},
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

// Test cons dot (improper list notation)
func TestTokenizer_ConsDot(t *testing.T) {
	tests := []tokenizerTestCase{
		{"(a . b)", TokenizerStateCons},
		{"(1 . 2)", TokenizerStateCons},
		{"(+ . args)", TokenizerStateCons},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			// Skip opening paren
			_, _ = p.Next()
			// Skip first element
			_, _ = p.Next()
			// Get the dot
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test dot followed by symbol subsequent (peculiar identifiers)
func TestTokenizer_DotSubsequent(t *testing.T) {
	tests := []tokenizerTestCase{
		{"+..", TokenizerStateSymbol},
		{"+.@", TokenizerStateSymbol},
		{"-..", TokenizerStateSymbol},
		{"+.+", TokenizerStateSymbol},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test typed arrays (u8 vectors) - R7RS §7.1.1: case-insensitive
func TestTokenizer_TypedArrays(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#u8(1 2 3)", TokenizerStateOpenVectorUnsignedByteMarker},
		{"#U8(1 2 3)", TokenizerStateOpenVectorUnsignedByteMarker}, // uppercase
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test more typed array and radix branches
func TestTokenizer_TypedArrayRadixBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Case insensitive booleans
		{"#T", TokenizerStateMarkerBooleanTrue},
		{"#TRUE", TokenizerStateMarkerBooleanTrue},
		{"#F", TokenizerStateMarkerBooleanFalse},
		{"#FALSE", TokenizerStateMarkerBooleanFalse},
		// After partial match, continue as marker
		{"#tfoo", TokenizerStateMarker},
		{"#ffoo", TokenizerStateMarker},
		// # followed by unknown letter
		{"#foo", TokenizerStateMarker},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test label references and assignments
func TestTokenizer_Labels(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#123=", TokenizerStateLabelAssignment},
		{"#123#", TokenizerStateLabelReference},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test directives
func TestTokenizer_Directives(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#!fold-case", TokenizerStateDirective},
		{"#!no-fold-case", TokenizerStateDirective},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test vectors
func TestTokenizer_Vectors(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#(", TokenizerStateOpenVector},
		{"#(1 2 3)", TokenizerStateOpenVector},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test syntax quotation tokens
func TestTokenizer_SyntaxQuotation(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#'x", TokenizerStateSyntax},
		{"#`x", TokenizerStateQuasisyntax},
		{"#,x", TokenizerStateUnsyntax},
		{"#,@x", TokenizerStateUnsyntaxSplicing},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// TestCombinedPrefixTokenSequence tests that combined prefixes produce correct token sequences
// The tokenizer preserves radix state between marker and number tokens.
func TestCombinedPrefixTokenSequence(t *testing.T) {
	t.Run("exactness_then_radix", func(t *testing.T) {
		// #e#b101 should produce: #e (exact marker), #b (binary marker), 101 (unsigned integer)
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("#e#b101"), false)

		tok1, err1 := p.Next()
		c.Assert(err1, qt.IsNil)
		c.Check(tok1.Type(), qt.Equals, TokenizerStateMarkerNumberExact)
		c.Check(tok1.(*SimpleToken).src, qt.Equals, "#e")

		tok2, err2 := p.Next()
		c.Assert(err2, qt.IsNil)
		c.Check(tok2.Type(), qt.Equals, TokenizerStateMarkerBase2)
		c.Check(tok2.(*SimpleToken).src, qt.Equals, "#b")

		tok3, err3 := p.Next()
		c.Assert(err3, qt.IsNil)
		c.Check(tok3.Type(), qt.Equals, TokenizerStateUnsignedInteger)
		c.Check(tok3.(*SimpleToken).src, qt.Equals, "101")
	})

	t.Run("radix_then_exactness_with_hex_digits", func(t *testing.T) {
		// #x#i1F should produce: #x (hex marker), #i (inexact marker), 1F (integer with hex digits)
		// Radix 16 persists through the #i marker to the number
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("#x#i1F"), false)

		tok1, err1 := p.Next()
		c.Assert(err1, qt.IsNil)
		c.Check(tok1.Type(), qt.Equals, TokenizerStateMarkerBase16)
		c.Check(tok1.(*SimpleToken).src, qt.Equals, "#x")

		tok2, err2 := p.Next()
		c.Assert(err2, qt.IsNil)
		c.Check(tok2.Type(), qt.Equals, TokenizerStateMarkerNumberInexact)
		c.Check(tok2.(*SimpleToken).src, qt.Equals, "#i")

		tok3, err3 := p.Next()
		c.Assert(err3, qt.IsNil)
		c.Check(tok3.Type(), qt.Equals, TokenizerStateUnsignedInteger)
		c.Check(tok3.(*SimpleToken).src, qt.Equals, "1F") // Hex digits included!
	})

	t.Run("hex_number_includes_af_digits", func(t *testing.T) {
		// #x1000ff should tokenize hex digits a-f correctly
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("#x1000ff"), false)

		tok1, err1 := p.Next()
		c.Assert(err1, qt.IsNil)
		c.Check(tok1.Type(), qt.Equals, TokenizerStateMarkerBase16)

		tok2, err2 := p.Next()
		c.Assert(err2, qt.IsNil)
		c.Check(tok2.Type(), qt.Equals, TokenizerStateUnsignedInteger)
		c.Check(tok2.(*SimpleToken).src, qt.Equals, "1000ff") // All hex digits included
	})

	t.Run("decimal_stops_at_hex_letters", func(t *testing.T) {
		// #d1000abc should stop at non-decimal digits
		// Note: we use 'a', 'b', 'c' since 'e', 'f', 's', 'd', 'l' are exponent markers per R7RS
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("#d1000abc"), false)

		tok1, err1 := p.Next()
		c.Assert(err1, qt.IsNil)
		c.Check(tok1.Type(), qt.Equals, TokenizerStateMarkerBase10)

		tok2, err2 := p.Next()
		c.Assert(err2, qt.IsNil)
		c.Check(tok2.Type(), qt.Equals, TokenizerStateUnsignedInteger)
		c.Check(tok2.(*SimpleToken).src, qt.Equals, "1000") // Stops at 'a'

		tok3, err3 := p.Next()
		c.Assert(err3, qt.IsNil)
		c.Check(tok3.Type(), qt.Equals, TokenizerStateSymbol)
		c.Check(tok3.(*SimpleToken).src, qt.Equals, "abc") // Remainder is symbol
	})

	t.Run("radix_resets_after_number", func(t *testing.T) {
		// After parsing a hex number, radix should reset to 10
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("#xff abc"), false)

		tok1, _ := p.Next() // #x
		c.Check(tok1.Type(), qt.Equals, TokenizerStateMarkerBase16)

		tok2, _ := p.Next() // ff (parsed as hex)
		c.Check(tok2.(*SimpleToken).src, qt.Equals, "ff")

		tok3, _ := p.Next() // abc (should be symbol, not hex)
		c.Check(tok3.Type(), qt.Equals, TokenizerStateSymbol)
		c.Check(tok3.(*SimpleToken).src, qt.Equals, "abc")
	})
}

// TestDelimiterBoundaries tests that tokens stop correctly at delimiters
func TestDelimiterBoundaries(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string
	}{
		// Numbers at delimiters
		{input: "123(", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123)", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123;", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123\"", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123|", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123 ", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123\t", state: TokenizerStateUnsignedInteger, span: "123"},
		{input: "123\n", state: TokenizerStateUnsignedInteger, span: "123"},

		// Symbols at delimiters
		{input: "foo(", state: TokenizerStateSymbol, span: "foo"},
		{input: "foo)", state: TokenizerStateSymbol, span: "foo"},
		{input: "foo;", state: TokenizerStateSymbol, span: "foo"},
		{input: "foo\"", state: TokenizerStateSymbol, span: "foo"},
		{input: "foo|", state: TokenizerStateSymbol, span: "foo"},

		// Complex numbers at delimiters
		{input: "1+2i(", state: TokenizerStateUnsignedComplex, span: "1+2i"},
		{input: "1+2i)", state: TokenizerStateUnsignedComplex, span: "1+2i"},

		// Decimal fractions at delimiters
		{input: "1.5(", state: TokenizerStateUnsignedDecimalFraction, span: "1.5"},
		{input: "1.5)", state: TokenizerStateUnsignedDecimalFraction, span: "1.5"},
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d: %q", i, tc.input), func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.span)
		})
	}
}

// TestMalformedHashUIsReadError pins that #u introduces nothing but a #u8( byte
// vector (R7RS §7.1.1). The scanner used to fall out of the #u arm with neither
// a state nor an error, so `#u9` emitted a token carrying the PREVIOUS token's
// type and `'(a #u9)` evaluated as though the #u were not there.
func TestMalformedHashUIsReadError(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{name: "wrong_digit", input: "(a #u9)"},
		{name: "no_paren", input: "#u8x"},
		{name: "bare_u", input: "(a #u)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			_, err := Tokenize(tc.input, false)
			c.Check(errors.Is(err, io.EOF), qt.IsFalse, qt.Commentf("malformed %q must not tokenize cleanly", tc.input))
		})
	}
}

// TestTokenStateDoesNotLeakAcrossTokens pins that a token never inherits the
// previous token's type. mark() resets every per-token field, state included: a
// scanner path that bails out before assigning one (a lexical error inside
// readBoolean, the old #u fall-through) would otherwise mis-type the token.
func TestTokenStateDoesNotLeakAcrossTokens(t *testing.T) {
	c := qt.New(t)

	// "a" scans as a Symbol; the malformed #u that follows must not be emitted
	// as a Symbol too.
	toks, err := Tokenize("a #u9", false)
	c.Assert(errors.Is(err, io.EOF), qt.IsFalse)
	for _, tok := range toks[1:] {
		c.Check(tok.Type(), qt.Not(qt.Equals), TokenizerStateSymbol)
	}
}

// TestBooleanAbuttingBracketIsBoolean pins [ and ] as delimiters (R7RS §2.1
// makes them equivalent to parens). Only the #-boolean scanners consult
// isDelimiter, so `#t]` was the visible casualty: it scanned as a bare marker,
// and `(let ([x #t]) ...)` would not parse.
func TestBooleanAbuttingBracketIsBoolean(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		want  TokenizerState
	}{
		{name: "true_close_bracket", input: "#t]", want: TokenizerStateMarkerBooleanTrue},
		{name: "false_close_bracket", input: "#f]", want: TokenizerStateMarkerBooleanFalse},
		{name: "true_open_bracket", input: "#t[", want: TokenizerStateMarkerBooleanTrue},
		{name: "long_true_close_bracket", input: "#true]", want: TokenizerStateMarkerBooleanTrue},
		{name: "true_close_paren", input: "#t)", want: TokenizerStateMarkerBooleanTrue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			toks, err := Tokenize(tc.input, false)
			c.Assert(err, qt.Equals, io.EOF)
			c.Assert(len(toks), qt.Equals, 2)
			c.Check(toks[0].Type(), qt.Equals, tc.want)
		})
	}
}
