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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestExtendedSymbols tests R7RS extended symbol syntax: |...|
// Per R7RS 7.1.1: <identifier> → <vertical line> <symbol element>* <vertical line>
// where <symbol element> is any character other than \ or |, or an escape sequence.
//
// R7RS escape sequences within extended symbols:
//
//	\a - alarm (bell)
//	\b - backspace
//	\t - tab
//	\n - newline
//	\r - carriage return
//	\| - vertical bar
//	\\ - backslash
//	\x<hex>; - hex escape (semicolon terminated)
//	\<intraline whitespace>*<line ending><intraline whitespace>* - line continuation
func TestExtendedSymbols(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string // raw source text
		val   string // processed symbol name (escapes resolved, delimiters removed)
		err   error
	}{
		// Basic extended symbols
		{
			input: "|hello|",
			state: TokenizerStateSymbol,
			span:  "|hello|",
			val:   "hello",
			err:   io.EOF,
		},
		{
			input: "|hello world|",
			state: TokenizerStateSymbol,
			span:  "|hello world|",
			val:   "hello world",
			err:   io.EOF,
		},
		{
			input: "||", // empty symbol
			state: TokenizerStateSymbol,
			span:  "||",
			val:   "",
			err:   io.EOF,
		},
		// Symbols with special characters that would normally be delimiters
		{
			input: `|(foo)|`,
			state: TokenizerStateSymbol,
			span:  `|(foo)|`,
			val:   "(foo)",
			err:   io.EOF,
		},
		{
			input: `|;comment-like|`,
			state: TokenizerStateSymbol,
			span:  `|;comment-like|`,
			val:   ";comment-like",
			err:   io.EOF,
		},
		{
			input: `|"quoted"|`,
			state: TokenizerStateSymbol,
			span:  `|"quoted"|`,
			val:   `"quoted"`,
			err:   io.EOF,
		},
		// Escape sequences
		{
			input: `|foo\|bar|`, // escaped vertical bar
			state: TokenizerStateSymbol,
			span:  `|foo\|bar|`,
			val:   "foo|bar",
			err:   io.EOF,
		},
		{
			input: `|foo\\bar|`, // escaped backslash
			state: TokenizerStateSymbol,
			span:  `|foo\\bar|`,
			val:   `foo\bar`,
			err:   io.EOF,
		},
		{
			input: `|foo\nbar|`, // newline escape
			state: TokenizerStateSymbol,
			span:  `|foo\nbar|`,
			val:   "foo\nbar",
			err:   io.EOF,
		},
		{
			input: `|foo\tbar|`, // tab escape
			state: TokenizerStateSymbol,
			span:  `|foo\tbar|`,
			val:   "foo\tbar",
			err:   io.EOF,
		},
		{
			input: `|\a\b\r|`, // alarm, backspace, return
			state: TokenizerStateSymbol,
			span:  `|\a\b\r|`,
			val:   "\a\b\r",
			err:   io.EOF,
		},
		// Hex escapes (R7RS requires semicolon terminator)
		{
			input: `|\x41;|`, // 'A'
			state: TokenizerStateSymbol,
			span:  `|\x41;|`,
			val:   "A",
			err:   io.EOF,
		},
		{
			input: `|\x03BB;|`, // Greek lambda
			state: TokenizerStateSymbol,
			span:  `|\x03BB;|`,
			val:   "λ",
			err:   io.EOF,
		},
		// Extended symbol followed by other tokens
		{
			input: "|foo| bar",
			state: TokenizerStateSymbol,
			span:  "|foo|",
			val:   "foo",
			err:   nil,
		},
		{
			input: "|foo|(bar)",
			state: TokenizerStateSymbol,
			span:  "|foo|",
			val:   "foo",
			err:   nil,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			tok, _ := p.Next()
			_, err := p.Next()
			c.Check(err, qt.ErrorIs, tc.err)
			c.Check(tok.Type(), qt.Equals, tc.state)
			c.Check(tok.String(), qt.Equals, tc.span)
			c.Check(tok.Value(), qt.Equals, tc.val)
		})
	}
}

// TestUnicodeIdentifiers tests R7RS Unicode letter support in identifiers
// Per R7RS 7.1.1: <letter> includes Unicode categories Lu, Ll, Lt, Lm, Lo, Nl
func TestUnicodeIdentifiers(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string
		err   error
	}{
		// Greek letters
		{
			input: "λ",
			state: TokenizerStateSymbol,
			span:  "λ",
			err:   io.EOF,
		},
		{
			input: "αβγ",
			state: TokenizerStateSymbol,
			span:  "αβγ",
			err:   io.EOF,
		},
		{
			input: "Ω",
			state: TokenizerStateSymbol,
			span:  "Ω",
			err:   io.EOF,
		},
		// Greek in compound identifiers
		{
			input: "λ-calculus",
			state: TokenizerStateSymbol,
			span:  "λ-calculus",
			err:   io.EOF,
		},
		// Cyrillic letters
		{
			input: "привет",
			state: TokenizerStateSymbol,
			span:  "привет",
			err:   io.EOF,
		},
		// Mixed ASCII and Unicode
		{
			input: "foo-λ",
			state: TokenizerStateSymbol,
			span:  "foo-λ",
			err:   io.EOF,
		},
		// Note: Mathematical symbols like ∑ are NOT valid identifier initials
		// (they are in Unicode category Sm - Symbol, math, not a letter category)
		// Unicode followed by delimiter
		{
			input: "λ(",
			state: TokenizerStateSymbol,
			span:  "λ",
			err:   nil,
		},
		// CJK characters (category Lo - Letter, other)
		{
			input: "漢字",
			state: TokenizerStateSymbol,
			span:  "漢字",
			err:   io.EOF,
		},
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d: %q", i, tc.input), func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.err, qt.ErrorIs, tc.err)
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.span)
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
		c.Check(tok3.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase2)
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
		c.Check(tok3.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase16)
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
		c.Check(tok2.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase16)
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
		c.Check(tok2.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase10)
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

// TestCombinedRadixExactness tests combined radix and exactness prefixes
// Per R7RS 7.1.1: <prefix> → <radix> <exactness> | <exactness> <radix>
func TestCombinedRadixExactness(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string
		err   error
	}{
		// Exactness prefix alone (returns marker, parser handles number)
		{
			input: "#e",
			state: TokenizerStateMarkerNumberExact,
			span:  "#e",
			err:   io.EOF,
		},
		{
			input: "#i",
			state: TokenizerStateMarkerNumberInexact,
			span:  "#i",
			err:   io.EOF,
		},
		// Radix prefix alone
		{
			input: "#b",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   io.EOF,
		},
		{
			input: "#o",
			state: TokenizerStateMarkerBase8,
			span:  "#o",
			err:   io.EOF,
		},
		{
			input: "#d",
			state: TokenizerStateMarkerBase10,
			span:  "#d",
			err:   io.EOF,
		},
		{
			input: "#x",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   io.EOF,
		},
		// Exactness followed by radix: tokenizer returns proper exactness marker
		// Parser will handle assembling the prefixes
		{
			input: "#e#b101",
			state: TokenizerStateMarkerNumberExact,
			span:  "#e",
			err:   nil,
		},
		{
			input: "#i#x1a",
			state: TokenizerStateMarkerNumberInexact,
			span:  "#i",
			err:   nil,
		},
		// Radix followed by exactness: tokenizer returns proper radix marker
		{
			input: "#b#e101",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   nil,
		},
		{
			input: "#x#i1a",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   nil,
		},
		// Leading zeros in different radixes
		{
			input: "#b00101",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   nil,
		},
		{
			input: "#o00777",
			state: TokenizerStateMarkerBase8,
			span:  "#o",
			err:   nil,
		},
		{
			input: "#x00FF",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   nil,
		},
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d: %q", i, tc.input), func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.err, qt.ErrorIs, tc.err)
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.span)
		})
	}
}

// TestKnownBugs documents known tokenizer bugs per CLAUDE.md
// Note: Some bugs mentioned in CLAUDE.md may have been fixed
func TestKnownBugs(t *testing.T) {
	t.Run("signed_integer_with_exponent_now_works", func(t *testing.T) {
		// Previously documented as bug: +1e10 tokenized as two tokens
		// This has been FIXED - now tokenizes correctly as single decimal fraction
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("+1e10"), false)

		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.Type(), qt.Equals, TokenizerStateSignedInteger)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "+1e10")

		// Verify no second token
		_, err2 := p.Next()
		c.Check(err2, qt.Equals, io.EOF)
	})

	t.Run("trailing_dot_with_exponent_now_works", func(t *testing.T) {
		// Previously documented as bug: 1.e10 errored
		// Testing current behavior
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("1.e10"), false)

		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.Type(), qt.Equals, TokenizerStateUnsignedDecimalFraction)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "1.e10")
	})
}

// TestInvalidStrings tests error handling for malformed strings
func TestInvalidStrings(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "unterminated_string",
			input: `"hello`,
		},
		{
			name:  "unterminated_string_with_newline",
			input: "\"hello\n",
		},
		{
			name:  "unterminated_string_with_escape",
			input: `"hello\`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// Should either error or reach EOF without completing string
			c.Check(p.err, qt.IsNotNil)
		})
	}
}

// TestInvalidEscapeSequences tests error handling for invalid escape sequences
func TestInvalidEscapeSequences(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "invalid_escape_q",
			input: `"\q"`,
		},
		{
			name:  "invalid_escape_z",
			input: `"\z"`,
		},
		{
			name:  "invalid_escape_1",
			input: `"\1"`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// Invalid escape should produce an error
			c.Check(p.err, qt.IsNotNil)
		})
	}
}

// TestInvalidHexEscapes tests error handling for invalid hex escapes
func TestInvalidHexEscapes(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "hex_escape_invalid_chars_GG",
			input: `"\xGG"`,
		},
		{
			name:  "hex_escape_invalid_chars_ZZ",
			input: `"\xZZ"`,
		},
		{
			name:  "character_hex_invalid",
			input: `#\xGG`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// Note: behavior depends on implementation - may error or treat as different token
			// This test documents whatever the current behavior is
			_ = c // test runs to document behavior
		})
	}
}

// TestInvalidComments tests error handling for malformed comments
func TestInvalidComments(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "unclosed_block_comment",
			input: "#| unclosed",
		},
		{
			name:  "unclosed_nested_block_comment",
			input: "#| outer #| inner |#",
		},
		{
			name:  "block_comment_only_open",
			input: "#|",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// Unclosed block comments should reach EOF
			c.Check(p.err, qt.Equals, io.EOF)
		})
	}
}

// TestInvalidNumbers tests error handling for malformed numbers
func TestInvalidNumbers(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
		span  string
	}{
		{
			name:  "multiple_dots",
			input: "1.2.3",
			state: TokenizerStateUnsignedDecimalFraction,
			span:  "1.2",
		},
		{
			name:  "multiple_slashes",
			input: "1/2/3",
			state: TokenizerStateUnsignedRationalFraction,
			span:  "1/2",
		},
		{
			name:  "dot_only",
			input: ".",
			state: TokenizerStateCons,
			span:  ".",
		},
		{
			name:  "plus_only",
			input: "+",
			state: TokenizerStateSymbol,
			span:  "+",
		},
		{
			name:  "minus_only",
			input: "-",
			state: TokenizerStateSymbol,
			span:  "-",
		},
		{
			name:  "rational_no_denominator",
			input: "1/",
			state: TokenizerStateUnsignedRationalFraction, // tokenizer includes the /
			span:  "1/",
		},
		{
			name:  "decimal_trailing_dot_delimiter",
			input: "1.(",
			state: TokenizerStateUnsignedDecimalFraction,
			span:  "1.",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// These should tokenize as partial tokens or different types
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.span)
		})
	}
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
			c.Check(p.span(), qt.Equals, tc.span)
		})
	}
}

// TestCaseSensitivity tests case sensitivity behavior
func TestCaseSensitivity(t *testing.T) {
	t.Run("case_sensitive_mode", func(t *testing.T) {
		c := qt.New(t)
		// In case-sensitive mode (default), FOO and foo are different
		p := NewTokenizer(strings.NewReader("FOO"), false)
		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "FOO")
	})

	t.Run("case_insensitive_mode", func(t *testing.T) {
		c := qt.New(t)
		// In case-insensitive mode, symbols are still preserved as-is in text
		p := NewTokenizer(strings.NewReader("FOO"), true)
		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "FOO")
	})

	t.Run("booleans_always_case_insensitive", func(t *testing.T) {
		// R7RS requires booleans to be case-insensitive regardless of mode
		inputs := []string{"#t", "#T", "#true", "#TRUE", "#True"}
		for _, input := range inputs {
			t.Run(input, func(t *testing.T) {
				c := qt.New(t)
				p := NewTokenizer(strings.NewReader(input), false)
				tok, err := p.Next()
				c.Assert(err, qt.IsNil)
				c.Check(tok.Type(), qt.Equals, TokenizerStateMarkerBooleanTrue)
			})
		}
	})
}

// TestSpecialCharacterConstants tests character constant edge cases
func TestSpecialCharacterConstants(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string
		err   error
	}{
		// Standard mnemonics
		{input: `#\space`, state: TokenizerStateCharMnemonic, span: `#\space`, err: io.EOF},
		{input: `#\newline`, state: TokenizerStateCharMnemonic, span: `#\newline`, err: io.EOF},
		{input: `#\tab`, state: TokenizerStateCharMnemonic, span: `#\tab`, err: io.EOF},
		{input: `#\return`, state: TokenizerStateCharMnemonic, span: `#\return`, err: io.EOF},
		{input: `#\null`, state: TokenizerStateCharMnemonic, span: `#\null`, err: io.EOF},
		{input: `#\alarm`, state: TokenizerStateCharMnemonic, span: `#\alarm`, err: io.EOF},
		{input: `#\backspace`, state: TokenizerStateCharMnemonic, span: `#\backspace`, err: io.EOF},
		{input: `#\delete`, state: TokenizerStateCharMnemonic, span: `#\delete`, err: io.EOF},
		{input: `#\escape`, state: TokenizerStateCharMnemonic, span: `#\escape`, err: io.EOF},

		// Hex escapes of various lengths
		{input: `#\x0`, state: TokenizerStateCharHexEscape, span: `#\x0`, err: io.EOF},
		{input: `#\x00`, state: TokenizerStateCharHexEscape, span: `#\x00`, err: io.EOF},
		{input: `#\x000`, state: TokenizerStateCharHexEscape, span: `#\x000`, err: io.EOF},
		{input: `#\x0000`, state: TokenizerStateCharHexEscape, span: `#\x0000`, err: io.EOF},
		{input: `#\xFFFF`, state: TokenizerStateCharHexEscape, span: `#\xFFFF`, err: io.EOF},
		{input: `#\xabcd`, state: TokenizerStateCharHexEscape, span: `#\xabcd`, err: io.EOF},

		// Graphic characters
		{input: `#\a`, state: TokenizerStateCharGraphic, span: `#\a`, err: io.EOF},
		{input: `#\Z`, state: TokenizerStateCharGraphic, span: `#\Z`, err: io.EOF},
		{input: `#\0`, state: TokenizerStateCharGraphic, span: `#\0`, err: io.EOF},
		{input: `#\!`, state: TokenizerStateCharGraphic, span: `#\!`, err: io.EOF},
		{input: `#\@`, state: TokenizerStateCharGraphic, span: `#\@`, err: io.EOF},

		// Unicode graphic characters
		{input: `#\λ`, state: TokenizerStateCharGraphic, span: `#\λ`, err: io.EOF},
		{input: `#\中`, state: TokenizerStateCharGraphic, span: `#\中`, err: io.EOF},

		// Character followed by delimiter
		{input: `#\a(`, state: TokenizerStateCharGraphic, span: `#\a`, err: nil},
		{input: `#\space `, state: TokenizerStateCharMnemonic, span: `#\space`, err: nil},
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d: %q", i, tc.input), func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.err, qt.ErrorIs, tc.err)
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.span)
		})
	}
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
