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

// TestPeculiarIdentifiersExtended tests additional peculiar identifier cases
func TestPeculiarIdentifiersExtended(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "...more",
			expectedType: TokenizerStateSymbol,
			expectedStr:  "...more",
		},
		{
			input:        ".foo",
			expectedType: TokenizerStateSymbol,
			expectedStr:  ".foo",
		},
		{
			input:        "+soup+",
			expectedType: TokenizerStateSymbol,
			expectedStr:  "+soup+",
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

// TestEdgeCaseSymbols tests edge case symbols to improve coverage
func TestEdgeCaseSymbols(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Dot-subsequent symbols
		{input: ".+", expectedType: TokenizerStateSymbol},
		{input: ".-", expectedType: TokenizerStateSymbol},
		{input: "..", expectedType: TokenizerStateSymbol},
		{input: "...", expectedType: TokenizerStateSymbol},
		{input: ".@", expectedType: TokenizerStateSymbol},

		// Sign-subsequent symbols
		{input: "+.", expectedType: TokenizerStateSymbol},
		{input: "+..", expectedType: TokenizerStateSymbol},
		{input: "+...", expectedType: TokenizerStateSymbol},
		{input: "-.", expectedType: TokenizerStateSymbol},
		{input: "-..", expectedType: TokenizerStateSymbol},
		{input: "-...", expectedType: TokenizerStateSymbol},

		// Other peculiar identifiers
		{input: "+@", expectedType: TokenizerStateSymbol},
		{input: "-@", expectedType: TokenizerStateSymbol},
		{input: "+!", expectedType: TokenizerStateSymbol},
		{input: "-!", expectedType: TokenizerStateSymbol},
		{input: "+$", expectedType: TokenizerStateSymbol},
		{input: "-$", expectedType: TokenizerStateSymbol},
		{input: "+%", expectedType: TokenizerStateSymbol},
		{input: "-%", expectedType: TokenizerStateSymbol},
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

// TestSymbolVariations tests various symbol forms
func TestSymbolVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "foo", expectedType: TokenizerStateSymbol},
		{input: "foo-bar", expectedType: TokenizerStateSymbol},
		{input: "foo_bar", expectedType: TokenizerStateSymbol},
		{input: "foo?", expectedType: TokenizerStateSymbol},
		{input: "foo!", expectedType: TokenizerStateSymbol},
		{input: "foo.", expectedType: TokenizerStateSymbol},
		{input: "->string", expectedType: TokenizerStateSymbol},
		{input: "string->list", expectedType: TokenizerStateSymbol},
		{input: "list->vector", expectedType: TokenizerStateSymbol},
		{input: "+", expectedType: TokenizerStateSymbol},
		{input: "-", expectedType: TokenizerStateSymbol},
		{input: "*", expectedType: TokenizerStateSymbol},
		{input: "/", expectedType: TokenizerStateSymbol},
		{input: "<", expectedType: TokenizerStateSymbol},
		{input: ">", expectedType: TokenizerStateSymbol},
		{input: "=", expectedType: TokenizerStateSymbol},
		{input: "<=", expectedType: TokenizerStateSymbol},
		{input: ">=", expectedType: TokenizerStateSymbol},
		{input: "...", expectedType: TokenizerStateSymbol},
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

// Test extended symbols starting with |
// Skipped: Extended symbol implementation is in progress - see TestExtendedSymbols
func TestTokenizer_ExtendedSymbolsBasic(t *testing.T) {
	tests := []tokenizerTestCase{
		{"|", TokenizerStateSymbol},
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
		// Line continuation within extended symbol (R7RS 7.1.1)
		{
			input: "|hello\\\nworld|",
			state: TokenizerStateSymbol,
			span:  "|hello\\\nworld|",
			val:   "helloworld",
			err:   io.EOF,
		},
		{
			input: "|hello\\  \n  world|",
			state: TokenizerStateSymbol,
			span:  "|hello\\  \n  world|",
			val:   "helloworld",
			err:   io.EOF,
		},
		{
			input: "|hello\\\r\nworld|",
			state: TokenizerStateSymbol,
			span:  "|hello\\\r\nworld|",
			val:   "helloworld",
			err:   io.EOF,
		},
		// Multiple escapes combined in one symbol
		{
			input: `|tab\there\nnewline|`,
			state: TokenizerStateSymbol,
			span:  `|tab\there\nnewline|`,
			val:   "tab\there\nnewline",
			err:   io.EOF,
		},
		{
			input: `|\x48;\x65;\x6C;\x6C;\x6F;|`, // "Hello" via hex escapes
			state: TokenizerStateSymbol,
			span:  `|\x48;\x65;\x6C;\x6C;\x6F;|`,
			val:   "Hello",
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

// TestUnterminatedExtendedSymbol tests that unterminated extended symbols
// produce a TokenizerError (not a bare io.EOF) per R7RS §7.1.1.
func TestUnterminatedExtendedSymbol(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "no closing pipe",
			input: "|foo",
		},
		{
			name:  "trailing backslash",
			input: `|foo\`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			_, err1 := p.Next()
			_, err2 := p.Next()
			// Combine: error may surface on either call
			combined := err1
			if combined == nil {
				combined = err2
			}
			c.Assert(combined, qt.IsNotNil)
			var te *TokenizerError
			c.Check(errors.As(combined, &te), qt.IsTrue,
				qt.Commentf("expected TokenizerError, got %T: %v", combined, combined))
		})
	}
}

// TestExtendedSymbolEscapeErrors tests that invalid escape sequences within
// extended symbols produce errors per R7RS 7.1.1.
func TestExtendedSymbolEscapeErrors(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "invalid escape character",
			input: `|foo\qbar|`,
		},
		{
			name:  "hex escape missing semicolon",
			input: `|\x41|`,
		},
		{
			name:  "hex escape with no digits",
			input: `|\x;|`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			_, err1 := p.Next()
			_, err2 := p.Next()
			// Error may surface on either the first or second Next() call
			hasError := err1 != nil || (err2 != nil && err2 != io.EOF)
			c.Check(hasError, qt.IsTrue, qt.Commentf("err1=%v err2=%v", err1, err2))
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
			c.Check(p.Text(), qt.Equals, tc.span)
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
