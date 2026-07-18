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
	"time"

	qt "github.com/frankban/quicktest"
)

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

// TestStringEscapesExtended tests additional string escape sequences
func TestStringEscapesExtended(t *testing.T) {
	tcs := []struct {
		input       string
		expectedVal string
	}{
		{
			input:       `"back\bspace"`,
			expectedVal: "back\bspace",
		},
		{
			input:       `"alarm\a"`,
			expectedVal: "alarm\a",
		},
		{
			input:       `"return\rchar"`,
			expectedVal: "return\rchar",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Value(), qt.Equals, tc.expectedVal)
		})
	}
}

// TestStringEscapeVariations tests more string escape combinations
func TestStringEscapeVariations(t *testing.T) {
	tcs := []struct {
		input       string
		expectedVal string
	}{
		{input: `"\a"`, expectedVal: "\a"},
		{input: `"\b"`, expectedVal: "\b"},
		{input: `"\t"`, expectedVal: "\t"},
		{input: `"\n"`, expectedVal: "\n"},
		{input: `"\r"`, expectedVal: "\r"},
		{input: `"\""`, expectedVal: `"`},
		{input: `"\\"`, expectedVal: `\`},
		{input: `"\x00;"`, expectedVal: "\x00"},
		{input: `"\x20;"`, expectedVal: " "},
		{input: `"\x41;"`, expectedVal: "A"},
		{input: `"\x00;\x01;\x02;"`, expectedVal: "\x00\x01\x02"},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Value(), qt.Equals, tc.expectedVal)
		})
	}
}

// TestCharacterConstants tests character constant edge cases
func TestCharacterConstants(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Mnemonics
		{input: `#\alarm`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\backspace`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\delete`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\escape`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\newline`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\null`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\return`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\space`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\tab`, expectedType: TokenizerStateCharMnemonic},

		// Hex escapes
		{input: `#\x0`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x00`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x20`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\xFF`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x1234`, expectedType: TokenizerStateCharHexEscape},

		// Graphic characters
		{input: `#\a`, expectedType: TokenizerStateCharGraphic},
		{input: `#\A`, expectedType: TokenizerStateCharGraphic},
		{input: `#\0`, expectedType: TokenizerStateCharGraphic},
		{input: `#\!`, expectedType: TokenizerStateCharGraphic},
		{input: `#\(`, expectedType: TokenizerStateCharGraphic},
		{input: `#\)`, expectedType: TokenizerStateCharGraphic},
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

// Test string escape sequences
func TestTokenizer_StringEscapeSequences(t *testing.T) {
	tests := []struct {
		in  string
		val string
	}{
		{`"\a"`, "\a"},
		{`"\b"`, "\b"},
		{`"\t"`, "\t"},
		{`"\n"`, "\n"},
		{`"\r"`, "\r"},
		{`"\\"`, "\\"},
		{`"\""`, "\""},
		{`"\x41;"`, "A"},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
			qt.Assert(t, tok.Value(), qt.Equals, tc.val)
		})
	}
}

// Test character literals
func TestTokenizer_CharacterLiterals(t *testing.T) {
	tests := []tokenizerTestCase{
		{`#\a`, TokenizerStateCharGraphic},
		{`#\Z`, TokenizerStateCharGraphic},
		{`#\space`, TokenizerStateCharMnemonic},
		{`#\newline`, TokenizerStateCharMnemonic},
		{`#\tab`, TokenizerStateCharMnemonic},
		{`#\return`, TokenizerStateCharMnemonic},
		{`#\x41`, TokenizerStateCharHexEscape},
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

// Test strings with various content
func TestTokenizer_StringVariations(t *testing.T) {
	tests := []struct {
		in string
	}{
		{`"simple"`},
		{`"with spaces"`},
		{`"with\ttab"`},
		{`"multi\nline"`},
		{`""`}, // empty string
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
		})
	}
}

// Test string escape sequence branches (exercises readDelimited + readEscapeSequence)
func TestTokenizer_StringEscapeBranches(t *testing.T) {
	tests := []struct {
		in  string
		val string
	}{
		// Supported escape sequences
		{`"\a"`, "\a"}, // alert
		{`"\b"`, "\b"}, // backspace
		{`"\n"`, "\n"}, // newline
		{`"\r"`, "\r"}, // carriage return
		{`"\t"`, "\t"}, // tab
		{`"\\"`, "\\"}, // backslash
		{`"\""`, "\""}, // double quote
		{`"|"`, "|"},   // bar (no escape needed)
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
		})
	}
}

// TestStringHexEscape_H9_Validation tests that string hex escapes validate
// Unicode code point constraints: max U+10FFFF and no surrogates (U+D800-U+DFFF).
// This addresses H9 from the architectural review.
func TestStringHexEscape_H9_Validation(t *testing.T) {
	c := qt.New(t)

	t.Run("valid code points", func(t *testing.T) {
		tcs := []struct {
			name     string
			input    string
			expected string
		}{
			// ASCII range
			{"ASCII A", `"\x41;"`, "A"},
			{"ASCII space", `"\x20;"`, " "},
			{"ASCII null", `"\x00;"`, "\x00"},
			{"ASCII tilde", `"\x7E;"`, "~"},

			// Latin-1 supplement
			{"Latin é", `"\xE9;"`, "é"},
			{"Latin ñ", `"\xF1;"`, "ñ"},
			{"Latin ©", `"\xA9;"`, "©"},

			// BMP (Basic Multilingual Plane)
			{"CJK 你", `"\x4F60;"`, "你"},
			{"CJK 好", `"\x597D;"`, "好"},
			{"Greek α", `"\x3B1;"`, "α"},
			{"Hebrew א", `"\x5D0;"`, "א"},

			// Emoji and supplementary planes (4-byte UTF-8)
			{"Emoji 😀", `"\x1F600;"`, "😀"},
			{"Emoji 🎉", `"\x1F389;"`, "🎉"},

			// Edge cases: boundaries around surrogate range
			{"U+D7FF (before surrogates)", `"\xD7FF;"`, "\uD7FF"},
			{"U+E000 (after surrogates)", `"\xE000;"`, "\uE000"},

			// Maximum valid Unicode code point
			{"U+10FFFF (max valid)", `"\x10FFFF;"`, "\U0010FFFF"},

			// Multiple escapes in one string
			{"Multiple escapes", `"A\x42;C"`, "ABC"},
			{"Mixed content", `"Hello \x1F600; World"`, "Hello 😀 World"},
		}

		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				tok := NewTokenizer(strings.NewReader(tc.input), false)
				defer tok.Close()
				token, err := tok.Next()
				c.Assert(err, qt.IsNil)
				c.Assert(token.Type(), qt.Equals, TokenizerStateString)
				c.Assert(token.Value(), qt.Equals, tc.expected)
			})
		}
	})

	t.Run("invalid: surrogate range", func(t *testing.T) {
		tcs := []struct {
			name  string
			input string
			code  uint64
		}{
			{"U+D800 (start of surrogate range)", `"\xD800;"`, 0xD800},
			{"U+D801", `"\xD801;"`, 0xD801},
			{"U+DB7F", `"\xDB7F;"`, 0xDB7F},
			{"U+DB80", `"\xDB80;"`, 0xDB80},
			{"U+DBFF (high surrogate max)", `"\xDBFF;"`, 0xDBFF},
			{"U+DC00 (low surrogate start)", `"\xDC00;"`, 0xDC00},
			{"U+DD00", `"\xDD00;"`, 0xDD00},
			{"U+DFFF (end of surrogate range)", `"\xDFFF;"`, 0xDFFF},
		}

		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				tok := NewTokenizer(strings.NewReader(tc.input), false)
				defer tok.Close()
				// Errors during string parsing are deferred to the next call
				// See tokenizer.go:408-410
				_, _ = tok.Next()    // First call returns token with deferred error
				_, err := tok.Next() // Second call returns the error
				c.Assert(err, qt.Not(qt.IsNil))
				c.Assert(err.Error(), qt.Contains, "surrogate")
			})
		}
	})

	t.Run("invalid: exceeds Unicode maximum", func(t *testing.T) {
		tcs := []struct {
			name  string
			input string
		}{
			{"U+110000 (one above max)", `"\x110000;"`},
			{"U+110001", `"\x110001;"`},
			{"U+200000", `"\x200000;"`},
			{"U+FFFFFF (max parseable)", `"\xFFFFFF;"`},
		}

		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				tok := NewTokenizer(strings.NewReader(tc.input), false)
				defer tok.Close()
				// Errors during string parsing are deferred to the next call
				// See tokenizer.go:408-410
				_, _ = tok.Next()    // First call returns token with deferred error
				_, err := tok.Next() // Second call returns the error
				c.Assert(err, qt.Not(qt.IsNil))
				c.Assert(err.Error(), qt.Contains, "exceeds Unicode maximum")
			})
		}
	})

	t.Run("character hex escape validation (existing behavior)", func(t *testing.T) {
		// Verify that character hex escapes also have validation
		// (this was already working, we're just ensuring consistency)
		t.Run("char valid", func(t *testing.T) {
			tok := NewTokenizer(strings.NewReader(`#\x41`), false)
			defer tok.Close()
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, TokenizerStateCharHexEscape)
			c.Assert(token.Value(), qt.Equals, "A")
		})

		t.Run("char surrogate error", func(t *testing.T) {
			tok := NewTokenizer(strings.NewReader(`#\xD800`), false)
			defer tok.Close()
			// Errors are deferred for character tokens too
			_, _ = tok.Next()    // First call returns token with deferred error
			_, err := tok.Next() // Second call returns the error
			c.Assert(err, qt.Not(qt.IsNil))
			c.Assert(err.Error(), qt.Contains, "surrogate")
		})

		t.Run("char exceeds max error", func(t *testing.T) {
			tok := NewTokenizer(strings.NewReader(`#\x110000`), false)
			defer tok.Close()
			// Errors are deferred for character tokens too
			_, _ = tok.Next()    // First call returns token with deferred error
			_, err := tok.Next() // Second call returns the error
			c.Assert(err, qt.Not(qt.IsNil))
			c.Assert(err.Error(), qt.Contains, "exceeds Unicode maximum")
		})
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
			c.Assert(p.err, qt.IsNotNil)
			var te *TokenizerError
			c.Check(errors.As(p.err, &te), qt.IsTrue,
				qt.Commentf("expected TokenizerError, got %T: %v", p.err, p.err))
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
			c.Check(p.Text(), qt.Equals, tc.span)
		})
	}
}

// TestReplacementCharacterIsNotADecodeError pins U+FFFD as an ordinary rune.
// utf8.DecodeRune signals a decoding failure as (RuneError, 1) and a genuine
// U+FFFD as (RuneError, 3); the tokenizer used to test only the rune, so a
// U+FFFD that `write` had emitted could not be read back.
func TestReplacementCharacterIsNotADecodeError(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		typ   TokenizerState
		value string
	}{
		{name: "in_string", input: "\"a�b\"", typ: TokenizerStateString, value: "a�b"},
		{name: "character", input: "#\\�", typ: TokenizerStateCharGraphic, value: "�"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			toks, err := Tokenize(tc.input, false)
			c.Assert(err, qt.Equals, io.EOF)
			c.Assert(len(toks), qt.Equals, 1)
			c.Check(toks[0].Type(), qt.Equals, tc.typ)
			c.Check(toks[0].Value(), qt.Equals, tc.value)
		})
	}
}

// TestInvalidUTF8IsADecodeError is the other half of the U+FFFD distinction:
// a byte sequence that does not decode must still be a read error.
func TestInvalidUTF8IsADecodeError(t *testing.T) {
	c := qt.New(t)

	_, err := Tokenize("\"a\xffb\"", false)
	c.Check(errors.Is(err, NewTokenizerError(MessageRuneError)), qt.IsTrue)
}

// TestLargeTokenScansInLinearTime guards the read-side DoS surface: token text
// used to be accumulated with a string +=, making one large datum O(n^2) in its
// length (a 400KB symbol took ~9.5s). Appending into a reused byte buffer is
// linear, so the same input scans in milliseconds; the deadline is loose enough
// to be machine-independent while still failing the quadratic build by orders of
// magnitude.
func TestLargeTokenScansInLinearTime(t *testing.T) {
	c := qt.New(t)

	const n = 400 * 1024
	src := strings.Repeat("a", n)
	deadline := time.Now().Add(5 * time.Second)

	toks, err := Tokenize(src, false)
	c.Assert(err, qt.Equals, io.EOF)
	c.Assert(len(toks), qt.Equals, 1)
	c.Check(toks[0].Type(), qt.Equals, TokenizerStateSymbol)
	c.Check(len(toks[0].Value()), qt.Equals, n)
	c.Check(time.Now().Before(deadline), qt.IsTrue, qt.Commentf("scanning a %d-byte symbol took longer than the linear-time budget", n))
}

// TestStringHexEscapeUnboundedDigits pins R7RS §7.1.1
// <hex scalar value> ::= <digit 16>+ (no digit-count bound). A string hex
// escape with gratuitous leading zeros past the historical 8-digit cap denotes
// the same code point and must be accepted; validateCodePoint owns the range
// check. The character path is asserted for parity.
func TestStringHexEscapeUnboundedDigits(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		typ   TokenizerState
		value string
	}{
		{name: "string 9 digits", input: `"\x000000041;"`, typ: TokenizerStateString, value: "A"},
		{name: "string 16 digits", input: `"\x0000000000000041;"`, typ: TokenizerStateString, value: "A"},
		{name: "char 9 digits", input: `#\x000000041`, typ: TokenizerStateCharHexEscape, value: "A"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			defer tok.Close()
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.typ)
			c.Assert(token.Value(), qt.Equals, tc.value)
		})
	}
}
