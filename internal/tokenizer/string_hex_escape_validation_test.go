package tokenizer

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

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
