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
	"io"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestTokenizerErrorCarriesScannerState pins the reader-diagnostic contract:
// every lexical fault records where it stopped, the character it stopped on,
// and the lexical state it was scanning in. Before this, TokenizerError held
// only a message, so "#b109" surfaced as the shapeless "malformed input".
//
// The wantRune/wantState columns are the point — asserting only that an error
// occurred is what let the facts go missing for as long as they did.
func TestTokenizerErrorCarriesScannerState(t *testing.T) {
	tcs := []struct {
		name      string
		src       string
		wantMess  string
		wantIndex int
		wantCol   int
		wantLine  int
		wantRune  rune
		wantEOF   bool
		wantState TokenizerState
	}{{
		name: "out_of_radix_digit", src: "#b109",
		wantMess: MessageExpectingDelimiterAfterNumber,
		// Index 4 is the '9', not the start of the numeral: the digit set was
		// legal through "10" and the fault is the character that broke it.
		wantIndex: 4, wantCol: 4, wantLine: 1,
		wantRune: '9', wantState: TokenizerStateUnsignedInteger,
	}, {
		name: "trailing_symbol_char_after_radix_fraction", src: "#x1.8z",
		wantMess:  MessageExpectingDelimiterAfterNumber,
		wantIndex: 5, wantCol: 5, wantLine: 1,
		wantRune: 'z', wantState: TokenizerStateUnsignedDecimalFraction,
	}, {
		name: "unterminated_string_reports_eof", src: `"abc`,
		wantMess:  MessageUnterminatedString,
		wantIndex: 4, wantCol: 4, wantLine: 1,
		wantEOF: true, wantState: TokenizerStateFailed,
	}, {
		// The fault is on line 2: line/column must be tracked, not just index.
		name: "position_tracks_newlines", src: "(a b)\n#b19",
		wantMess:  MessageExpectingDelimiterAfterNumber,
		wantIndex: 9, wantCol: 3, wantLine: 2,
		wantRune: '9', wantState: TokenizerStateUnsignedInteger,
	}, {
		name: "bad_bytevector_prefix", src: "#u9(",
		wantMess:  MessageExpectingByteVectorPrefix,
		wantIndex: 2, wantCol: 2, wantLine: 1,
		wantRune: '9', wantState: TokenizerStateFailed,
	}}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			_, err := Tokenize(tc.src, false)
			var terr *TokenizerError
			c.Assert(errors.As(err, &terr), qt.IsTrue,
				qt.Commentf("tokenizing %q gave %T: %v", tc.src, err, err))

			// Message identity is unaffected by the stamp: errors.Is against a
			// bare sentinel must keep matching.
			c.Check(errors.Is(terr, NewTokenizerError(tc.wantMess)), qt.IsTrue,
				qt.Commentf("message identity lost; got %v", terr))

			at, located := terr.At()
			c.Assert(located, qt.IsTrue)
			c.Check(at.Index(), qt.Equals, tc.wantIndex)
			c.Check(at.Column(), qt.Equals, tc.wantCol)
			c.Check(at.Line(), qt.Equals, tc.wantLine)

			r, isChar := terr.Rune()
			c.Check(isChar, qt.Equals, !tc.wantEOF)
			if !tc.wantEOF {
				c.Check(r, qt.Equals, tc.wantRune)
			}
			c.Check(terr.State(), qt.Equals, tc.wantState)

			// Every fact must also reach the rendered text — a structured field
			// nobody prints is not a diagnostic.
			rendered := terr.Error()
			for _, want := range []string{
				tc.wantMess,
				"index " + strconv.Itoa(tc.wantIndex),
				"line " + strconv.Itoa(tc.wantLine),
				"column " + strconv.Itoa(tc.wantCol),
				tc.wantState.String(),
			} {
				c.Check(strings.Contains(rendered, want), qt.IsTrue,
					qt.Commentf("rendered %q lacks %q", rendered, want))
			}
		})
	}
}

// TestTokenizerErrorDistinguishesEOFFromLiteralReplacementChar pins the one
// place the offending-character report could lie. readNextRune reports end of
// input as utf8.RuneError, and a U+FFFD written in the source decodes to the
// same rune — so the rune alone cannot tell them apart, and reporting "<end of
// input>" for a real character (or vice versa) would misdirect the reader.
func TestTokenizerErrorDistinguishesEOFFromLiteralReplacementChar(t *testing.T) {
	c := qt.New(t)

	// A literal U+FFFD starts no token: a real character the scanner rejected.
	_, err := Tokenize("�", false)
	var terr *TokenizerError
	c.Assert(errors.As(err, &terr), qt.IsTrue)
	r, isChar := terr.Rune()
	c.Check(isChar, qt.IsTrue)
	c.Check(r, qt.Equals, '�')
	c.Check(strings.Contains(terr.Error(), "<end of input>"), qt.IsFalse,
		qt.Commentf("literal U+FFFD misreported as EOF: %v", terr))

	// Genuine end of input inside a string.
	_, err = Tokenize(`"abc`, false)
	c.Assert(errors.As(err, &terr), qt.IsTrue)
	_, isChar = terr.Rune()
	c.Check(isChar, qt.IsFalse)
	c.Check(strings.Contains(terr.Error(), "<end of input>"), qt.IsTrue,
		qt.Commentf("EOF not reported as such: %v", terr))
}

// TestTokenizerErrorSentinelRendersBare pins that the New* constructors stay
// position-free. They build the comparison values for errors.Is; appending a
// zero position to them would put "line 0" into any message that used one.
func TestTokenizerErrorSentinelRendersBare(t *testing.T) {
	c := qt.New(t)

	sentinel := NewTokenizerError(MessageExpectingNumber)
	c.Check(sentinel.Error(), qt.Equals, MessageExpectingNumber)
	_, located := sentinel.At()
	c.Check(located, qt.IsFalse)
}

// TestTokenizerErrorIsHonorsMessage pins the corrected semantics: two
// tokenizer errors are errors.Is-equal iff they carry the same message.
// Previously Is returned true for any *TokenizerError, making all 25 message
// constants mutually indistinguishable.
func TestTokenizerErrorIsHonorsMessage(t *testing.T) {
	a := NewTokenizerError(MessageExpectingNumber)
	b := NewTokenizerError(MessageExpectingNumber)
	c := NewTokenizerError(MessageUnterminatedString)

	if !errors.Is(a, b) {
		t.Errorf("same-message tokenizer errors should be errors.Is-equal")
	}
	if errors.Is(a, c) {
		t.Errorf("%q and %q should NOT be errors.Is-equal", a, c)
	}
}

// TestTokenizerErrorIsMatchesThroughWrap shows message identity survives
// wrapping, and that the Unwrap chain still matches a wrapped cause.
func TestTokenizerErrorIsMatchesThroughWrap(t *testing.T) {
	wrapped := NewTokenizerErrorWithWrap(io.EOF, MessageExpectingToken)

	if !errors.Is(wrapped, io.EOF) {
		t.Errorf("tokenizer error wrapping io.EOF should match io.EOF via Unwrap")
	}
	if !errors.Is(wrapped, NewTokenizerError(MessageExpectingToken)) {
		t.Errorf("wrapped error should match a same-message sentinel by message identity")
	}
	if errors.Is(wrapped, NewTokenizerError(MessageExpectingNumber)) {
		t.Errorf("wrapped error should not match a different-message sentinel")
	}
}
