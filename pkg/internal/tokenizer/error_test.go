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
	"unicode/utf8"

	"github.com/aalpar/wile/pkg/werr"

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
		wantEOF: true, wantState: TokenizerStateString,
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
	}, {
		// #z and #m carry their own rule: neither is a radix prefix, so the fault
		// must not borrow the radix message. State is the big-number state, not
		// the integer state a radix numeral is scanned in.
		name: "bigint_rational_leaves_a_second_datum", src: "#z1/2",
		wantMess:  MessageExpectingDelimiterAfterBigNumber,
		wantIndex: 3, wantCol: 3, wantLine: 1,
		wantRune: '/', wantState: TokenizerStateBigInteger,
	}, {
		name: "bigfloat_complex_leaves_a_second_datum", src: "#m2+3i",
		wantMess:  MessageExpectingDelimiterAfterBigNumber,
		wantIndex: 3, wantCol: 3, wantLine: 1,
		wantRune: '+', wantState: TokenizerStateBigFloat,
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

			// A reader that knows the file gets the same facts with the
			// position as file:line:col; without a name the form is Error's.
			inFile := terr.ErrorInFile("f.scm")
			for _, want := range []string{
				tc.wantMess,
				"(at f.scm:" + strconv.Itoa(tc.wantLine) + ":" + strconv.Itoa(tc.wantCol) +
					", index " + strconv.Itoa(tc.wantIndex) + ", character ",
				"tokenizer state " + tc.wantState.String() + ")",
			} {
				c.Check(strings.Contains(inFile, want), qt.IsTrue,
					qt.Commentf("ErrorInFile %q lacks %q", inFile, want))
			}
			c.Check(terr.ErrorInFile(""), qt.Equals, rendered)
		})
	}
}

// TestFaultPastACompleteTokenIsIntertoken pins which state a fault on the rune
// after a token reports. The scanner always reads one rune past a token; when
// that rune could still extend the token ("(" might be "()", "ab" might be
// "abc") it is read while the token is being scanned and reports the token's
// state. When the token's last rune already fixed its extent (")", "#(", the
// closing "|" of "|ab|"), the rune is read in intertoken space and reports
// failed, as Next reports a fault in whitespace. The token itself keeps its
// type either way. The fault is a reader failing right after the prefix.
func TestFaultPastACompleteTokenIsIntertoken(t *testing.T) {
	closed := werr.WrapForeignErrorf(werr.ErrPortClosed, "string-input-port is closed")
	tcs := []struct {
		prefix    string
		wantTok   TokenizerState
		wantState TokenizerState
	}{
		// Complete once their last rune is consumed.
		{prefix: ")", wantTok: TokenizerStateCloseParen, wantState: TokenizerStateFailed},
		{prefix: "]", wantTok: TokenizerStateCloseBracket, wantState: TokenizerStateFailed},
		{prefix: "()", wantTok: TokenizerStateEmptyList, wantState: TokenizerStateFailed},
		{prefix: "[]", wantTok: TokenizerStateEmptyList, wantState: TokenizerStateFailed},
		{prefix: "'", wantTok: TokenizerStateQuote, wantState: TokenizerStateFailed},
		{prefix: "`", wantTok: TokenizerStateQuasiquote, wantState: TokenizerStateFailed},
		{prefix: ",@", wantTok: TokenizerStateUnquoteSplicing, wantState: TokenizerStateFailed},
		{prefix: "#'", wantTok: TokenizerStateSyntax, wantState: TokenizerStateFailed},
		{prefix: "#`", wantTok: TokenizerStateQuasisyntax, wantState: TokenizerStateFailed},
		{prefix: "#,@", wantTok: TokenizerStateUnsyntaxSplicing, wantState: TokenizerStateFailed},
		{prefix: "#(", wantTok: TokenizerStateOpenVector, wantState: TokenizerStateFailed},
		{prefix: "#u8(", wantTok: TokenizerStateOpenVectorUnsignedByteMarker, wantState: TokenizerStateFailed},
		{prefix: "#;", wantTok: TokenizerStateDatumCommentBegin, wantState: TokenizerStateFailed},
		{prefix: "#&", wantTok: TokenizerStateBoxBegin, wantState: TokenizerStateFailed},
		{prefix: "#1=", wantTok: TokenizerStateLabelAssignment, wantState: TokenizerStateFailed},
		{prefix: "#1#", wantTok: TokenizerStateLabelReference, wantState: TokenizerStateFailed},
		{prefix: "|ab|", wantTok: TokenizerStateSymbol, wantState: TokenizerStateFailed},
		{prefix: "#|c|#", wantTok: TokenizerStateBlockCommentBody, wantState: TokenizerStateFailed},
		{prefix: `"a"`, wantTok: TokenizerStateString, wantState: TokenizerStateFailed},
		{prefix: `#\(`, wantTok: TokenizerStateCharGraphic, wantState: TokenizerStateFailed},
		{prefix: `#\1`, wantTok: TokenizerStateCharGraphic, wantState: TokenizerStateFailed},
		// Still open when the rune is read: it could have extended the token.
		{prefix: "(", wantTok: TokenizerStateOpenParen, wantState: TokenizerStateOpenParen},
		{prefix: "[", wantTok: TokenizerStateOpenBracket, wantState: TokenizerStateOpenBracket},
		{prefix: ",", wantTok: TokenizerStateUnquote, wantState: TokenizerStateUnquote},
		{prefix: "#,", wantTok: TokenizerStateUnsyntax, wantState: TokenizerStateUnsyntax},
		{prefix: "ab", wantTok: TokenizerStateSymbol, wantState: TokenizerStateSymbol},
		{prefix: `#\a`, wantTok: TokenizerStateCharGraphic, wantState: TokenizerStateCharGraphic},
	}
	for _, tc := range tcs {
		t.Run(tc.prefix, func(t *testing.T) {
			c := qt.New(t)
			rdr := &faultingRuneReader{rdr: strings.NewReader(tc.prefix), after: utf8.RuneCountInString(tc.prefix), fault: closed}
			tok := NewTokenizer(rdr, false)
			first, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Check(first.Type(), qt.Equals, tc.wantTok)
			_, err = tok.Next()
			c.Assert(errors.Is(err, NewTokenizerError(MessageReaderFailed)), qt.IsTrue, qt.Commentf("got %v", err))
			var terr *TokenizerError
			c.Assert(errors.As(err, &terr), qt.IsTrue)
			at, located := terr.At()
			c.Assert(located, qt.IsTrue)
			c.Check(at.Index(), qt.Equals, len(tc.prefix))
			c.Check(terr.State(), qt.Equals, tc.wantState)
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

	// An invalid byte reads as U+FFFD, so it is that same character, not end
	// of input, even as the last byte of the source.
	_, err = Tokenize("\xff", false)
	c.Assert(errors.As(err, &terr), qt.IsTrue)
	r, isChar = terr.Rune()
	c.Check(isChar, qt.IsTrue)
	c.Check(r, qt.Equals, '\uFFFD')
	c.Check(strings.Contains(terr.Error(), "<end of input>"), qt.IsFalse,
		qt.Commentf("invalid byte misreported as EOF: %v", terr))

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
	c.Check(sentinel.ErrorInFile("f.scm"), qt.Equals, MessageExpectingNumber)
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

// faultingRuneReader yields src's runes, then fails every later read with
// fault, the way a closed port or a broken pipe does mid-stream.
type faultingRuneReader struct {
	rdr   *strings.Reader
	after int
	fault error
}

func (p *faultingRuneReader) ReadRune() (rune, int, error) {
	if p.after <= 0 {
		return 0, 0, p.fault
	}
	p.after--
	return p.rdr.ReadRune()
}

// drainTokens reads tokens until the scanner reports an error, and returns it.
func drainTokens(tok *Tokenizer) error {
	for {
		_, err := tok.Next()
		if err != nil {
			return err
		}
	}
}

// TestReaderFaultIsNotEndOfInput pins that a failing reader is reported as
// that failure, not as end of input. readNextRune used to replace any error
// from a zero-byte ReadRune with io.EOF, so a closed port read as #<eof> and a
// fault mid-datum truncated it silently.
func TestReaderFaultIsNotEndOfInput(t *testing.T) {
	closed := werr.WrapForeignErrorf(werr.ErrPortClosed, "string-input-port is closed")
	tcs := []struct {
		name      string
		src       string
		after     int
		wantIndex int
		wantState TokenizerState
	}{
		{name: "fault before the first rune", src: "abc", after: 0, wantIndex: 0, wantState: TokenizerStateFailed},
		{name: "fault inside a symbol", src: "abc", after: 1, wantIndex: 1, wantState: TokenizerStateSymbol},
		{name: "fault inside a list", src: "(a b c)", after: 4, wantIndex: 4, wantState: TokenizerStateSymbol},
		// Between datums no token is being scanned, so the previous token's
		// state is not the one the fault happened in.
		{name: "fault between datums", src: "x y", after: 2, wantIndex: 2, wantState: TokenizerStateFailed},
		{name: "fault after a line comment", src: "; hi\nx", after: 5, wantIndex: 5, wantState: TokenizerStateFailed},
		// ")" is complete before the rune after it is read; "(" might be "()".
		{name: "fault after a close paren", src: "(x)", after: 3, wantIndex: 3, wantState: TokenizerStateFailed},
		{name: "fault after an open paren", src: "(x)", after: 1, wantIndex: 1, wantState: TokenizerStateOpenParen},
		// A string or |symbol| body is scanned in the token's own state, set
		// before the opening delimiter is consumed.
		{name: "fault inside a string", src: `"ab"`, after: 2, wantIndex: 2, wantState: TokenizerStateString},
		{name: "fault inside an extended symbol", src: "|ab|", after: 2, wantIndex: 2, wantState: TokenizerStateSymbol},
		{name: "fault first in an extended symbol", src: "|ab|", after: 1, wantIndex: 1, wantState: TokenizerStateSymbol},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			rdr := &faultingRuneReader{rdr: strings.NewReader(tc.src), after: tc.after, fault: closed}
			err := drainTokens(NewTokenizer(rdr, false))
			c.Assert(errors.Is(err, io.EOF), qt.IsFalse, qt.Commentf("fault reported as end of input: %v", err))
			c.Check(errors.Is(err, werr.ErrPortClosed), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(errors.Is(err, NewTokenizerError(MessageReaderFailed)), qt.IsTrue, qt.Commentf("got: %v", err))
			var terr *TokenizerError
			c.Assert(errors.As(err, &terr), qt.IsTrue)
			at, located := terr.At()
			c.Check(located, qt.IsTrue)
			c.Check(at.Index(), qt.Equals, tc.wantIndex)
			c.Check(terr.State(), qt.Equals, tc.wantState)
			// No rune was read, so none may be reported: neither the U+FFFD
			// placeholder nor end of input.
			_, isChar := terr.Rune()
			c.Check(isChar, qt.IsFalse)
			c.Check(strings.Contains(err.Error(), "U+FFFD"), qt.IsFalse, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "<end of input>"), qt.IsFalse, qt.Commentf("got: %v", err))
			// The generic message is rendered with the cause that explains it.
			c.Check(strings.Contains(err.Error(), "string-input-port is closed"), qt.IsTrue, qt.Commentf("got: %v", err))
			// A named input renders through ErrorInFile, which must keep it too.
			inFile := terr.ErrorInFile("f.scm")
			c.Check(strings.Contains(inFile, "string-input-port is closed"), qt.IsTrue, qt.Commentf("got: %v", inFile))
		})
	}
}

// TestReaderWrappedEOFIsEndOfInput is the control for the above: a reader
// that wraps io.EOF has reached end of input, and is reported as the bare
// io.EOF the parser compares against.
func TestReaderWrappedEOFIsEndOfInput(t *testing.T) {
	c := qt.New(t)
	rdr := &faultingRuneReader{
		rdr:   strings.NewReader("ab"),
		after: 2,
		fault: werr.WrapForeignErrorf(io.EOF, "source exhausted"),
	}
	tok := NewTokenizer(rdr, false)
	_, err := tok.Next()
	c.Assert(err, qt.IsNil)
	_, err = tok.Next()
	c.Check(err, qt.Equals, io.EOF)
}
