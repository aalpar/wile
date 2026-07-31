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
	"io"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/aalpar/wile/pkg/syntax"
)

// Error messages returned by the tokenizer.
const (
	MessageRuneError                             = "rune error"
	MessageExpectingNumber                       = "expecting number"
	MessageExpectingDelimiterAfterNumber         = "expecting delimiter after radix-prefixed number"
	MessageExpectingExponentDigits               = "expecting exponent digits"
	MessageExpectingImaginary                    = "expecting imaginary"
	MessageExpectingDecimalFraction              = "expecting decimal fraction"
	MessageExpectingNan                          = "expecting NaN"
	MessageExpectingInf                          = "expecting Inf"
	MessageExpectingTrue                         = "expecting true"
	MessageExpectingFalse                        = "expecting false"
	MessageExpectingToken                        = "expecting token"
	MessageExpectingEscape                       = "expecting escape"
	MessageExpectingHexSequenceTerminator        = "expecting hex sequence terminator"
	MessageExpectingLineEnding                   = "expecting line ending"
	MessageExpectingHexDigit                     = "expecting hex digit"
	MessageExpectingCharacterMnemonicOrHexEscape = "expecting character mnemonic or hex escape"
	MessageExpectingDirective                    = "expecting directive"
	MessageCannotParseNumber                     = "cannot parse number"
	MessageCodePointExceedsUnicodeMaximum        = "character code point exceeds Unicode maximum (0x10FFFF)"
	MessageCodePointIsSurrogate                  = "character code point is a surrogate (0xD800-0xDFFF)"
	MessageInvalidHexEscape                      = "invalid hex escape"
	MessageInvalidCharacterHexEscape             = "invalid character hex escape"
	MessageInvalidCharacterMnemonic              = "invalid character mnemonic"
	MessageUnterminatedExtendedSymbol            = "unterminated extended symbol"
	MessageUnterminatedString                    = "unterminated string"
	MessageUnterminatedBlockComment              = "unterminated block comment"
	MessageExpectingByteVectorPrefix             = "expecting #u8( byte vector prefix"
)

// TokenizerState represents the type of token recognized by the tokenizer.
// Each state corresponds to a distinct lexical element in Scheme syntax.
type TokenizerState int

// TokenizerState values for different token types.
const (
	// TokenizerStateFailed indicates tokenization failed.
	TokenizerStateFailed TokenizerState = iota

	// TokenizerStateSyntax represents #'expr (syntax quote).
	TokenizerStateSyntax
	// TokenizerStateUnsyntax represents #,expr (unsyntax).
	TokenizerStateUnsyntax
	// TokenizerStateUnsyntaxSplicing represents #,@expr (unsyntax-splicing).
	TokenizerStateUnsyntaxSplicing
	// TokenizerStateQuasisyntax represents #`expr (quasisyntax).
	TokenizerStateQuasisyntax

	// TokenizerStateQuote represents 'expr (quote).
	TokenizerStateQuote
	// TokenizerStateUnquote represents ,expr (unquote).
	TokenizerStateUnquote
	// TokenizerStateUnquoteSplicing represents ,@expr (unquote-splicing).
	TokenizerStateUnquoteSplicing
	// TokenizerStateQuasiquote represents `expr (quasiquote).
	TokenizerStateQuasiquote

	// TokenizerStateSignedInf represents +inf.0 or -inf.0 (infinity).
	TokenizerStateSignedInf
	// TokenizerStateSignedNan represents +nan.0 or -nan.0 (not a number).
	TokenizerStateSignedNan
	// TokenizerStateSignedImaginaryInf represents +inf.0i or -inf.0i (imaginary infinity).
	TokenizerStateSignedImaginaryInf
	// TokenizerStateSignedImaginaryNan represents +nan.0i or -nan.0i (imaginary NaN).
	TokenizerStateSignedImaginaryNan
	// TokenizerStateSignedImaginary represents +i, -i, +3i, -3.5i (pure imaginary).
	TokenizerStateSignedImaginary
	// TokenizerStateSignedComplex represents +1+2i, 3.5-2.5i (rectangular complex).
	TokenizerStateSignedComplex
	// TokenizerStateSignedComplexPolar represents +1@1.5708 (polar complex: magnitude@angle).
	TokenizerStateSignedComplexPolar
	// TokenizerStateUnsignedImaginaryInf represents inf.0i (unsigned imaginary infinity).
	TokenizerStateUnsignedImaginaryInf
	// TokenizerStateUnsignedImaginaryNan represents nan.0i (unsigned imaginary NaN).
	TokenizerStateUnsignedImaginaryNan
	// TokenizerStateUnsignedImaginary represents 3i, 3.5i (unsigned pure imaginary).
	TokenizerStateUnsignedImaginary
	// TokenizerStateUnsignedComplex represents 1+2i (unsigned rectangular complex).
	TokenizerStateUnsignedComplex
	// TokenizerStateUnsignedComplexPolar represents 1@1.5708 (unsigned polar complex).
	TokenizerStateUnsignedComplexPolar

	// TokenizerStateMarker represents a generic # marker.
	TokenizerStateMarker
	// TokenizerStateMarkerBooleanFalse represents #f or #false.
	TokenizerStateMarkerBooleanFalse
	// TokenizerStateMarkerBooleanTrue represents #t or #true.
	TokenizerStateMarkerBooleanTrue
	// TokenizerStateMarkerNumberInexact represents #i prefix (inexact).
	TokenizerStateMarkerNumberInexact
	// TokenizerStateMarkerNumberExact represents #e prefix (exact).
	TokenizerStateMarkerNumberExact

	// TokenizerStateSignedInteger represents a signed integer in any base
	// (-123, +456, #x-FF, #b+101). The base is carried on the token via Radix().
	TokenizerStateSignedInteger
	// TokenizerStateUnsignedInteger represents an unsigned integer in any base
	// (123, #xFF, #b101). The base is carried on the token via Radix().
	TokenizerStateUnsignedInteger

	// TokenizerStateBigFloat represents #m arbitrary-precision decimal.
	TokenizerStateBigFloat
	// TokenizerStateBigInteger represents #z arbitrary-precision integer (decimal).
	TokenizerStateBigInteger

	// TokenizerStateMarkerBase2 represents #b prefix (binary).
	TokenizerStateMarkerBase2
	// TokenizerStateMarkerBase8 represents #o prefix (octal).
	TokenizerStateMarkerBase8
	// TokenizerStateMarkerBase10 represents #d prefix (decimal).
	TokenizerStateMarkerBase10
	// TokenizerStateMarkerBase16 represents #x prefix (hexadecimal).
	TokenizerStateMarkerBase16

	// TokenizerStateSignedDecimalFraction represents -1.23 or +4.56.
	TokenizerStateSignedDecimalFraction
	// TokenizerStateSignedRationalFraction represents -1/2 or +3/4.
	TokenizerStateSignedRationalFraction
	// TokenizerStateUnsignedRationalFraction represents 1/2 or 3/4.
	TokenizerStateUnsignedRationalFraction
	// TokenizerStateUnsignedDecimalFraction represents 1.23 or 4.56.
	TokenizerStateUnsignedDecimalFraction

	// TokenizerStateSignedScientificNotation represents integers with exponents like +1e10, -2e-5.
	// Parser determines if result is integer or float based on exponent sign and mantissa.
	TokenizerStateSignedScientificNotation
	// TokenizerStateUnsignedScientificNotation represents integers with exponents like 1e10, 2e-5.
	// Parser determines if result is integer or float based on exponent sign and mantissa.
	TokenizerStateUnsignedScientificNotation

	// TokenizerStateEmptyList represents () (empty list).
	TokenizerStateEmptyList
	// TokenizerStateOpenParen represents ( (open parenthesis).
	TokenizerStateOpenParen
	// TokenizerStateCloseParen represents ) (close parenthesis).
	TokenizerStateCloseParen
	// TokenizerStateOpenBracket represents [ (open square bracket).
	// R7RS §2.1: Square brackets are equivalent to parentheses but must match.
	TokenizerStateOpenBracket
	// TokenizerStateCloseBracket represents ] (close square bracket).
	// R7RS §2.1: Square brackets are equivalent to parentheses but must match.
	TokenizerStateCloseBracket
	// TokenizerStateCons represents . (dot for improper lists).
	TokenizerStateCons

	// TokenizerStateString represents complete "string".
	TokenizerStateString

	// TokenizerStateCharMnemonicOrHexEscape represents intermediate character state.
	TokenizerStateCharMnemonicOrHexEscape
	// TokenizerStateCharMnemonic represents #\newline, #\space, etc.
	TokenizerStateCharMnemonic
	// TokenizerStateCharHexEscape represents #\x0A (hex escape).
	TokenizerStateCharHexEscape
	// TokenizerStateCharGraphic represents #\a (single graphic char).
	TokenizerStateCharGraphic

	// TokenizerStateLineCommentBody represents comment text.
	TokenizerStateLineCommentBody
	// TokenizerStateBlockCommentBody represents block content.
	TokenizerStateBlockCommentBody
	// TokenizerStateDatumCommentBegin represents the #; marker; the following
	// datum is read by the parser.
	TokenizerStateDatumCommentBegin

	// TokenizerStateSymbol represents an identifier or symbol.
	TokenizerStateSymbol

	// TokenizerStateOpenVector represents #( (vector).
	TokenizerStateOpenVector
	// TokenizerStateOpenVectorUnsignedByteMarker represents #u8( (bytevector).
	TokenizerStateOpenVectorUnsignedByteMarker

	// TokenizerStateDirective represents #!fold-case, etc.
	TokenizerStateDirective
	// TokenizerStateLabelReference represents #123# (datum label reference).
	TokenizerStateLabelReference
	// TokenizerStateLabelAssignment represents #123= (datum label assignment).
	TokenizerStateLabelAssignment
)

// Tokenizer reads Scheme source code and produces a stream of tokens.
type Tokenizer struct {
	rdr        io.RuneReader
	cur        rune
	err        error
	runeEnd    syntax.SourceIndexes // end of the current rune
	runeStart  syntax.SourceIndexes // start of the current rune
	tokenStart syntax.SourceIndexes
	tokenEnd   syntax.SourceIndexes // end of the current token
	// used to build up token "value", which may differ from the raw text.
	// Byte slices, not strings: a string += per rune is O(n^2) in the token
	// length, so a single large datum (a 400KB symbol on an untrusted port)
	// costs quadratic time. Appending into a reused buffer is linear.
	value []byte
	// raw source code text of the current token
	text []byte
	// state describes the type of the current token
	state TokenizerState
	// signed indicates whether the current number token is signed (has + or -)
	signed bool
	// radix indicates the current number radix (base) while scanning; 0 = default
	// decimal, 2/8/10/16 = explicit prefix. Reset to 0 after each number.
	radix int
	// tokRadix is the effective parse base recorded on the emitted integer token
	// (2/8/10/16); 0 for tokens where base is not meaningful. Reset per token in mark().
	tokRadix   int
	blockDepth int  // nesting depth for block comments
	ci         bool // case insensitive symbol mode
	hashDigit  bool // R7RS §7.1.1: whether # appeared as inexact digit placeholder
}

// Tokenize returns all tokens read from s, and the terminating error
// (typically io.EOF on success). The ci flag enables case-insensitive
// tokenization.
func Tokenize(s string, ci bool) ([]Token, error) {
	q := make([]Token, 0)
	tkns := NewTokenizer(strings.NewReader(s), ci)
	t, err := tkns.Next()
	for err == nil {
		q = append(q, t)
		t, err = tkns.Next()
	}
	return q, err
}

// NewTokenizer creates a new tokenizer that reads from the given RuneReader.
// The tokenizer is initialized with the first rune already read.
func NewTokenizer(rdr io.RuneReader, ci bool) *Tokenizer {
	// Seed the position at line 1. SourceIndexes documents line as 1-based
	// (index and column are 0-based); the struct zero value would start line
	// at 0, making every reported source location one line too low. Seeding
	// runeEnd is what matters — readNextRune copies it into runeStart and the
	// first token's positions derive from there — but all four are set so the
	// invariant is local and obvious.
	start := syntax.NewSourceIndexes(0, 0, 1)
	q := &Tokenizer{
		rdr:        rdr,
		ci:         ci,
		radix:      0, // 0 means default decimal; 10 means explicit #d prefix
		runeStart:  start,
		runeEnd:    start,
		tokenStart: start,
		tokenEnd:   start,
	}
	q.readNextRune()
	return q
}

// Text returns the text of the current token.
func (p *Tokenizer) Text() string {
	return string(p.text)
}

// Next returns the next token from the input stream.
// Returns io.EOF when the input is exhausted. End of input is carried by p.err
// (readNextRune sets io.EOF), never by p.cur: utf8.RuneError is both the decode
// sentinel and a writable character, so testing the rune reported a literal
// U+FFFD as EOF and silently discarded the rest of the input.
// Comment tokens are always emitted; callers that want them elided (the parser,
// when constructed with skipComments) drop them themselves.
func (p *Tokenizer) Next() (Token, error) {
	for p.err == nil {
		p.skipWhitespace() //nolint:errcheck
		if p.err != nil {
			return nil, p.err
		}
		// start new token
		p.mark()
		p.read()
		src := string(p.text)
		val := string(p.value)
		// here p.err may be != nil due to read failure.  will be returned on next call to Next
		q := NewSimpleToken(p.state, src, val, &p.tokenStart, &p.tokenEnd, p.hashDigit, p.tokRadix)
		return q, nil //nolint:staticcheck
	}
	return nil, p.err
}

// Reader returns the underlying RuneReader.
func (p *Tokenizer) Reader() io.RuneReader {
	return p.rdr
}

// read dispatches token reading based on the current character.
// This is the main tokenization switch that handles all token types.
func (p *Tokenizer) read() {
	switch {
	case p.curr() == '(':
		p.state = TokenizerStateOpenParen
		p.next()
		if p.curr() == ')' {
			p.state = TokenizerStateEmptyList
			p.next()
		}
		p.term()
		return
	case p.curr() == ')':
		p.state = TokenizerStateCloseParen
		p.next()
		p.term()
		return
	case p.curr() == '[':
		// R7RS §2.1: [ and ] are equivalent to ( and ) but must match
		p.state = TokenizerStateOpenBracket
		p.next()
		if p.curr() == ']' {
			p.state = TokenizerStateEmptyList
			p.next()
		}
		p.term()
		return
	case p.curr() == ']':
		p.state = TokenizerStateCloseBracket
		p.next()
		p.term()
		return
	case isDot(p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(10) //nolint:errcheck
		p.term()
		return
	case isQuote(p.curr()):
		p.state = TokenizerStateQuote
		p.next()
		p.term()
		return
	case p.curr() == ',':
		p.state = TokenizerStateUnquote
		p.next()
		if p.err != nil || p.curr() != '@' {
			p.term()
			return
		}
		p.state = TokenizerStateUnquoteSplicing
		p.next()
		p.term()
		return
	case p.curr() == '`': // quasiquote
		p.state = TokenizerStateQuasiquote
		p.next()
		p.term()
		return
	case p.curr() == '"': // string
		p.state = TokenizerStateFailed
		p.next() // skip "
		if p.err != nil {
			p.term()
			return
		}
		p.readString() //nolint:errcheck
		p.term()
		return
	case p.curr() == ';': // line comment
		// A line comment is one token: body text through the line ending.
		p.state = TokenizerStateLineCommentBody
		p.readLineCommentOrPragma() //nolint:errcheck
		p.term()
		p.value = append(p.value[:0], p.text...)
		return
	case isVerticalLine(p.curr()): // '|'
		// skip vertical lines outside symbols
		p.next()
		p.state = TokenizerStateSymbol
		if p.err != nil {
			p.term()
			return
		}
		p.readExtendedSymbol() //nolint:errcheck
		p.term()
		return
	case isMarker(p.curr()): // '#'
		p.next()
		if p.err != nil {
			p.term()
			return
		}
		p.readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment() //nolint:errcheck
		p.term()
		return
	case isExplicitSign(p.curr()), isDigit(p.radix, p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(p.radix) //nolint:errcheck
		p.requireDelimiterAfterRadixNumeral(p.radix)
		p.radix = 0 // Reset radix after parsing number
		p.term()
		return
	case isInitial(p.curr()): // read symbol
		p.state = TokenizerStateSymbol
		p.value = utf8.AppendRune(p.value, p.curr())
		p.next()
		if p.err != nil {
			p.term()
			return
		}
		p.readSymbol() //nolint:errcheck
		p.term()
		return
	default:
		p.term()
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingToken)
		return
	}
}

// skipWhitespace consumes whitespace characters (spaces, tabs, newlines).
func (p *Tokenizer) skipWhitespace() {
	for p.err == nil && (isIntralineWhitespace(p.curr()) || isLineEnding(p.curr())) {
		p.next()
	}
}

// scanWith matches bytes using the provided comparison function.
// Returns len(s) minus the number of runes matched (0 = complete match); all
// call sites pass ASCII, where that is the count of unmatched bytes.
func (p *Tokenizer) scanWith(s []byte, match func(input, target rune) bool) int {
	k := len(s) // k = len(s) less the runes matched so far
	i := 0      // number of bytes consumed
	for {
		r, n := utf8.DecodeRune(s[i:])
		if n == 0 && r == utf8.RuneError {
			return k
		}
		i += n
		if !match(p.curr(), r) {
			return k
		}
		k--
		p.next()
		if p.err != nil {
			return k
		}
	}
}

// scan matches bytes using either case-sensitive or case-insensitive matching.
// Returns len(s) minus the number of runes matched (0 = complete match); all
// call sites pass ASCII, where that is the count of unmatched bytes.
func (p *Tokenizer) scan(s []byte) int {
	if p.ci {
		// Case-insensitive mode: accept if either case matches
		return p.scanCaseInsensitive(s)
	}
	return p.scanWith(s, func(input, target rune) bool { return input == target })
}

// scanCaseInsensitive matches bytes case-insensitively (always, regardless of p.ci).
// Used for R7RS-required case-insensitive tokens like booleans.
// Returns len(s) minus the number of runes matched (0 = complete match); all
// call sites pass ASCII, where that is the count of unmatched bytes.
func (p *Tokenizer) scanCaseInsensitive(s []byte) int {
	return p.scanWith(s, func(input, target rune) bool {
		return unicode.ToLower(input) == unicode.ToLower(target)
	})
}

// scanLineEnding consumes a line ending (\n, \r, or \r\n).
// Returns true if a line ending was consumed.
func (p *Tokenizer) scanLineEnding() bool {
	switch p.curr() {
	case '\n':
		p.next()
		return true
	case '\r':
		p.next() // consume '\r'
		if p.err != nil {
			return true
		}
		// check for '\n' following '\r'
		if p.curr() != '\n' {
			return true
		}
		p.next() // consume '\n'
		return true
	}
	return false
}

// curr returns the current rune being examined.
func (p *Tokenizer) curr() rune {
	return p.cur
}

// mark marks the current position as the beginning of a new token.
func (p *Tokenizer) mark() {
	p.value = p.value[:0]
	p.text = p.text[:0]
	p.tokenStart = p.runeStart
	p.tokenEnd = p.runeStart
	p.signed = false
	p.hashDigit = false
	p.tokRadix = 0
	// Every per-token field is reset here, state included: a scanner path that
	// bails out before assigning one (readBoolean on a lexical error) would
	// otherwise emit this token under the PREVIOUS token's type.
	p.state = TokenizerStateFailed
}

// term terminates the current token, setting its end position.
func (p *Tokenizer) term() {
	p.tokenEnd = p.runeStart
}

// readNextRune reads the next rune from the reader and updates position tracking.
// Sets p.cur to utf8.RuneError and p.err appropriately on EOF or encoding error.
func (p *Tokenizer) readNextRune() {
	n := 0
	p.cur, n, p.err = p.rdr.ReadRune()
	p.runeStart = p.runeEnd
	if n == 0 {
		p.cur = utf8.RuneError
		p.err = io.EOF
		return
	}
	// Index counts bytes; column counts characters. A byte-valued column would
	// only restate the index and would misplace a caret in non-ASCII source.
	p.runeEnd = syntax.NewSourceIndexes(p.runeEnd.Index()+n, p.runeEnd.Column()+1, p.runeEnd.Line())
	// A decoding failure is reported as (RuneError, 1); a genuine U+FFFD in the
	// source decodes to (RuneError, 3). Only the one-byte form is an error —
	// conflating them makes a written U+FFFD unreadable.
	if p.cur == utf8.RuneError && n == 1 {
		p.err = NewTokenizerError(MessageRuneError)
		return
	}
	// Line and tab-stop adjustments belong here, not in next(): the constructor
	// reads the first rune through this path, and a source starting with a tab
	// or newline must be tracked like any other.
	if isNewLine(p.cur) {
		p.runeEnd.NewLine()
		return
	}
	if p.cur == '\t' {
		p.runeEnd = tabStop(p.runeEnd)
	}
}

// tabStop advances si's column to the next 8-column tab stop. si's column was
// already stepped one past the tab's own column by readNextRune, so the stop is
// computed from the tab's column (column-1). SourceIndexes.Tab() adds a whole
// stop on top of that step instead, over-advancing by 8 whenever the tab sits on
// a column congruent to 7 (mod 8).
func tabStop(si syntax.SourceIndexes) syntax.SourceIndexes {
	col := si.Column() - 1
	return syntax.NewSourceIndexes(si.Index(), col+8-col%8, si.Line())
}

// next advances to the next rune, appending the current rune to scratch.
// Updates position tracking and handles line endings and EOF.
func (p *Tokenizer) next() {
	p.text = utf8.AppendRune(p.text, p.cur)
	p.readNextRune()
}

// Close closes the underlying reader if it implements io.Closer.
func (p *Tokenizer) Close() error {
	var err error
	cls, ok := p.rdr.(io.Closer)
	if ok {
		err = cls.Close()
	}
	p.rdr = nil
	return err
}
