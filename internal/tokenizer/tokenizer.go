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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

// Error messages returned by the tokenizer.
const (
	MessageRuneError                             = "rune error"
	MessageExpectingNumber                       = "expecting number"
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
)

// ErrNotAnUnsignedByteMarker is returned when parsing fails on an unsigned byte marker.
var (
	ErrNotAnUnsignedByteMarker = werr.NewStaticError("not an unsigned byte marker")
	ErrNotALiteral             = werr.NewStaticError("not a literal")
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

	// TokenizerStateSignedInteger represents -123 or +456 (signed decimal).
	TokenizerStateSignedInteger
	// TokenizerStateUnsignedInteger represents 123 (unsigned decimal).
	TokenizerStateUnsignedInteger

	// TokenizerStateSignedIntegerBase2 represents signed binary integer after #b prefix.
	TokenizerStateSignedIntegerBase2
	// TokenizerStateUnsignedIntegerBase2 represents unsigned binary integer after #b prefix.
	TokenizerStateUnsignedIntegerBase2
	// TokenizerStateSignedIntegerBase8 represents signed octal integer after #o prefix.
	TokenizerStateSignedIntegerBase8
	// TokenizerStateUnsignedIntegerBase8 represents unsigned octal integer after #o prefix.
	TokenizerStateUnsignedIntegerBase8
	// TokenizerStateSignedIntegerBase10 represents signed decimal integer after #d prefix.
	TokenizerStateSignedIntegerBase10
	// TokenizerStateUnsignedIntegerBase10 represents unsigned decimal integer after #d prefix.
	TokenizerStateUnsignedIntegerBase10
	// TokenizerStateSignedIntegerBase16 represents signed hexadecimal integer after #x prefix.
	TokenizerStateSignedIntegerBase16
	// TokenizerStateUnsignedIntegerBase16 represents unsigned hexadecimal integer after #x prefix.
	TokenizerStateUnsignedIntegerBase16

	// TokenizerStateBigFloat represents #m arbitrary-precision decimal.
	TokenizerStateBigFloat
	TokenizerStateBigIntegerDefaultBase // represents #z arbitrary-precision integer (default base).
	TokenizerStateBigIntegerBase2       // represents #b arbitrary-precision binary.
	TokenizerStateBigIntegerBase8       // represents #o arbitrary-precision octal.
	TokenizerStateBigIntegerBase10      // represents #d arbitrary-precision decimal.
	TokenizerStateBigIntegerBase16      // represents #x arbitrary-precision hexadecimal.

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

	// TokenizerStateStringStart represents opening " (string start).
	TokenizerStateStringStart
	// TokenizerStateStringSpan represents string content.
	TokenizerStateStringSpan
	// TokenizerStateStringIntraEscape represents escape sequence within string.
	TokenizerStateStringIntraEscape
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

	// TokenizerStateLineCommentBody represents comment text (multi-token: body).
	TokenizerStateLineCommentBody
	// TokenizerStateBlockCommentBody represents block content (multi-token: body).
	TokenizerStateBlockCommentBody
	// TokenizerStateDatumCommentBegin represents #; (multi-token mode).
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
	runeEnd    syntax.SourceIndexes // emd of the current rune
	runeStart  syntax.SourceIndexes // start of the current rune tokenStartIndex  int // start of the current token
	tokenStart syntax.SourceIndexes
	tokenEnd   syntax.SourceIndexes // end of the current token
	// used to build up token "value", which may differ from the raw text
	value string
	// raw source code text of the current token
	text string
	// state describes the type of the current token
	state TokenizerState
	// signed indicates whether the current number token is signed (has + or -)
	signed bool
	// radix indicates the current number radix (base)
	radix      int
	blockDepth int  // nesting depth for block comments
	ci         bool // case insensitive symbol mode
	hashDigit  bool // R7RS §7.1.1: whether # appeared as inexact digit placeholder
}

// integerStateForRadix returns the appropriate integer token state based on the
// current radix and whether the number is signed.
// radix 0 means "default decimal" (plain numbers), radix 10 means "explicit #d prefix".
// Returns all tokens and any error (typically io.EOF on success).
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
	q := &Tokenizer{
		rdr:   rdr,
		ci:    ci,
		radix: 0, // 0 means default decimal; 10 means explicit #d prefix
	}
	q.readNextRune()
	return q
}

// NewTokenizerWithComments creates a tokenizer with optional comment token emission.
// When emitComments is true, comments are returned as Begin/Body/End token sequences
// instead of being skipped.
func NewTokenizerWithComments(rdr io.RuneReader, ci bool) *Tokenizer {
	q := NewTokenizer(rdr, ci)
	return q
}

// Text returns the text of the current token.
func (p *Tokenizer) Text() string {
	return p.text
}

// Next returns the next token from the input stream.
// Returns io.EOF when the input is exhausted.
// Comment tokens are skipped unless emitComments was set to true.
func (p *Tokenizer) Next() (Token, error) {
	for p.err == nil {
		// Handle comment phase continuation BEFORE EOF check
		// This allows emitting End tokens even when at EOF
		if p.curr() == utf8.RuneError {
			if p.err != nil {
				return nil, p.err
			}
			return nil, io.EOF
		}

		p.skipWhitespace() //nolint:errcheck
		if p.err != nil {
			return nil, p.err
		}
		// start new token
		p.mark()
		p.read()
		src := p.text
		val := p.value
		// here p.err may be != nil due to read failure.  will be returned on next call to Next
		q := NewSimpleToken(p.state, src, val, &p.tokenStart, &p.tokenEnd, p.hashDigit)
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
		// Emit LineCommentBegin, set up for Body/End on subsequent calls
		p.state = TokenizerStateLineCommentBody
		p.readLineCommentOrPragma() //nolint:errcheck
		p.term()
		p.value = p.text
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
	case isExplicitSign(p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(p.radix) //nolint:errcheck
		p.radix = 0                                                                      // Reset radix after parsing number
		p.term()
		return
	case isDigit(p.radix, p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(p.radix) //nolint:errcheck
		p.radix = 0                                                                      // Reset radix after parsing number
		p.term()
		return
	case isInitial(p.curr()): // read symbol
		p.state = TokenizerStateSymbol
		p.value += string(p.curr())
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
// Returns the number of unmatched bytes (0 = complete match).
func (p *Tokenizer) scanWith(s []byte, match func(input, target rune) bool) int {
	k := len(s) // k = number of runes left to match
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

// scan matches bytes using either case-sensitive or case-insensitive matching
func (p *Tokenizer) scan(s []byte) int {
	if p.ci {
		// Case-insensitive mode: accept if either case matches
		return p.scanCaseInsensitive(s)
	}
	return p.scanWith(s, func(input, target rune) bool { return input == target })
}

// scanCaseInsensitive matches bytes case-insensitively (always, regardless of p.ci).
// Used for R7RS-required case-insensitive tokens like booleans.
// Returns the number of unmatched bytes (0 = complete match).
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

// reset resets the tokenizer state after an error or token completion.
func (p *Tokenizer) reset() {
	p.runeStart = p.tokenEnd
	p.runeEnd = p.tokenEnd
	p.tokenStart = p.tokenEnd // set the next token to start at tokenEndIndex
	p.err = nil
}

// mark marks the current position as the beginning of a new token.
func (p *Tokenizer) mark() {
	p.value = p.value[:0]
	p.text = p.text[:0]
	p.tokenStart = p.runeStart
	p.tokenEnd = p.runeStart
	p.signed = false
	p.hashDigit = false
}

// term terminates the current token, setting its end position and text.
func (p *Tokenizer) term() {
	p.tokenEnd = p.runeStart
}

// readNextRune reads the next rune from the reader and updates position tracking.
// Sets p.cur to utf8.RuneError and p.err appropriately on EOF or encoding error.
func (p *Tokenizer) readNextRune() {
	n := 0
	p.cur, n, p.err = p.rdr.ReadRune()
	p.runeStart = p.runeEnd
	p.runeEnd.Inc(n)
	if n == 0 {
		p.cur = utf8.RuneError
		p.err = io.EOF
	} else if p.cur == utf8.RuneError {
		p.err = NewTokenizerError(MessageRuneError)
	}
}

// next advances to the next rune, appending the current rune to scratch.
// Updates position tracking and handles line endings and EOF.
func (p *Tokenizer) next() {
	p.text += string(p.cur)
	p.readNextRune()
	if isNewLine(p.cur) {
		p.runeEnd.NewLine()
	}
	if p.cur == '\t' {
		p.runeEnd.Tab()
	}
}

// isUnicodeLetter returns true if c is an ASCII letter (A-Z or a-z) or unicode number-letter

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
