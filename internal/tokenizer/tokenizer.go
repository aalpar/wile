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

// Package tokenizer implements lexical analysis for Scheme source code.
//
// The tokenizer converts a stream of runes into tokens representing the
// lexical elements of Scheme: parentheses, quotes, numbers, symbols, strings,
// characters, booleans, vectors, and comments.
//
// # Token Types
//
// Tokens are categorized by TokenizerState values:
//   - Delimiters: OpenParen, CloseParen, EmptyList, Cons
//   - Quotation: Quote, Quasiquote, Unquote, UnquoteSplicing, Syntax variants
//   - Numbers: SignedInteger, UnsignedInteger, DecimalFraction, RationalFraction
//   - Special numbers: SignedInf, SignedNan, SignedImaginary
//   - Radix prefixes: MarkerBase2, MarkerBase8, MarkerBase10, MarkerBase16
//   - Literals: Sym, String, Character (graphic, mnemonic, hex escape)
//   - Booleans: MarkerBooleanTrue, MarkerBooleanFalse
//   - Comments: LineComment, BlockComment, DatumComment
//   - Vectors: OpenVector, OpenVectorUnsignedByteMarker
//   - Labels: LabelReference, LabelAssignment
//
// # Usage
//
//	tok := tokenizer.NewTokenizer(strings.NewReader("(+ 1 2)"))
//	for {
//	    token, err := tok.Next()
//	    if err == io.EOF {
//	        break
//	    }
//	    // process token
//	}
package tokenizer

import (
	"errors"
	"io"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// Error messages returned by the tokenizer.
const (
	MessageRuneError                             = "rune error"
	MessageExpectingNumber                       = "expecting number"
	MessageExpectingExponentMarker               = "expecting exponent marker"
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
	MessageInvalidHexEscape                      = "character code point is a surrogate (0xD800-0xDFFF)"
	MessageInvalidCharacterHexEscape             = "invalid character hex escape"
	MessageInvalidCharacterMnemonic              = "invalid character mnemonic"
	MessageUnterminatedExtendedSymbol            = "unterminated extended symbol"
	MessageUnterminatedString                    = "unterminated string"
)

// ErrNotAnUnsignedByteMarker is returned when parsing fails on an unsigned byte marker.
var (
	ErrNotAnUnsignedByteMarker = values.NewStaticError("not an unsigned byte marker")
	ErrNotALiteral             = values.NewStaticError("not a literal")
)

var digs [128]int

func init() {
	for i := 0; i < len(digs); i++ {
		digs[i] = -1
	}
	for i := '0'; i < '9'; i++ {
		digs['0'] = int(i - '0')
	}
	for i := 'a'; i < 'f'; i++ {
		digs[i] = int(i-'a') + 10
	}
	for i := 'A'; i < 'F'; i++ {
		digs[i] = int(i-'A') + 10
	}
}

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
	scratch    []rune
	// used to build up token "value", which may differ from the raw text
	value string
	// raw source code text of the current token
	text string
	// state describes the type of the current token
	state TokenizerState
	// signed indicates whether the current number token is signed (has + or -)
	signed bool
	// radix indicates the current number radix (base)
	radix int
	// strength indicates the number of bits in the floating point representation
	strength   int
	blockDepth int  // nesting depth for block comments
	ci         bool // case insensitive symbol mode
	hashDigit  bool // R7RS §7.1.1: whether # appeared as inexact digit placeholder
}

// integerStateForRadix returns the appropriate integer token state based on the
// current radix and whether the number is signed.
// radix 0 means "default decimal" (plain numbers), radix 10 means "explicit #d prefix".
func (p *Tokenizer) integerStateForRadix(signed bool) TokenizerState {
	switch p.radix {
	case 2:
		if signed {
			return TokenizerStateSignedIntegerBase2
		}
		return TokenizerStateUnsignedIntegerBase2
	case 8:
		if signed {
			return TokenizerStateSignedIntegerBase8
		}
		return TokenizerStateUnsignedIntegerBase8
	case 10:
		// Explicit #d prefix
		if signed {
			return TokenizerStateSignedIntegerBase10
		}
		return TokenizerStateUnsignedIntegerBase10
	case 16:
		if signed {
			return TokenizerStateSignedIntegerBase16
		}
		return TokenizerStateUnsignedIntegerBase16
	default: // 0 or unset (default decimal)
		if signed {
			return TokenizerStateSignedInteger
		}
		return TokenizerStateUnsignedInteger
	}
}

// Tokenize is a convenience function that tokenizes a complete string.
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
		q := NewSimpleToken(p.state, src, val, &p.tokenStart, &p.tokenEnd, p.signed, p.radix, p.hashDigit)
		return q, nil //nolint:staticcheck
	}
	return nil, p.err
}

// continueBlockComment handles Body and End phases for block comments
func (p *Tokenizer) continueBlockComment() Token {
	p.state = TokenizerStateBlockCommentBody
	// Read content until we find |# at depth 0 or EOF
	for p.err == nil {
		switch {
		case isMarker(p.curr()):
			p.next()
			if p.err != nil {
				break
			}
			if isVerticalLine(p.curr()) {
				// Nested #|
				p.blockDepth++
				p.next()
			}

		case isVerticalLine(p.curr()):
			p.next()
			if p.err != nil {
				break
			}
			if isMarker(p.curr()) { // Found |#
				p.next()
				if p.err != nil {
					break
				}
				if p.blockDepth == 0 {
					// Found closing |# - remove the | from scratch and stop
					if len(p.value) > 0 {
						p.value = p.value[:len(p.value)-1]
					}
					p.term()
					return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.signed, p.radix, false)
				}
				p.blockDepth--
			}

		default:
			p.next()
		}
	}
	// EOF before closing - emit Body token, no End will follow
	p.term()
	return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.signed, p.radix, false)
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
		// set state
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
	case isSymbolInitial(p.curr()): // read symbol
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
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingToken, p.tokenStart, p.tokenEnd)
		return
	}
}

func (p *Tokenizer) readHexEscapeToken() {
	p.next()
	if p.err != nil {
		return
	}
	x, n := p.readUnsignedBaseNInteger(16, 8)
	if p.err != nil {
		return
	}
	if n == 0 {
		p.err = NewTokenizerError(MessageExpectingHexDigit, p.tokenStart, p.tokenEnd)
		return
	}
	if p.cur != ';' {
		p.err = NewTokenizerError(MessageExpectingHexSequenceTerminator, p.tokenStart, p.tokenEnd)
		return
	}
	p.next()
	if p.err != nil {
		return
	}
	p.value += string(rune(x))
}

// readEscapeSequence handles escape sequences within strings and extended tokens.
// R7RS §6.7 and §7.1.1: Both \" and \| are valid in strings and extended tokens.
// Recognizes: \a, \b, \t, \n, \r, \\, \", \|, \xNN; (hex escape), and line continuations.
func (p *Tokenizer) readEscapeSequence() {
	if p.curr() == 'x' {
		p.readHexEscapeToken()
		return
	}
	switch {
	case isIntralineWhitespace(p.curr()) || isLineEnding(p.curr()):
		p.skipLineContinuation()
		return
	case p.curr() == 'a': // \a (alarm bell)
		p.value += "\a"
	case p.curr() == 'b': // \b (backspace)
		p.value += "\b"
	case p.curr() == 't': // \t (tab)
		p.value += "\t"
	case p.curr() == 'n': // \n (newline)
		p.value += "\n"
	case p.curr() == 'r': // \r (return)
		p.value += "\r"
	case isBackSlash(p.curr()): // \\ (back slash)
		p.value += "\\"
	case p.curr() == '"': // \" (double quote) - R7RS §6.7
		p.value += "\""
	case p.curr() == '|': // \| (vertical bar) - R7RS §6.7
		p.value += "|"
	default:
		p.err = NewTokenizerError(MessageExpectingEscape, p.tokenStart, p.tokenEnd)
		return
	}
	p.next()
}

func (p *Tokenizer) readIntraExtendedToken() {
	p.readEscapeSequence()
}

// readIntraStringEscape handles escape sequences within strings.
func (p *Tokenizer) readIntraStringEscape() {
	p.readEscapeSequence()
}

// readString reads a string literal until the closing double-quote.
// Handles escape sequences via readIntraStringEscape.
// Builds the processed string content in p.value (without quotes, with escapes converted).
func (p *Tokenizer) readString() {
	p.value = "" // Reset processed value for this string
	for p.curr() != '"' {
		if isBackSlash(p.curr()) {
			p.next() // skip \
			if p.err != nil {
				if errors.Is(p.err, io.EOF) {
					p.err = NewTokenizerError(MessageUnterminatedString, p.tokenStart, p.tokenEnd)
				}
				return
			}
			p.readIntraStringEscape() //nolint:errcheck
			if p.err != nil {
				return
			}
			continue
		}
		// Regular character - add to processed value
		p.value += string(p.curr())
		p.next()
		if p.err != nil {
			if errors.Is(p.err, io.EOF) {
				p.err = NewTokenizerError(MessageUnterminatedString, p.tokenStart, p.tokenEnd)
			}
			return
		}
	}
	p.next() // skip "
	p.state = TokenizerStateString
}

// skipWhitespace consumes whitespace characters (spaces, tabs, newlines).
func (p *Tokenizer) skipWhitespace() {
	for p.err == nil && (isIntralineWhitespace(p.curr()) || isLineEnding(p.curr())) {
		p.next()
	}
}

// skipLineContinuation consumes all characters until the line ending.
func (p *Tokenizer) skipLineContinuation() {
	// skip intra-line whitespace
	for p.err == nil && isIntralineWhitespace(p.curr()) {
		p.next()
	}
	if p.err != nil {
		return
	}
	// consume line ending
	if !p.scanLineEnding() {
		p.err = NewTokenizerError(MessageExpectingLineEnding, p.tokenStart, p.tokenEnd)
		return
	}
	// scanLineEnding() already advanced past the line ending
	// skip intra-line whitespace after line ending
	for p.err == nil && isIntralineWhitespace(p.curr()) {
		p.next()
	}
}

// readLineCommentOrPragma reads a line comment starting with one or more semicolons.
// Consumes all characters until (but not including) the line ending.
func (p *Tokenizer) readLineCommentOrPragma() {
	for p.err == nil && p.curr() == ';' {
		p.next()
	}
	for p.err == nil && !isLineEnding(p.curr()) {
		p.next()
	}
}

// readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment handles # prefixed tokens.
// Called after '#' is consumed. Dispatches based on the next character to parse:
//   - #' #` #, #,@ : syntax quotation
//   - #| : block comment
//   - #t #f : booleans
//   - #b #o #d #x : radix prefixes
//   - #i #e : exactness markers
//   - #u8( : byte vector
//   - #\ : character literal
//   - #( : vector
//   - #; : datum comment
//   - #! : directive
//   - #N= #N# : datum labels
func (p *Tokenizer) readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment() {
	switch {
	case isQuote(p.curr()): // #' (syntax)
		p.state = TokenizerStateSyntax
		p.next()
		return

	case p.curr() == ',': // #' (unsyntax) (unsyntax-splice)
		p.next()
		if p.err != nil || p.curr() != '@' {
			p.state = TokenizerStateUnsyntax
			// unsyntax
			return
		}
		p.state = TokenizerStateUnsyntaxSplicing
		// unsyntax-splice
		p.next()
		return

	case p.curr() == '`': // #' (quasisyntax)
		p.state = TokenizerStateQuasisyntax
		p.next()
		return

	case isVerticalLine(p.curr()):
		// Emit BlockCommentBegin, set up for Body/End on subsequent calls
		p.next() // skip '|'
		p.blockDepth = 0
		p.continueBlockComment()
		return

	case isUnicodeLetter(p.curr()): // #u8(, #true, #false ... unsigned byte array
		p.readTypedArrayOrExactnessOrRadixOrBooleanMarker()
		return

	case isDigit(10, p.curr()):
		// label: #[0-9]+# #[0-9]+[=]value#
		p.state = TokenizerStateLabelReference
		p.next()
		for p.err == nil && isDigit(10, p.curr()) {
			p.next()
		}
		switch {
		case p.curr() == '=':
			p.state = TokenizerStateLabelAssignment
		case isMarker(p.curr()):
			// #1234#
			// label reference
			p.state = TokenizerStateLabelReference
		}
		if p.err != nil {
			return
		}
		p.next()
		return

	case isBackSlash(p.curr()): // #\ character escape or character name
		p.state = TokenizerStateCharMnemonicOrHexEscape
		p.next()
		if p.err != nil {
			return
		}
		p.value = string(p.readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape())
		return

	case p.curr() == '(': // #( array
		p.state = TokenizerStateOpenVector
		p.next()
		return

	case p.curr() == ';': // datum comment
		// Emit DatumCommentBegin - parser handles datum boundary detection
		p.state = TokenizerStateDatumCommentBegin
		p.next()
		return

	case p.curr() == '!': // #! directive (R7RS §2.1)
		p.state = TokenizerStateDirective
		p.readDirective()
		return

	default:
		p.err = NewTokenizerError("invalid character after #", p.tokenStart, p.tokenEnd)
		return
	}
}

func (p *Tokenizer) readRadixMarker(r int, state TokenizerState) {
	p.radix = r
	p.state = state
	p.next()
	if p.err != nil {
		return
	}
	// Valid followers: delimiter, number initial, or another # (for combined prefixes like #d#e)
	if isDelimiterOrMarker(p.curr()) || isNumberInitial(p.radix, p.curr()) {
		return
	}
	p.state = TokenizerStateMarker
	for p.err == nil && isSubsequent(p.curr()) {
		p.next()
	}
}

func (p *Tokenizer) readExactness(state TokenizerState) {
	p.next() // skip i
	// Valid followers: delimiter, number initial, or another # (for combined prefixes like #i#x)
	if p.err != nil || isDelimiterOrMarker(p.curr()) || isNumberInitial(p.radix, p.curr()) {
		p.state = state
		return
	}
	p.state = TokenizerStateMarker
	for p.err == nil && isSubsequent(p.curr()) {
		p.next()
	}
}

func (p *Tokenizer) readBoolean(key string, state TokenizerState) {
	k := p.scanCaseInsensitive([]byte(key))
	if p.err != nil && !errors.Is(p.err, io.EOF) {
		// #t is a valid boolean
		return
	}
	if (k == 0 || k == len(key)-1) && (isDelimiter(p.curr()) || errors.Is(p.err, io.EOF)) {
		p.state = state
		return
	}
	p.state = TokenizerStateMarker
	for p.err == nil && isSubsequent(p.curr()) {
		p.next()
	}
}

// readTypedArrayOrExactnessOrRadixOrBooleanMarker parses letter-prefixed # tokens.
// Handles: #t/#true, #f/#false, #u8(, #i, #e, #b, #o, #d, #x.
// Called when curr() is a letter following '#'.
// Note: R7RS requires booleans to be case-insensitive, so #T, #TRUE, #F, #FALSE are valid.
func (p *Tokenizer) readTypedArrayOrExactnessOrRadixOrBooleanMarker() {
	switch {
	case p.curr() == 't' || p.curr() == 'T': // #true, #t, #TRUE, #T (R7RS: case-insensitive)
		p.readBoolean("true", TokenizerStateMarkerBooleanTrue)
		return
	case p.curr() == 'f' || p.curr() == 'F': // #false, #f, #FALSE, #F (R7RS: case-insensitive)
		p.readBoolean("false", TokenizerStateMarkerBooleanFalse)
		return
	case p.curr() == 'u' || p.curr() == 'U': // #u8(/#U8( bytevector (R7RS §7.1.1: case-insensitive)
		k := p.scanCaseInsensitive([]byte("u8"))
		if p.err != nil {
			p.state = TokenizerStateMarker
			return
		}
		if k != 0 || p.curr() != '(' {
			return
		}
		p.state = TokenizerStateOpenVectorUnsignedByteMarker
		p.next()
		return
	case p.curr() == 'i' || p.curr() == 'I': // inexact, #i/#I (R7RS §7.1.1: case-insensitive)
		p.readExactness(TokenizerStateMarkerNumberInexact)
		return
	case p.curr() == 'e' || p.curr() == 'E': // exact, #e/#E (R7RS §7.1.1: case-insensitive)
		p.readExactness(TokenizerStateMarkerNumberExact)
		return
	case p.curr() == 'b' || p.curr() == 'B': // binary, #b/#B (R7RS §7.1.1: case-insensitive)
		p.readRadixMarker(2, TokenizerStateMarkerBase2)
		return
	case p.curr() == 'o' || p.curr() == 'O': // octal, #o/#O (R7RS §7.1.1: case-insensitive)
		p.readRadixMarker(8, TokenizerStateMarkerBase8)
		return
	case p.curr() == 'd' || p.curr() == 'D': // decimal, #d/#D (R7RS §7.1.1: case-insensitive)
		p.readRadixMarker(10, TokenizerStateMarkerBase10)
		return
	case p.curr() == 'x' || p.curr() == 'X': // hex #x/#X (R7RS §7.1.1: case-insensitive)
		p.readRadixMarker(16, TokenizerStateMarkerBase16)
		return
	case p.curr() == 'm' || p.curr() == 'M': // big float #m
		p.state = TokenizerStateBigFloat
		p.readBigNum(isExtendedExponentMarkerForRadix)
		return
	case p.curr() == 'z' || p.curr() == 'Z': // big int #z
		p.state = TokenizerStateBigIntegerBase10
		p.readBigNum(isExtendedExponentMarkerForRadix)
		return
	default:
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		return
	}
}

// readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape parses character literals.
// Handles: #\x1234 (hex), #\newline (mnemonic), #\a (graphic character).
// Called after #\ has been consumed.
func (p *Tokenizer) readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape() rune {
	var qrune rune
	var mnemonic string
	// #\backspace, #\ffff, ...
	p.state = TokenizerStateCharMnemonicOrHexEscape
	switch {
	case p.curr() == 'x':
		// Peek ahead to see if hex digits follow - if not, #\x is just the character 'x'
		p.next()
		if p.err != nil {
			// EOF after #\x means just the character 'x'
			p.err = nil
			p.state = TokenizerStateCharGraphic
			return 'x'
		}
		if !isDigit(16, p.curr()) {
			// No hex digits follow, so #\x is the graphic character 'x'
			// (the next char will be handled by the caller)
			p.state = TokenizerStateCharGraphic
			return 'x'
		}
		// R7RS: \x<hex scalar value>; where hex scalar value is any Unicode code point
		// (0 to 0x10FFFF) except surrogates (0xD800-0xDFFF)
		p.state = TokenizerStateCharHexEscape
		x, n := p.readUnsignedBaseNInteger(16, 0) //nolint:errcheck
		if n == 0 {
			p.err = NewTokenizerErrorWithWrap(p.err, MessageInvalidCharacterHexEscape, p.tokenStart, p.tokenEnd)
			return utf8.RuneError
		}
		if x > 0x10FFFF {
			p.err = NewTokenizerError(MessageCodePointExceedsUnicodeMaximum, p.tokenStart, p.tokenEnd)
			return utf8.RuneError
		}
		if x >= 0xD800 && x <= 0xDFFF {
			p.err = NewTokenizerError(MessageCodePointIsSurrogate, p.tokenStart, p.tokenEnd)
			return utf8.RuneError
		}
		return rune(x)

	case isUnicodeLetter(p.curr()):
		p.state = TokenizerStateCharGraphic
		// consume identifier start
		qrune = p.curr()
		p.next()
		if p.err != nil {
			return qrune
		}
		if !isUnicodeLetter(p.curr()) {
			// Single letter character like #\a - valid graphic character
			return qrune
		}
		// It's a mnemonic - continue reading
		mnemonic = string(qrune)
		p.state = TokenizerStateCharMnemonic
		// read letters, digits or negative sign.  this applies to mnemonics like "backspace"
		for p.err == nil && (isUnicodeLetter(p.curr()) || isDigit(10, p.curr()) || isNegativeSign(p.curr())) {
			mnemonic += string(p.curr())
			p.next() // skip letter
		}
		switch {
		case strings.EqualFold(mnemonic, "alarm"):
			return '\a'
		case strings.EqualFold(mnemonic, "backspace"), strings.EqualFold(mnemonic, "back-space"):
			return '\b'
		case strings.EqualFold(mnemonic, "delete"):
			return '\x7F'
		case strings.EqualFold(mnemonic, "escape"):
			return '\x1B'
		case strings.EqualFold(mnemonic, "newline"):
			return '\n'
		case strings.EqualFold(mnemonic, "null"):
			return '\x00'
		case strings.EqualFold(mnemonic, "return"):
			return '\r'
		case strings.EqualFold(mnemonic, "space"):
			return ' '
		case strings.EqualFold(mnemonic, "tab"):
			return '\t'
		case strings.EqualFold(mnemonic, "vertical-tab"):
			return '\v'
		case strings.EqualFold(mnemonic, "form-feed"):
			return '\f'
		}
		p.err = NewTokenizerError(MessageInvalidCharacterMnemonic, p.tokenStart, p.tokenEnd)
		return utf8.RuneError

	case unicode.IsGraphic(p.curr()):
		p.state = TokenizerStateCharGraphic
		qrune = p.curr()
		p.next()
		return qrune
	}
	p.err = NewTokenizerError(MessageExpectingCharacterMnemonicOrHexEscape, p.tokenStart, p.tokenEnd)
	return utf8.RuneError
}

// readDirective reads a #! directive (e.g., #!fold-case, #!no-fold-case).
// Called after '#' has been consumed with '!' as curr().
// R7RS §2.1: #!fold-case and #!no-fold-case affect subsequent identifier reading.
func (p *Tokenizer) readDirective() {
	// #!backspace, #!b, ...
	// drop the '!'
	p.next()
	if p.err != nil {
		p.err = NewTokenizerError(MessageExpectingDirective, p.tokenStart, p.tokenEnd)
		return
	}
	// traditional identifier - letter followed by letter or number or dash
	if isUnicodeLetter(p.curr()) {
		p.next()
	} else {
		p.err = NewTokenizerError(MessageExpectingDirective, p.tokenStart, p.tokenEnd)
		return
	}
	// read letters, digits, dashes. this applies to directives like "fold-case"
	for p.err == nil && (isUnicodeLetter(p.curr()) || isDigit(10, p.curr()) || isNegativeSign(p.curr())) {
		p.next()
	}
}

// readSpecialNumber reads inf.0 or nan.0 special number literals.
// If onMismatch is provided, it's called when the keyword doesn't match;
// otherwise an error is set.
func (p *Tokenizer) readSpecialNumber(s string, r int, mismatchErr string, onMismatch func()) {
	n := p.scanCaseInsensitive([]byte(s))
	if p.err != nil {
		return
	}
	if n != 0 {
		if onMismatch != nil {
			onMismatch()
		} else {
			p.err = NewTokenizerError(mismatchErr, p.tokenStart, p.tokenEnd)
			return
		}
	}
	if !isDot(p.curr()) {
		p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		return
	}
	p.next()
	if !isDigit(r, p.curr()) {
		p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
}

func (p *Tokenizer) readNan(s string, r int) {
	p.readSpecialNumber(s, r, "", func() {
		for p.err == nil && isSignSubsequent(p.curr()) {
			p.next()
		}
	})
}

func (p *Tokenizer) readInf(s string, r int) {
	p.readSpecialNumber(s, r, MessageExpectingInf, nil)
}

// readBigNum reads an arbitrary-precision number after the prefix marker.
// The isExponentMarker function determines which exponent markers are valid.
func (p *Tokenizer) readBigNum(isExpMarker func(rune) bool) {
	p.next() // Advance past 'm'/'M' or 'z'/'Z'
	if p.err != nil {
		return
	}
	// Optional sign
	if isExplicitSign(p.curr()) {
		p.next()
		if p.err != nil {
			return
		}
	}
	// Integer part
	for p.err == nil && isDigit(10, p.curr()) {
		p.next()
	}
	// Optional decimal point
	if p.err == nil && p.curr() == '.' {
		p.next()
	}
	if p.err != nil {
		return
	}
	// Fractional part
	for p.err == nil && isDigit(10, p.curr()) {
		p.next()
	}
	if p.err != nil {
		return
	}
	// Optional exponent
	if isExpMarker(p.curr()) {
		p.next()
		if p.err != nil {
			return
		}
		if isExplicitSign(p.curr()) {
			p.next()
			if p.err != nil {
				return
			}
		}
		for p.err == nil && isDigit(10, p.curr()) {
			p.next()
		}
	}
}

func (p *Tokenizer) readDiv(r int) {
	p.next()
	if p.err != nil {
		return
	}
	if !isDigit(r, p.curr()) { // +10/10
		p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	p.readHashDigits()
}

func (p *Tokenizer) readDecimalFractionWithExponent(r int) {
	// R7RS §7.1.1 production 4: <digit 10>+ #+ . #* <suffix>
	// If integer part had hash digits, fraction part can only have hash digits (no real digits).
	hadHash := p.hashDigit
	// consume '.'
	p.next()
	if p.err != nil {
		return
	}
	if hadHash {
		// Production 4: only hash digits allowed after dot
		p.readHashDigits()
	} else {
		// R7RS allows zero or more digits after decimal point: <digit 10>+ . <digit 10>*
		// So "1." is valid (equivalent to "1.0")
		p.readUnsignedBaseNNumber(r) //nolint:errcheck
		p.readHashDigits()
	}
	if p.err != nil {
		return
	}
	// read optional exponent
	p.mayReadExponent(r) //nolint:errcheck
}

func (p *Tokenizer) readImaginaryOrSignedInfinity(r int) {
	p.state = TokenizerStateSignedImaginary
	p.next()
	if p.err != nil {
		return
	}
	if p.curr() != 'n' && p.curr() != 'N' { // +i -i
		// TODO +i -i
		p.state = TokenizerStateSignedImaginary
		return
	}
	// +inf.0
	p.state = TokenizerStateSignedInf
	// TODO: refactor readNan
	p.readInf("nf", r) //nolint:errcheck
	if p.err != nil {
		return
	}
	if isExplicitSign(p.curr()) {
		// +inf.0+2i - complex with signed decimal real
		p.state = TokenizerStateSignedComplex
		p.next()
		if p.err != nil {
			return
		}
		p.mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
		if p.err != nil {
			return
		}
		if !isImaginary(p.curr()) { // +inf.0i
			return
		}
		// skip 'i'
		p.next()
		return
	}
	if !isImaginary(p.curr()) { // +inf.0i
		return
	}
	p.state = TokenizerStateSignedImaginaryInf
	// skip 'i'
	p.next()
}

func (p *Tokenizer) readSignedNan(r int) {
	p.state = TokenizerStateSymbol
	p.readNan("nan", r) //nolint:errcheck
	// TODO: do not remove line below.  should not be needed, but current readNan implementation sets p.state to Symbol on error.
	if p.err != nil && !errors.Is(p.err, io.EOF) {
		// readNan failed - this is a symbol, not nan.0
		return
	}
	p.state = TokenizerStateSignedNan
	if !isImaginary(p.curr()) {
		return
	}
	p.state = TokenizerStateSignedImaginaryNan
	p.next()
}

func (p *Tokenizer) readSignedDecimalFractionOrExponentWithImaginary(r int) {
	p.next()
	if p.err != nil {
		return
	}
	switch {
	case isDotSubsequent(p.curr()):
		p.state = TokenizerStateSymbol
		p.next()
		if p.err != nil {
			return
		}
		p.readSymbol() //nolint:errcheck
		if p.err != nil {
			return
		}
	case !isDigit(r, p.curr()):
		p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		return
	default:
		p.state = TokenizerStateSignedDecimalFraction
	}
	// read decimal fractional part
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	p.readHashDigits()
	if p.err != nil {
		return
	}
	// read optional exponent
	p.mayReadExponent(r) //nolint:errcheck
	if p.err != nil {
		return
	}
	switch {
	case isImaginary(p.curr()):
		p.state = TokenizerStateSignedImaginary
		p.next()
		return
	case isComplexPolar(p.curr()):
		p.mayReadPolarPart(r)
		return
	case isExplicitSign(p.curr()):
		p.mayReadSignedImaginaryPart(true, r)
		return
	}
}

func (p *Tokenizer) readIntegerAndFraction(signed bool, r int) {
	p.state = p.integerStateForRadix(signed)
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	p.readHashDigits()
	if p.err != nil {
		return
	}
	switch {
	case isDot(p.curr()):
		if signed {
			p.state = TokenizerStateSignedDecimalFraction
		} else {
			p.state = TokenizerStateUnsignedDecimalFraction
		}
		p.readDecimalFractionWithExponent(r)
	case p.curr() == '/':
		if signed {
			p.state = TokenizerStateSignedRationalFraction
		} else {
			p.state = TokenizerStateUnsignedRationalFraction
		}
		p.readDiv(r) //nolint:errcheck
	case isExtendedExponentMarkerForRadix(p.curr()):
		if signed {
			p.state = TokenizerStateSignedScientificNotation
		} else {
			p.state = TokenizerStateUnsignedScientificNotation
		}
		p.mayReadExponent(r) //nolint:errcheck
	}
	if p.err != nil {
		return
	}
	switch {
	case isImaginary(p.curr()):
		if signed {
			p.state = TokenizerStateSignedImaginary
		} else {
			p.state = TokenizerStateUnsignedImaginary
		}
		p.next()
	case isExplicitSign(p.curr()):
		if signed {
			p.state = TokenizerStateSignedComplex
			p.mayReadSignedImaginaryPart(true, r) //nolint:errcheck
		} else {
			p.state = TokenizerStateUnsignedComplex
			p.next() // consume sign
			if p.err != nil {
				return
			}
			// Check for unit imaginary (+i or -i) - must peek to distinguish from inf.0
			if p.curr() == 'i' {
				p.next() // consume 'i'
				if p.err != nil {
					return
				}
				// If followed by 'n', it might be inf.0i - continue parsing
				if p.curr() == 'n' {
					// Parse "nf.0" portion of inf.0
					n := p.scanCaseInsensitive([]byte("nf"))
					if p.err != nil {
						return
					}
					if n != 0 {
						p.err = NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
						return
					}
					if !isDot(p.curr()) {
						p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
						return
					}
					p.next()
					if p.err != nil {
						return
					}
					if !isDigit(r, p.curr()) {
						p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
						return
					}
					p.readUnsignedBaseNNumber(r)
					if p.err != nil {
						return
					}
					if isImaginary(p.curr()) {
						p.next()
					}
				}
				// Otherwise just unit imaginary, already consumed 'i'
				return
			}
			p.mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
			if p.err != nil {
				return
			}
			if !isImaginary(p.curr()) {
				p.err = NewTokenizerError(MessageExpectingImaginary, p.tokenStart, p.tokenEnd)
				return
			}
			p.next()
		}
	case isComplexPolar(p.curr()):
		if signed {
			p.state = TokenizerStateSignedComplexPolar
		} else {
			p.state = TokenizerStateUnsignedComplexPolar
		}
		p.mayReadPolarPart(r) //nolint:errcheck
	}
}

func (p *Tokenizer) readConsOrDecimalFractionWithExponent(r int) {
	p.state = TokenizerStateCons
	p.next()
	if p.err != nil {
		return
	}
	if isDotSubsequent(p.curr()) {
		p.state = TokenizerStateSymbol
		p.next()
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		p.value = p.text
	}
	if !isDigit(r, p.curr()) {
		return
	}
	p.state = TokenizerStateUnsignedDecimalFraction
	p.next()
	if p.err != nil {
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	p.readHashDigits()
	if p.err != nil {
		return
	}
	p.mayReadExponent(r) //nolint:errcheck
}

// readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber parses numeric literals.
//
// This function handles the complex grammar for Scheme numeric literals, which can be:
//   - Signed integers: +123, -456
//   - Unsigned integers: 123
//   - Decimal fractions: 1.23, +1.23, -1.23, .5
//   - Rational fractions: 1/2, +3/4, -5/6
//   - Special values: +inf.0, -inf.0, +nan.0, -nan.0
//   - Imaginary units: +i, -i, +inf.0i, -inf.0i, +nan.0i, -nan.0i
//   - Scientific notation: 1e10, 1.5e-3
//   - Cons dot: . (when followed by whitespace)
//   - Ellipsis and peculiar identifiers: ..., .foo
//
// Parameters:
//   - e: unused (reserved for exactness tracking)
//   - r: radix (base) for parsing digits (2, 8, 10, or 16)
//
// The function uses a decision tree based on the first character:
//
//	+/- (explicit sign):
//	  ├─ 'i' → +i/-i or +inf.0/-inf.0 (with optional trailing 'i' for imaginary)
//	  ├─ 'n' → +nan.0/-nan.0 (with optional trailing 'i' for imaginary)
//	  ├─ '.' → signed decimal fraction (+.5, -.25)
//	  ├─ digit → signed integer, may extend to decimal (+1.5) or rational (+1/2)
//	  └─ sign-subsequent → peculiar identifier (+++, ---)
//
//	'.' (dot):
//	  ├─ dot-subsequent → symbol (..., .+, etc.)
//	  ├─ digit → unsigned decimal fraction (.5, .123)
//	  └─ otherwise → cons dot
//
//	digit:
//	  ├─ '/' after digits → rational fraction (1/2)
//	  ├─ '.' after digits → decimal fraction (1.5)
//	  ├─ 'e'/'E' after digits → scientific notation (1e10)
//	  └─ otherwise → unsigned integer
func (p *Tokenizer) readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(r int) {
	// Branch 1: Starts with explicit sign (+/-)
	switch {
	case isExplicitSign(p.curr()):
		p.signed = true
		p.state = TokenizerStateSymbol
		p.next()
		if p.err != nil {
			// Bare sign at EOF — populate value from text
			p.value = p.text
			return
		}
		switch {
		case isImaginary(p.curr()):
			p.readImaginaryOrSignedInfinity(r)
			return
		case p.curr() == 'n' || p.curr() == 'N':
			p.readSignedNan(r)
			return
		case isDigit(r, p.curr()):
			p.readIntegerAndFraction(true, r)
			return
		case isDot(p.curr()):
			p.readSignedDecimalFractionOrExponentWithImaginary(r)
			return
		case isSignSubsequent(p.curr()):
			p.state = TokenizerStateSymbol
			for p.err == nil && isSymbolSubsequent(p.curr()) {
				p.next()
			}
			p.value = p.text
			return
		}
		// Bare sign (+/-) as symbol
		p.value = p.text
		return
	case isDot(p.curr()):
		p.readConsOrDecimalFractionWithExponent(r)
		return
	case isDigit(r, p.curr()):
		p.readIntegerAndFraction(false, r)
		return
	default:
		p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
		return
	}
}

func (p *Tokenizer) mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r int) {
	// Branch 1: Starts with explicit sign (+/-)
	switch {
	case p.curr() == 'i':
		// Could be inf.0 - caller already handles unit imaginary 'i'
		p.readInf("inf", r) //nolint:errcheck
		return
	case p.curr() == 'n':
		// TODO: refactor readNan
		p.readNan("nan", r) //nolint:errcheck
		return
	case isDot(p.curr()):
		// consume dot
		// p.readDecimalFractionWithExponent(r) is very similar
		// decimal fraction - use p.signed to determine correct state
		p.next()
		if p.err != nil {
			return
		}
		if isDotSubsequent(p.curr()) {
			// TODO Sym
			p.next()
			if p.err != nil {
				return
			}
			p.readSymbol() //nolint:errcheck
		} else if !isDigit(r, p.curr()) { // +.10
			// TODO Peculiar identifier
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return
		}
		p.readUnsignedBaseNNumber(r) //nolint:errcheck
		p.readHashDigits()
		if p.err != nil {
			return
		}
		p.mayReadExponent(r) //nolint:errcheck
		return
	case isDigit(r, p.curr()):
		p.readUnsignedBaseNNumber(r) //nolint:errcheck
		p.readHashDigits()
		if p.err != nil {
			return
		}
		switch {
		case isDot(p.curr()):
			p.readDecimalFractionWithExponent(r)
		case isExtendedExponentMarkerForRadix(p.curr()):
			p.mayReadExponent(r) // nolint:errcheck
		case p.curr() == '/':
			p.readDiv(r) // nolint:errcheck
		}
		return
	}
}

func (p *Tokenizer) mayReadExponent(r int) {
	if !isExtendedExponentMarkerForRadix(p.curr()) {
		return
	}
	var ok bool
	ok, p.strength = exponentMarkerStrength(p.curr())
	if !ok {
		p.err = NewTokenizerError(MessageExpectingExponentMarker, p.tokenStart, p.tokenEnd)
		return
	}
	p.next() // consume exponent marker
	if p.err != nil {
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingExponentDigits, p.tokenStart, p.tokenEnd)
		return
	}
	// Don't return early on EOF - we need to check for required digits below

	// Optional sign
	if isExplicitSign(p.curr()) {
		p.next()
		if p.err != nil {
			p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingExponentDigits, p.tokenStart, p.tokenEnd)
			return
		}
	}

	// R7RS requires at least one digit after exponent marker (and optional sign)
	// When p.err is set (including io.EOF), p.curr() returns RuneError which isDigit rejects
	if !isDigit(r, p.curr()) {
		p.err = NewTokenizerError(MessageExpectingExponentDigits, p.tokenStart, p.tokenEnd)
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
}

func (p *Tokenizer) scanForImaginaryNumberSpecials(r int, txt string) {
	n := p.scanCaseInsensitive([]byte(txt))
	if p.err != nil {
		return
	}
	if n != 0 {
		p.err = NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
		return
	}
	if !isDot(p.curr()) { // +inf.0
		p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		return
	}
	// skip '.'
	p.next()
	if p.err != nil {
		return
	}
	if !isDigit(r, p.curr()) {
		p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		return
	}
	// read a number
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	if p.err != nil {
		return
	}
	// check for imaginary 'i'
	if !isImaginary(p.curr()) {
		return
	}
	// skip 'i'
	p.next()
}

// SignedImaginaryPart reads an optional imaginary part for complex numbers.
// Called when current character is '+' or '-' after reading a real number.
// Handles patterns like: +3i, +3.5i, +i, -2i, -inf.0i, -nan.0i
// The complexState parameter specifies which state to set on success
// (SignedComplex or UnsignedComplex depending on whether the real part was signed).
func (p *Tokenizer) mayReadSignedImaginaryPart(_ bool, r int) {
	// Save position in case we need to backtrack (but we don't support backtracking,
	// so we commit once we see the sign followed by valid imaginary syntax)
	if isExplicitSign(p.curr()) {
		p.next() // consume '+' or '-'
		if p.err != nil {
			return
		}
	}

	// Check for +i or -i (pure imaginary unit) or +inf.0i or -inf.0i
	if p.curr() == 'i' {
		p.next() // consume 'i'
		if p.err != nil {
			return
		}
		// If NOT followed by 'n', it's pure imaginary (+i or -i) - we're done
		if p.curr() != 'n' {
			return
		}
		// Could be inf.0i - continue parsing "nf.0<digits>i"
		n := p.scanCaseInsensitive([]byte("nf"))
		if p.err != nil {
			return
		}
		if n != 0 {
			p.err = NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
			return
		}
		if !isDot(p.curr()) {
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return
		}
		p.next()
		if p.err != nil {
			return
		}
		if !isDigit(r, p.curr()) {
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return
		}
		p.readUnsignedBaseNNumber(r)
		if p.err != nil {
			return
		}
		if isImaginary(p.curr()) {
			p.next()
		}
		return
	} else if p.curr() == 'n' {
		// Check for +nan.0i or -nan.0i
		p.scanForImaginaryNumberSpecials(r, "nan") //nolint:errcheck
		return
	}

	// Check for numeric coefficient: +3i, +3.5i, +3/4i, etc.
	if !isDigit(r, p.curr()) {
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	p.readHashDigits()
	if p.err != nil {
		return
	}
	// Check for decimal part, rational part, or exponent
	switch {
	case isDot(p.curr()):
		// Decimal: +3.5i
		hadHash := p.hashDigit
		p.next()
		if p.err != nil {
			return
		}
		if hadHash {
			// Production 4: only hash digits after dot
			p.readHashDigits()
		} else if isDigit(r, p.curr()) {
			p.readUnsignedBaseNNumber(r) //nolint:errcheck
			p.readHashDigits()
		}
		if p.err != nil {
			return
		}
		p.mayReadExponent(r) //nolint:errcheck
		if p.err != nil {
			return
		}
	case p.curr() == '/':
		// Rational: +3/4i
		p.next()
		if p.err != nil {
			return
		}
		if !isDigit(r, p.curr()) {
			p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
			return
		}
		p.readUnsignedBaseNNumber(r) //nolint:errcheck
		p.readHashDigits()
		if p.err != nil {
			return
		}
	default:
		// Exponent: +3e10i or just integer +3i
		p.mayReadExponent(r) //nolint:errcheck
		if p.err != nil {
			return
		}
	}
	// Must end with 'i'
	if !isImaginary(p.curr()) {
		return
	}
	p.next()
}

// mayReadPolarPart reads an optional polar angle part for complex numbers.
// Called when current character is '@' after reading a real number (the magnitude).
// Handles patterns like: @1.5708, @0.785, @-1.5708
// If successful, sets state to TokenizerStateUnsignedComplexPolar.
func (p *Tokenizer) mayReadPolarPart(r int) {
	if p.curr() != '@' {
		return
	}
	p.next() // consume '@'
	if p.err != nil {
		return
	}

	// The angle can be a signed or unsigned real number
	// Check for explicit sign first
	if isExplicitSign(p.curr()) {
		p.next() // consume '+' or '-'
		if p.err != nil {
			return
		}
	}

	// Must have digits or a dot followed by digits
	switch {
	case isDot(p.curr()):
		p.readDiv(r) //nolint:errcheck
		if p.err != nil {
			return
		}
	case isDigit(r, p.curr()):
		p.readUnsignedBaseNNumber(r) //nolint:errcheck
		p.readHashDigits()
		if p.err != nil {
			return
		}
		if !isDot(p.curr()) {
			break
		}
		hadHash := p.hashDigit
		p.next()
		if p.err != nil {
			return
		}
		if hadHash {
			// Production 4: only hash digits after dot
			p.readHashDigits()
		} else if isDigit(r, p.curr()) {
			p.readUnsignedBaseNNumber(r) //nolint:errcheck
			p.readHashDigits()
		}
		if p.err != nil {
			return
		}
	default:
		p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
		return
	}

	// Check for exponent
	p.mayReadExponent(r) //nolint:errcheck
	if p.err != nil {
		return
	}

	p.state = TokenizerStateUnsignedComplexPolar
}

func (p *Tokenizer) readBaseNInteger(r, maxn int) (int64, int) {
	n := 0
	sign := '+'
	if isExplicitSign(p.curr()) {
		sign = p.curr()
		p.next()
		n++
		if p.err != nil {
			return 0, n
		}
	}
	q, n0 := p.readUnsignedBaseNInteger(r, maxn)
	n += n0
	if sign == '-' {
		q = -q
	}
	return q, n
}

func (p *Tokenizer) readUnsignedBaseNInteger(r, maxn int) (int64, int) {
	n := 0
	var s string
	for p.err == nil && (isDigit(r, p.curr()) && (maxn <= 0 || n < maxn)) {
		s += string(p.curr())
		p.next()
		n++
	}
	if n == 0 {
		return 0, n
	}
	var q int64
	var err error
	// always attempt to parse s, even on error
	q, err = strconv.ParseInt(s, r, 64)
	if err != nil {
		err = NewTokenizerErrorWithWrap(p.err, MessageCannotParseNumber, p.tokenStart, p.tokenEnd)
	}
	if p.err == nil {
		p.err = err
	}
	return q, n
}

func (p *Tokenizer) readUnsignedBaseNNumber(r int) {
	for p.err == nil && isDigit(r, p.curr()) {
		p.next()
	}
}

// readHashDigits consumes a run of '#' characters used as R7RS §7.1.1
// inexact digit placeholders. Each '#' represents an unknown digit (treated
// as 0 by the parser). Once '#' appears, the number becomes inexact.
func (p *Tokenizer) readHashDigits() {
	for p.err == nil && p.curr() == '#' {
		p.hashDigit = true
		p.next()
	}
}

// readSymbol consumes subsequent characters that are valid in a symbol.
// Called after the initial character has been read.
func (p *Tokenizer) readSymbol() {
	for p.err == nil && isSymbolSubsequent(p.curr()) {
		p.value += string(p.curr())
		p.next()
	}
}

// readExtendedSymbol reads an extended symbol enclosed in vertical bars (|symbol|).
// Called after the opening '|' has been consumed.
func (p *Tokenizer) readExtendedSymbol() {
	for p.err == nil && !isVerticalLine(p.curr()) {
		if isBackSlash(p.curr()) {
			// skip backslash and read next intra-extended token character
			p.next()
			if p.err != nil {
				if errors.Is(p.err, io.EOF) {
					p.err = NewTokenizerError(MessageUnterminatedExtendedSymbol, p.tokenStart, p.tokenEnd)
				}
				return
			}
			p.readIntraExtendedToken()
			continue
		}
		p.value += string(p.curr())
		if p.err != nil {
			return
		}
		p.next()
	}
	if p.err != nil {
		if errors.Is(p.err, io.EOF) {
			p.err = NewTokenizerError(MessageUnterminatedExtendedSymbol, p.tokenStart, p.tokenEnd)
		}
		return
	}
	// consume closing '|'
	p.next()
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

// escape returns the character for common escape sequences.
// Handles: \0, \a, \b, \t, \n, \r.
func (p *Tokenizer) escape() rune {
	switch p.cur {
	case '0':
		return 0
	case 'a':
		return '\a'
	case 'b':
		return '\b'
	case 't':
		return '\t'
	case 'n':
		return '\n'
	case 'r':
		return '\r'
	}
	return 0
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
	p.scratch = p.scratch[:0]
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

// isEOF returns true if the tokenizer has reached end of input.
func (p *Tokenizer) isEOF() bool {
	return errors.Is(p.err, io.EOF)
}

// span returns the accumulated source text for the current token.
func (p *Tokenizer) span() string {
	return p.text
}

// this returns the current rune and any error.
func (p *Tokenizer) this() rune {
	return p.cur
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
		p.err = NewTokenizerError(MessageRuneError, p.tokenStart, p.tokenEnd)
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
func isUnicodeLetter(c rune) bool {
	return unicode.IsLetter(c) || unicode.Is(unicode.Nl, c)
}

// isSymbolInitial returns true if c can start a symbol (letter or special initial).
func isSymbolInitial(c rune) bool {
	if isUnicodeLetter(c) {
		return true
	}
	if isSpecialInitial(c) {
		return true
	}
	return false
}

func isUnicodeDigit(c rune) bool {
	return unicode.IsDigit(c) || unicode.IsOneOf([]*unicode.RangeTable{unicode.Nd}, c)
}

// isSymbolSubsequent returns true if c can appear after the first character of a symbol.
func isSymbolSubsequent(c rune) bool {
	for isIdentifierInitial(c) || isSpecialSubsequent(c) || isExtendedSubsequent(c) || isUnicodeDigit(c) {
		return true
	}
	return false
}

// isIdentifierInitial returns true if c can start an identifier.
func isIdentifierInitial(c rune) bool {
	return isSpecialInitial(c) || isUnicodeLetter(c)
}

// isVerticalLine returns true if c is the vertical line character (|).
func isVerticalLine(c rune) bool {
	return c == '|'
}

// isDelimiter returns true if c is a token delimiter per R7RS 7.1.1.
// Delimiters are: whitespace, |, (, ), ", and ;
func isDelimiter(c rune) bool {
	return isIntralineWhitespace(c) || isVerticalLine(c) || isLineEnding(c) ||
		c == '(' || c == ')' || c == '"' || c == ';'
}

// isDelimiterOrMarker returns true if c is a token delimiter per R7RS 7.1.1.
// Delimiters are: whitespace, |, (, ), ", and ;
func isDelimiterOrMarker(c rune) bool {
	return isDelimiter(c) || isMarker(c)
}

// isReturn returns true if c is a carriage return (\r).
func isReturn(c rune) bool {
	return c == '\r'
}

// isNewLine returns true if c is a newline (\n).
func isNewLine(c rune) bool {
	return c == '\n'
}

// isLineEnding returns true if c is a line ending character (\r or \n).
func isLineEnding(c rune) bool {
	return isReturn(c) || isNewLine(c)
}

// isIntralineWhitespace returns true if c is a space or tab.
func isIntralineWhitespace(c rune) bool {
	return c == ' ' || c == '\t'
}

// isDigit returns true if c is a valid digit in the given radix (2, 8, 10, or 16).
// Radix 0 is treated as 10 (default decimal).
func isDigit(r int, c rune) bool {
	if c < '0' {
		return false
	}
	if r == 2 && c <= '1' {
		return true
	}
	if r == 8 && c <= '7' {
		return true
	}
	if (r == 0 || r == 10) && c <= '9' {
		return true
	}
	if r == 16 && c <= '9' {
		return true
	}
	if r == 16 && ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
		return true
	}
	return false
}

// isNumberInitial returns true if c can start a number (sign, dot, or digit).
func isNumberInitial(r int, c rune) bool {
	if isExplicitSign(c) || isDot(c) {
		return true
	}
	if c < '0' {
		return false
	}
	switch r {
	case 2:
		return c <= '1'
	case 8:
		return c <= '7'
	case 16:
		if (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') {
			return true
		}
		fallthrough
	case 10, 0: // 0 == default
		return c <= '9'
	}
	return false
}

// digit returns the numeric value of c in the given radix, or -1 if invalid.
func digit(r int, c rune) int {
	if c >= utf8.RuneSelf {
		return -1
	}
	switch r {
	case 2, 8, 10:
		return int(c - '0')
	case 16:
		if c >= '0' && c <= '9' {
			return int(c - '0')
		}
		if c >= 'A' && c <= 'F' {
			return int(c-'A') + 10
		}
		if c >= 'a' && c <= 'f' {
			return int(c-'a') + 10
		}
		// error
	}
	return -1
}

// isMarket returns true if c is a market character (#).
func isMarker(c rune) bool {
	return c == '#'
}

// isSpecialInitial returns true if c is a special initial character for identifiers.
// Special initials: ! $ % & * / : < = > ? ^ _ ~
func isSpecialInitial(c rune) bool {
	switch c {
	case '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~':
		return true
	}
	return false
}

// isPositiveSign returns true if c is a plus sign (+).
func isPositiveSign(c rune) bool {
	return c == '+'
}

// isNegativeSign returns true if c is a minus sign (-).
func isNegativeSign(c rune) bool {
	return c == '-'
}

// isDot returns true if c is a dot (.).
func isDot(c rune) bool {
	return c == '.'
}

// isBackSlash returns true if c is a backslash (\).
func isBackSlash(c rune) bool {
	return c == '\\'
}

// isQuote returns true if c is a backslash (\).
func isQuote(c rune) bool {
	return c == '\''
}

// isImaginary returns true if c is 'i'.
func isImaginary(c rune) bool {
	return c == 'i' || c == 'I'
}

// isExtendedExponentMarker returns true if c is an exponent marker character.
// R7RS defines 'e' as the standard marker; s/f/d/l are precision extensions.
func isExtendedExponentMarker(c rune) bool {
	return c == 'e' || c == 'E' || c == 's' || c == 'S' || c == 'f' || c == 'F' || c == 'd' || c == 'D' || c == 'l' || c == 'L'
}

// isExtendedExponentMarkerForRadix returns true if c is an exponent marker valid for radix r.
// Per R7RS, exponent markers are only valid in base-10 decimal numbers.
// There is no <decimal 2>, <decimal 8>, or <decimal 16> in the grammar.
func isExtendedExponentMarkerForRadix(c rune) bool {
	// Exponents only valid in base 10 (r=0 means default decimal)
	return isExtendedExponentMarker(c)
}

// exponentMarkerStrength returns true and the strength of the exponent marker if c is valid.
// Strengths: e/E=64, s/S=16, f/F=32, d/D=64, l/L=128; otherwise false,0.
func exponentMarkerStrength(c rune) (bool, int) {
	switch c {
	case 'e', 'E':
		// default exponent marker (double)
		return true, 64
	case 's', 'S':
		// small float (not supported natively)
		return true, 16
	case 'f', 'F':
		// single-precision float
		return true, 32
	case 'd', 'D':
		// double-precision float
		return true, 64
	case 'l', 'L':
		// long double-precision float (not supported natively)
		return true, 128
	}
	return false, 0
}

// isComplexPolar returns true if c is '@'.
func isComplexPolar(c rune) bool {
	return c == '@'
}

// isExplicitSign returns true if c is a sign character (+ or -).
func isExplicitSign(c rune) bool {
	return isPositiveSign(c) || isNegativeSign(c)
}

// isSpecialSubsequent returns true if c is a special subsequent character (. @ + -).
func isSpecialSubsequent(c rune) bool {
	return isDot(c) || c == '@' || isExplicitSign(c)
}

// isSignSubsequent returns true if c can follow a sign in a peculiar identifier.
func isSignSubsequent(c rune) bool {
	return c == '@' || isInitial(c) || isExplicitSign(c)
}

// isDotSubsequent returns true if c can follow a dot in a peculiar identifier.
func isDotSubsequent(c rune) bool {
	return isDot(c) || isSignSubsequent(c)
}

// isInitial returns true if c can start an identifier (letter or special initial).
func isInitial(c rune) bool {
	return isUnicodeLetter(c) || isSpecialInitial(c)
}

// isExtendedSubsequent
func isExtendedSubsequent(c rune) bool {
	// check for unicode mark or connector punctuation
	return unicode.IsOneOf([]*unicode.RangeTable{unicode.Mark, unicode.Pc}, c)
}

// isSubsequent returns true if c can appear after the first character of an identifier.
func isSubsequent(c rune) bool {
	return isInitial(c) || isDigit(10, c) || isSpecialSubsequent(c) || isExtendedSubsequent(c)
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
