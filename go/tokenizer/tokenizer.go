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
//   - Literals: Symbol, String, Character (graphic, mnemonic, hex escape)
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
	"io"
	"wile/syntax"
	"wile/values"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

const (
	MessageRuneError                = "rune error"
	MessageExpectingNumber          = "expecting number"
	MessageExpectingImaginary       = "expecting imaginary"
	MessageExpectingDecimalFraction = "expecting decimal fraction"
	MessageExpectingNan             = "expecting NaN"
	MessageExpectingInf             = "expecting Inf"
	MessageExpectingTrue            = "expecting true"
	MessageExpectingFalse           = "expecting false"
	MessageExpectingToken           = "expecting token"
	MessageExpectingEscape          = "expecting escape"
)

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

const (
	TokenizerStateFailed TokenizerState = iota // Tokenization failed

	// Syntax quotation tokens (#', #`, #,, #,@)
	TokenizerStateSyntax           // #'expr (syntax quote)
	TokenizerStateUnsyntax         // #,expr (unsyntax)
	TokenizerStateUnsyntaxSplicing // #,@expr (unsyntax-splicing)
	TokenizerStateQuasisyntax      // #`expr (quasisyntax)

	// Standard quotation tokens (', `, ,, ,@)
	TokenizerStateQuote           // 'expr (quote)
	TokenizerStateUnquote         // ,expr (unquote)
	TokenizerStateUnquoteSplicing // ,@expr (unquote-splicing)
	TokenizerStateQuasiquote      // `expr (quasiquote)

	// Special numeric values
	TokenizerStateSignedInf            // +inf.0 or -inf.0 (infinity)
	TokenizerStateSignedNan            // +nan.0 or -nan.0 (not a number)
	TokenizerStateSignedImaginaryInf   // +inf.0i or -inf.0i (imaginary infinity)
	TokenizerStateSignedImaginaryNan   // +nan.0i or -nan.0i (imaginary NaN)
	TokenizerStateSignedImaginary      // +i, -i, +3i, -3.5i (pure imaginary with optional coefficient)
	TokenizerStateSignedComplex        // +1+2i, 3.5-2.5i (full complex number)
	TokenizerStateSignedComplexPolar   // +1@1.5708, 3.5@0.785 (polar complex: magnitude@angle)
	TokenizerStateUnsignedImaginaryInf // +inf.0i or -inf.0i (imaginary infinity)
	TokenizerStateUnsignedImaginaryNan // +nan.0i or -nan.0i (imaginary NaN)
	TokenizerStateUnsignedImaginary    // +i, -i, +3i, -3.5i (pure imaginary with optional coefficient)
	TokenizerStateUnsignedComplex      // 1+2i, 3.5-2.5i (full complex number)
	TokenizerStateUnsignedComplexPolar // 1@1.5708, 3.5@0.785 (polar complex: magnitude@angle)

	// Marker tokens (#-prefixed)
	TokenizerStateMarker              // Generic # marker
	TokenizerStateMarkerBooleanFalse  // #f or #false
	TokenizerStateMarkerBooleanTrue   // #t or #true
	TokenizerStateMarkerNumberInexact // #i prefix (inexact)
	TokenizerStateMarkerNumberExact   // #e prefix (exact)

	// Integer tokens
	TokenizerStateSignedInteger   // -123 or +456 (signed decimal)
	TokenizerStateUnsignedInteger // 123 (unsigned decimal)
	TokenizerStateIntegerBase2    // Binary integer after #b
	TokenizerStateIntegerBase8    // Octal integer after #o
	TokenizerStateIntegerBase10   // Decimal integer after #d
	TokenizerStateIntegerBase16   // Hex integer after #x

	// Numeric Types
	TokenizerStateBigFloat   // #z Arbitrary-precision decimal number
	TokenizerStateBigInteger // #m Arbitrary-precision integer number

	// Radix prefix markers
	TokenizerStateMarkerBase2  // #b prefix (binary)
	TokenizerStateMarkerBase8  // #o prefix (octal)
	TokenizerStateMarkerBase10 // #d prefix (decimal)
	TokenizerStateMarkerBase16 // #x prefix (hexadecimal)

	// Fractional number tokens
	TokenizerStateSignedDecimalFraction    // -1.23 or +4.56
	TokenizerStateSignedRationalFraction   // -1/2 or +3/4
	TokenizerStateUnsignedRationalFraction // 1/2 or 3/4
	TokenizerStateUnsignedDecimalFraction  // 1.23 or 4.56

	// Delimiter tokens
	TokenizerStateEmptyList  // ()
	TokenizerStateOpenParen  // (
	TokenizerStateCloseParen // )
	TokenizerStateCons       // . (dot for improper lists)

	// String tokens
	TokenizerStateStringStart       // Opening "
	TokenizerStateStringSpan        // String content
	TokenizerStateStringIntraEscape // Escape sequence within string
	TokenizerStateStringEnd         // Complete "string"

	// Character tokens (#\...)
	TokenizerStateCharMnemonicOrHexEscape // Intermediate state
	TokenizerStateCharMnemonic            // #\newline, #\space, etc.
	TokenizerStateCharHexEscape           // #\x0A (hex escape)
	TokenizerStateCharGraphic             // #\a (single graphic char)

	// Comment tokens
	TokenizerStateLineCommentStart  // ; (legacy single token)
	TokenizerStateBlockComment      // #| ... |# (legacy single token)
	TokenizerStateDatumComment      // #; (legacy single token)
	TokenizerStateLineCommentBegin  // ; (multi-token: begin)
	TokenizerStateLineCommentBody   // comment text (multi-token: body)
	TokenizerStateLineCommentEnd    // newline (multi-token: end)
	TokenizerStateBlockCommentBegin // #| (multi-token: begin)
	TokenizerStateBlockCommentBody  // block content (multi-token: body)
	TokenizerStateBlockCommentEnd   // |# (multi-token: end)
	TokenizerStateDatumCommentBegin // #; (multi-token mode)

	// Symbol and identifier tokens
	TokenizerStateExtendedSymbolStart // | (start of |symbol|)
	TokenizerStateSymbol              // identifier or symbol

	// Vector tokens
	TokenizerStateOpenVector                   // #(
	TokenizerStateOpenVectorUnsignedByteMarker // #u8(

	// Directive and label tokens
	TokenizerStateDirective       // #!fold-case, etc.
	TokenizerStateLabelReference  // #123# (datum label reference)
	TokenizerStateLabelAssignment // #123= (datum label assignment)
)

type Tokenizer struct {
	rdr        io.RuneReader
	cur        rune
	err        error
	i          int
	runeEnd    syntax.SourceIndexes // emd of the current rune
	runeStart  syntax.SourceIndexes // start of the current rune tokenStartIndex  int // start of the current token
	tokenStart syntax.SourceIndexes
	tokenEnd   syntax.SourceIndexes // end of the current token
	scratch    []rune
	value      string
	text       string
	state      TokenizerState
	negative   bool
	signed     bool
	radix      int
	// Comment tokenization support
	emitComments bool // whether to emit comment tokens instead of skipping
	blockDepth   int  // nesting depth for block comments
	commentPhase int  // 0=none, 1=begin emitted, 2=body emitted
	commentType  int  // 1=line, 2=block (tracks which comment type we're in)
	ci           bool // case insensitive symbol mode
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
		rdr: rdr,
		ci:  ci,
	}
	// 611-FIXME: duplicate code.  consolidate
	n := 0
	q.cur, n, q.err = q.rdr.ReadRune()
	q.runeStart = q.runeEnd
	q.runeEnd.Inc(n)
	if n == 0 {
		q.cur = utf8.RuneError
		q.err = io.EOF
	} else if q.cur == utf8.RuneError {
		q.err = NewTokenizerError(MessageRuneError, q.tokenStart, q.tokenEnd)
	}
	return q
}

// NewTokenizerWithComments creates a tokenizer with optional comment token emission.
// When emitComments is true, comments are returned as Begin/Body/End token sequences
// instead of being skipped.
func NewTokenizerWithComments(rdr io.RuneReader, ci, emitComments bool) *Tokenizer {
	q := NewTokenizer(rdr, ci)
	q.emitComments = emitComments
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
	for {
		// Handle comment phase continuation BEFORE EOF check
		// This allows emitting End tokens even when at EOF
		if p.commentPhase > 0 {
			tok, err := p.continueCommentToken()
			if err != nil {
				return nil, err
			}
			// Skip comment tokens when not emitting comments
			if !p.emitComments && isCommentToken(tok.Type()) {
				continue
			}
			return tok, nil
		}

		if p.curr() == utf8.RuneError {
			return nil, p.err
		}

		p.skipWhitespace() //nolint:errcheck
		if p.err != nil {
			return nil, p.err
		}
		p.mark()
		p.read()
		src := p.text
		val := p.value
		q := NewSimpleToken(p.state, src, val, &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix)

		// Skip comment tokens when not emitting comments
		if !p.emitComments && isCommentToken(p.state) {
			continue
		}

		return q, nil
	}
}

// continueCommentToken continues emitting tokens for an in-progress comment sequence
func (p *Tokenizer) continueCommentToken() (Token, error) {
	p.mark()

	switch p.commentType {
	case 1: // line comment
		return p.continueLineComment()
	case 2: // block comment
		return p.continueBlockComment()
	}

	// Should not reach here - reset and continue normal tokenization
	p.commentPhase = 0
	p.commentType = 0
	return p.Next()
}

// continueLineComment handles Body and End phases for line comments
func (p *Tokenizer) continueLineComment() (Token, error) {
	switch p.commentPhase {
	case 1: // Begin was emitted, now emit Body
		p.state = TokenizerStateLineCommentBody
		p.commentPhase = 2
		// Read comment content until newline or EOF
		for p.err == nil && !isLineEnding(p.curr()) {
			p.next()
		}
		p.term()
		return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix), nil

	case 2: // Body was emitted, now emit End (only if there's a line ending, not at EOF)
		p.commentPhase = 0
		p.commentType = 0
		// Only emit End token if there's an actual line ending; skip at EOF
		if p.err == nil && isLineEnding(p.curr()) {
			p.state = TokenizerStateLineCommentEnd
			p.scanLineEnding() //nolint:errcheck
			p.term()
			return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix), nil
		}
		// At EOF, no End token - just continue to next (which will return EOF)
		return p.Next()
	}

	// Should not reach here
	p.commentPhase = 0
	p.commentType = 0
	return p.Next()
}

// continueBlockComment handles Body and End phases for block comments
func (p *Tokenizer) continueBlockComment() (Token, error) {
	switch p.commentPhase {
	case 1: // Begin was emitted, now emit Body
		p.state = TokenizerStateBlockCommentBody
		p.commentPhase = 2
		// Read content until we find |# at depth 0 or EOF
		for p.err == nil {
			if p.curr() == '#' {
				p.next()
				if p.err != nil {
					break
				}
				if isVerticalLine(p.curr()) {
					// Nested #|
					p.blockDepth++
					p.next()
				}
			} else if isVerticalLine(p.curr()) {
				// Check for |#
				p.next()
				if p.err != nil {
					break
				}
				if p.curr() == '#' {
					if p.blockDepth == 0 {
						// Found closing |# - remove the | from scratch and stop
						if len(p.scratch) > 0 {
							p.scratch = p.scratch[:len(p.scratch)-1]
						}
						p.term()
						return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix), nil
					}
					p.blockDepth--
					p.next()
				}
			} else {
				p.next()
			}
		}
		// EOF before closing - emit Body token, no End will follow
		p.term()
		p.commentPhase = 0
		p.commentType = 0
		return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix), nil

	case 2: // Body was emitted, now emit End
		p.state = TokenizerStateBlockCommentEnd
		p.commentPhase = 0
		p.commentType = 0
		p.blockDepth = 0
		// The | was consumed during Body phase but trimmed from scratch
		// We need to manually add it back for the End token
		p.scratch = append(p.scratch, '|')
		// Now consume the #
		if p.err == nil && p.curr() == '#' {
			p.next()
		}
		p.term()
		return NewSimpleToken(p.state, p.text, "", &p.tokenStart, &p.tokenEnd, p.negative, p.signed, p.radix), nil
	}

	// Should not reach here
	p.commentPhase = 0
	p.commentType = 0
	return p.Next()
}

// isCommentToken returns true if the state represents a comment token
func isCommentToken(state TokenizerState) bool {
	switch state {
	case TokenizerStateLineCommentBegin, TokenizerStateLineCommentBody, TokenizerStateLineCommentEnd,
		TokenizerStateBlockCommentBegin, TokenizerStateBlockCommentBody, TokenizerStateBlockCommentEnd,
		TokenizerStateDatumCommentBegin:
		return true
	}
	return false
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
	case p.curr() == ')':
		p.state = TokenizerStateCloseParen
		p.next()
		p.term()
	case isDot(p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(false, 10) //nolint:errcheck
		p.term()
	case p.curr() == '\'':
		p.state = TokenizerStateQuote
		p.next()
		p.term()
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
	case p.curr() == '`':
		p.state = TokenizerStateQuasiquote
		p.next()
		p.term()
	case p.curr() == '"':
		p.state = TokenizerStateStringStart
		p.next() // skip "
		if p.err != nil {
			p.term()
			return
		}
		p.readString() //nolint:errcheck
		p.term()
	case p.curr() == ';':
		if p.emitComments {
			// Emit LineCommentBegin, set up for Body/End on subsequent calls
			p.state = TokenizerStateLineCommentBegin
			p.commentType = 1 // line comment
			p.commentPhase = 1
			// Consume all leading semicolons
			for p.err == nil && p.curr() == ';' {
				p.next()
			}
		} else {
			// Old behavior: return single token with all comment content
			p.state = TokenizerStateLineCommentStart
			p.next()
			p.readLineCommentOrPragma() //nolint:errcheck
		}
		p.term()
	case isVerticalLine(p.curr()):
		p.state = TokenizerStateExtendedSymbolStart
		p.next()
		p.term()
	case p.curr() == '#':
		p.next()
		if p.err != nil {
			p.term()
			return
		}
		p.readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment() //nolint:errcheck
		p.term()
	case isExplicitSign(p.curr()):
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(false, 10) //nolint:errcheck
		p.term()
	case isDigit(10, p.curr()):
		p.state = TokenizerStateIntegerBase10
		p.readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(false, 10) //nolint:errcheck
		p.term()
	case isSymbolInitial(p.curr()): // read symbol
		p.state = TokenizerStateSymbol
		p.next()
		if p.err != nil {
			p.term()
			return
		}
		p.readSymbol() //nolint:errcheck
		p.term()
	default:
		p.term()
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingToken, p.tokenStart, p.tokenEnd)
	}
}

// skip consumes up to maxn decimal digits, returning the count consumed.
func (p *Tokenizer) skip(maxn int, c rune) int {
	n := 0
	for p.err == nil && isDigit(10, p.curr()) && n < maxn {
		p.next()
		n++
	}
	return n
}

// readToEndOfLine skips intraline whitespace and consumes the line ending.
// Returns an error if no newline is found.
func (p *Tokenizer) readToEndOfLine() error {
	for p.err == nil && isIntralineWhitespace(p.curr()) {
		p.next()
	}
	if p.err != nil {
		return p.err
	}
	// capture '\n' or '\r', '\n' or '\r'
	switch {
	case p.curr() == '\n':
		p.next()
		fallthrough
	case p.curr() == '\r':
		p.next()
	default:
		// err
		p.err = NewTokenizerError("expected newline", p.tokenStart, p.tokenEnd)
		return p.err
	}
	return nil
}

// readIntraStringEscape handles escape sequences within strings.
// Recognizes: \a, \b, \t, \n, \r, \\, \", \xNN (hex escape), and whitespace.
func (p *Tokenizer) readIntraStringEscape() (rune, error) {
	switch {
	case p.curr() == 'a': // \a (??)
		p.value = p.value + "\a"
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == 'b': // \b (backspace)
		p.value = p.value + "\b"
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == 't': // \t (tab)
		p.value = p.value + "\t"
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == 'n': // \n (newline)
		p.value = p.value + "\n"
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == 'r': // \r (return)
		p.value = p.value + "\r"
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == '\\': // \ (forward slash)
		p.value = p.value + string(p.curr())
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == '"': // " (double quote)
		p.value = p.value + string(p.curr())
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case isIntralineWhitespace(p.curr()):
		p.value = p.value + string(p.curr())
		p.next()
		if p.err != nil {
			return 0, p.err
		}
	case p.curr() == 'x':
		p.next()
		if p.err != nil {
			return 0, p.err
		}
		x, _ := p.readBaseNInteger(16, 8)
		if p.err != nil {
			return 0, p.err
		}
		p.value = p.value + string(rune(x))
	default:
		// error
		p.err = NewTokenizerError(MessageExpectingEscape, p.tokenStart, p.tokenEnd)
		return 0, p.err
	}
	return 0, nil
}

// readString reads a string literal until the closing double-quote.
// Handles escape sequences via readIntraStringEscape.
// Builds the processed string content in p.value (without quotes, with escapes converted).
func (p *Tokenizer) readString() error {
	p.state = TokenizerStateStringSpan
	p.value = "" // Reset processed value for this string
	for p.curr() != '"' {
		if p.curr() == '\\' {
			p.state = TokenizerStateStringIntraEscape
			p.next() // skip \
			if p.err != nil {
				return p.err
			}
			p.readIntraStringEscape() //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			continue // escape handler already consumed the next char
		}
		// Regular character - add to processed value
		p.value = p.value + string(p.curr())
		p.next()
		if p.err != nil {
			return p.err
		}
	}
	p.next() // skip "
	p.state = TokenizerStateStringEnd
	return nil
}

// skipComment is a no-op placeholder for potential future comment skipping.
func (p *Tokenizer) skipComment() error {
	return nil
}

// skipWhitespace consumes whitespace characters (spaces, tabs, newlines).
func (p *Tokenizer) skipWhitespace() error {
	for p.err == nil && (isIntralineWhitespace(p.curr()) || isLineEnding(p.curr())) {
		p.next()
	}
	return p.err
}

// skipLine consumes all characters until and including the line ending.
func (p *Tokenizer) skipLine() error {
	for p.err == nil && !isLineEnding(p.curr()) {
		p.next()
	}
	if p.err == nil && isLineEnding(p.curr()) {
		p.scanLineEnding() //nolint:errcheck
	}
	return p.err
}

// readPeculiarIdentifier reads a peculiar identifier (e.g., ..., +, -, ->).
// Peculiar identifiers can contain dots, signs, and letters.
func (p *Tokenizer) readPeculiarIdentifier() error {
	for p.err == nil && (isDot(p.curr()) || isExplicitSign(p.curr()) || isLetter(p.curr())) {
		p.next()
	}
	return p.err
}

// readLineCommentOrPragma reads a line comment starting with one or more semicolons.
// Consumes all characters until (but not including) the line ending.
func (p *Tokenizer) readLineCommentOrPragma() error {
	for p.err == nil && p.curr() == ';' {
		p.next()
	}
	for p.err == nil && !isLineEnding(p.curr()) {
		p.next()
	}
	return p.err
}

// readBlockComment reads a block comment #|...|# (non-nesting version).
// Called after the opening #| has been consumed.
func (p *Tokenizer) readBlockComment() error {
	for p.err == nil {
		for p.err == nil && !isVerticalLine(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
		if p.curr() == '#' {
			p.next() // consume
			return nil
		}
		p.next() // consume
		if p.err != nil {
			return p.err
		}
	}
	return p.err
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
func (p *Tokenizer) readVectorOrExactnessOrRadixOrModifierOrMnemonicOrBooleanOrComment() error {
	switch {
	case p.curr() == '\'': // #' (syntax)
		p.state = TokenizerStateSyntax
		p.next()
		if p.err != nil {
			return p.err
		}
	case p.curr() == ',': // #' (unsyntax) (unsyntax-splice)
		p.state = TokenizerStateUnsyntax
		p.next()
		if p.err != nil || p.curr() != '@' {
			return p.err
		}
		p.state = TokenizerStateUnsyntaxSplicing
		p.next()
		if p.err != nil {
			return p.err
		}
	case p.curr() == '`': // #' (quasisyntax)
		p.state = TokenizerStateQuasisyntax
		p.next()
		if p.err != nil {
			return p.err
		}
	case isVerticalLine(p.curr()):
		if p.emitComments {
			// Emit BlockCommentBegin, set up for Body/End on subsequent calls
			p.state = TokenizerStateBlockCommentBegin
			p.commentType = 2 // block comment
			p.commentPhase = 1
			p.blockDepth = 0
			// Consume the |
			p.next()
		} else {
			// Old behavior: return single token with all comment content
			p.state = TokenizerStateBlockComment
			p.next()
			p.readBlockComment() //nolint:errcheck
		}
	case isLetter(p.curr()): // #u8(, #true, #false ... unsigned byte array
		return p.readTypedArrayOrExactnessOrRadixOrBooleanMarker()
	case isDigit(10, p.curr()):
		// label: #[0-9]+# #[0-9]+[=]value#
		p.state = TokenizerStateLabelReference
		p.next()
		for p.err == nil && isDigit(10, p.curr()) {
			p.next()
		}
		switch p.curr() {
		case '=':
			p.state = TokenizerStateLabelAssignment
		case '#':
			// #1234#
			// label reference
			p.state = TokenizerStateLabelReference
		}
		if p.err != nil {
			return p.err
		}
		p.next()
		if p.err != nil {
			return p.err
		}
	case p.curr() == '\\': // #\ character escape or character name
		p.state = TokenizerStateCharMnemonicOrHexEscape
		p.next()
		if p.err != nil {
			return p.err
		}
		return p.readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape()
	case p.curr() == '(': // #( array
		p.state = TokenizerStateOpenVector
		p.next()
		if p.err != nil {
			return p.err
		}
	case p.curr() == ';': // datum comment
		if p.emitComments {
			// Emit DatumCommentBegin - parser handles datum boundary detection
			p.state = TokenizerStateDatumCommentBegin
		} else {
			// Old behavior: return single datum comment token
			p.state = TokenizerStateDatumComment
		}
		p.next()
		if p.err != nil {
			return p.err
		}
	case p.curr() == '!': // #! array
		p.state = TokenizerStateDirective
		return p.readDirective()
	default:
		// error
		panic("error")
	}
	return nil
}

// readTypedArrayOrExactnessOrRadixOrBooleanMarker parses letter-prefixed # tokens.
// Handles: #t/#true, #f/#false, #u8(, #i, #e, #b, #o, #d, #x.
// Called when curr() is a letter following '#'.
// Note: R7RS requires booleans to be case-insensitive, so #T, #TRUE, #F, #FALSE are valid.
func (p *Tokenizer) readTypedArrayOrExactnessOrRadixOrBooleanMarker() error {
	var k int
	switch {
	case p.curr() == 't' || p.curr() == 'T': // #true, #t, #TRUE, #T (R7RS: case-insensitive)
		k, _ = p.scanCaseInsensitive([]byte("true"))
		if p.err != nil && p.err != io.EOF {
			// #t is a valid boolean
			return p.err
		}
		if (k == 0 || k == 3) && (isDelimiter(p.curr()) || p.err == io.EOF) {
			p.state = TokenizerStateMarkerBooleanTrue
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'f' || p.curr() == 'F': // #false, #f, #FALSE, #F (R7RS: case-insensitive)
		k, _ = p.scanCaseInsensitive([]byte("false"))
		if p.err != nil && p.err != io.EOF {
			// #f is a valid boolean
			return p.err
		}
		if (k == 0 || k == 4) && (isDelimiter(p.curr()) || p.err == io.EOF) {
			p.state = TokenizerStateMarkerBooleanFalse
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
		return nil
	case p.curr() == 'u': // #u8( .. unsigned with 8, 16, 32, 64, 128
		p.next()
		if p.err != nil {
			return NewTokenizerErrorWithWrap(p.err, "invalid word size marker.", p.tokenStart, p.tokenEnd)
		}
		// u8 is only supported
		if p.curr() != '8' {
			return ErrNotAnUnsignedByteMarker
		}
		p.next()
		if p.err != nil {
			return p.err
		}
		if p.curr() == '(' {
			p.state = TokenizerStateOpenVectorUnsignedByteMarker
			p.next()
			if p.err != nil {
				return p.err
			}
			return nil
		}
	case p.curr() == 'i': // inexact, #i
		p.next() // skip i
		// subsequent [0-9]+
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(p.radix, p.curr()) {
			p.state = TokenizerStateMarkerNumberInexact
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'e': // exact, #e
		// subsequent [0-9]+
		p.next()
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(p.radix, p.curr()) {
			p.state = TokenizerStateMarkerNumberExact
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'b': // binary, #b
		// subsequent [0-1]+
		p.radix = 2
		p.next()
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(2, p.curr()) {
			p.state = TokenizerStateMarkerBase2
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'o': // octal, #o
		// subsequent [0-7]+
		p.radix = 8
		p.next()
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(8, p.curr()) {
			p.state = TokenizerStateMarkerBase8
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'd': // decimal, #d
		// subsequent [0-9]+
		p.radix = 10
		p.next()
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(10, p.curr()) {
			p.state = TokenizerStateMarkerBase10
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'x': // hex #x
		// subsequent [0-9a-fA-F]+
		p.radix = 16
		p.next()
		if p.err != nil || isDelimiter(p.curr()) || isNumberInitial(16, p.curr()) {
			p.state = TokenizerStateMarkerBase16
			return p.err
		}
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	case p.curr() == 'm' || p.curr() == 'M': // big integer #m
		p.next()
		if p.err != nil {
			return p.err
		}
		// Read the number (digits only for now, supports sign)
		if p.curr() == '-' || p.curr() == '+' {
			p.next()
			if p.err != nil {
				return p.err
			}
		}
		// Read digits
		for p.err == nil && isDigit(10, p.curr()) {
			p.next()
		}
		if p.err != nil && p.err != io.EOF {
			return p.err
		}
		p.state = TokenizerStateBigInteger
	case p.curr() == 'z' || p.curr() == 'Z': // big float #z
		p.next()
		if p.err != nil {
			return p.err
		}
		// Read the number (supports sign, decimal point, exponent)
		if p.curr() == '-' || p.curr() == '+' {
			p.next()
			if p.err != nil {
				return p.err
			}
		}
		// Read integer part
		for p.err == nil && isDigit(10, p.curr()) {
			p.next()
		}
		// Check for decimal point
		if p.err == nil && p.curr() == '.' {
			p.next()
			// Read fractional part
			for p.err == nil && isDigit(10, p.curr()) {
				p.next()
			}
		}
		// Check for exponent
		if p.err == nil && (p.curr() == 'e' || p.curr() == 'E') {
			p.next()
			if p.err == nil && (p.curr() == '-' || p.curr() == '+') {
				p.next()
			}
			for p.err == nil && isDigit(10, p.curr()) {
				p.next()
			}
		}
		if p.err != nil && p.err != io.EOF {
			return p.err
		}
		p.state = TokenizerStateBigFloat
	default:
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		if p.err != nil {
			return p.err
		}
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

// readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape parses character literals.
// Handles: #\x1234 (hex), #\newline (mnemonic), #\a (graphic character).
// Called after #\ has been consumed.
func (p *Tokenizer) readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape() error {
	// #\backspace, #\ffff, ...
	p.state = TokenizerStateCharMnemonicOrHexEscape
	if p.curr() == 'x' { // #\xFFFF
		p.state = TokenizerStateCharHexEscape
		p.next()
		if p.err != nil {
			return p.err
		}
		p.readBaseNInteger(16, 4) //nolint:errcheck
		return p.err
	} else if isLetter(p.curr()) {
		p.state = TokenizerStateCharGraphic
		p.next() // skip letter
		if p.err != nil || !isLetter(p.curr()) {
			return p.err
		}
		p.state = TokenizerStateCharMnemonic
		if p.err == nil && (isLetter(p.curr()) || isNegativeSign(p.curr())) {
			p.next()
			for p.err == nil && (isLetter(p.curr()) || isDigit(10, p.curr()) || isNegativeSign(p.curr())) {
				p.next() // skip letter
			}
		}
		return p.err
	} else if unicode.IsGraphic(p.curr()) {
		p.state = TokenizerStateCharGraphic
		p.next() // skip graphic letter
		return p.err
	}
	return NewTokenizerError("expecting character mnemonic or hex escape.", p.tokenStart, p.tokenEnd)
}

// readRadixPrefixOrExactness reads a radix or exactness prefix sequence.
// Consumes letters and digits following the prefix marker.
func (p *Tokenizer) readRadixPrefixOrExactness() error {
	// #i, #d, ...
	p.state = TokenizerStateCharMnemonicOrHexEscape
	for p.err == nil && (isLetter(p.curr()) || isDigit(10, p.curr())) {
		p.next()
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

// readDirective reads a #! directive (e.g., #!fold-case, #!no-fold-case).
// Called after '#' has been consumed with '!' as curr().
func (p *Tokenizer) readDirective() error {
	// #!backspace, #!b, ...
	// drop the '!'
	p.next()
	if p.err != nil {
		return NewTokenizerError("expecting directive.", p.tokenStart, p.tokenEnd)
	}
	// traditional identifier - letter followed by letter or number or dash
	if isLetter(p.curr()) {
		p.next()
	} else {
		return NewTokenizerError("expecting directive.", p.tokenStart, p.tokenEnd)
	}
	for p.err == nil && (isLetter(p.curr()) || isDigit(10, p.curr()) || isNegativeSign(p.curr())) {
		p.next()
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

// readCons reads a cons dot (.) for improper list notation.
func (p *Tokenizer) readCons() error {
	p.state = TokenizerStateCons
	// consume '.'
	p.next()
	if p.err != nil {
		return p.err
	}
	return nil
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
func (p *Tokenizer) readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(e bool, r int) error {
	var n int
	// Branch 1: Starts with explicit sign (+/-)
	if !p.isEOF() && isExplicitSign(p.curr()) {
		p.signed = true
		if isNegativeSign(p.curr()) {
			p.negative = true
		}
		p.state = TokenizerStateSymbol
		p.next() // skip + -
		if p.err != nil {
			return p.err
		}
		if p.curr() == 'i' { // +inf.0, +i, -i
			p.state = TokenizerStateSignedImaginary
			p.next()
			if p.err != nil {
				return p.err
			}
			if p.curr() == 'n' { // +inf.0
				p.state = TokenizerStateSignedInf
				n, _ = p.scan([]byte("nf"))
				if p.err != nil {
					return p.err
				}
				if n != 0 {
					return NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
				}
				if !isDot(p.curr()) { // +inf.0
					return NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
				}
				p.next()
				if p.err != nil {
					return p.err
				}
				p.mustReadUnsignedInteger(r) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
				if isExplicitSign(p.curr()) {
					p.state = TokenizerStateSignedComplex
					p.next()
					if p.err != nil {
						return p.err
					}
					// +inf.0+2i - complex with signed decimal real
					p.mayUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
					if p.err != nil {
						return p.err
					}
					if isImaginary(p.curr()) { // +inf.0i
						p.next()
						if p.err != nil {
							return p.err
						}
					}
					return nil
				}
				if isImaginary(p.curr()) { // +inf.0i
					p.state = TokenizerStateSignedImaginaryInf
					p.next()
					if p.err != nil {
						return p.err
					}
				}
			} else { // +i -i
				// TODO +i -i
				p.state = TokenizerStateSignedImaginary
			}
		} else if p.curr() == 'n' { // +nan.0
			n, _ = p.scan([]byte("nan"))
			if p.err != nil {
				return p.err
			}
			if n != 0 {
				for p.err == nil && isSignSubsequent(p.curr()) {
					p.next()
				}
			}
			if !isDot(p.curr()) {
				p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
				return p.err
			}
			p.next()
			if p.err != nil {
				return p.err
			}
			if !isDigit(r, p.curr()) {
				p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
				return p.err
			}
			p.state = TokenizerStateSignedNan
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if isImaginary(p.curr()) {
				p.state = TokenizerStateSignedImaginaryNan
				p.next()
				if p.err != nil {
					return p.err
				}
			}
		} else if isSignSubsequent(p.curr()) { // ++++
			p.state = TokenizerStateSymbol
			// TODO Symbol
			for p.err == nil && isSymbolSubsequent(p.radix, p.curr()) {
				p.next()
			}
			if p.err != nil {
				return p.err
			}
		} else if isDot(p.curr()) { // +.10
			p.next()
			if p.err != nil {
				return p.err
			}
			if isDotSubsequent(p.curr()) {
				p.state = TokenizerStateSymbol
				// TODO Symbol
				p.next()
				if p.err != nil {
					return p.err
				}
				p.readSymbol() //nolint:errcheck
			} else if !isDigit(r, p.curr()) { // +.10
				// TODO Peculiar identifier
				p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
				return p.err
			} else {
				p.state = TokenizerStateSignedDecimalFraction
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if isImaginary(p.curr()) { // +.10i - signed imaginary with coefficient
				p.state = TokenizerStateSignedImaginary
				p.next()
				if p.err != nil && p.err != io.EOF {
					return p.err
				}
			} else if isComplexPolar(p.curr()) { // +.10@1.5708 - polar form with signed decimal magnitude
				if err := p.mayReadPolarPart(r); err != nil {
					return err
				}
			} else if isExplicitSign(p.curr()) { // +.10+2i - complex with signed decimal real
				if err := p.mayReadSignedImaginaryPart(r); err != nil {
					return err
				}
			}
		} else if isDigit(r, p.curr()) {
			p.state = TokenizerStateSignedInteger
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if isDot(p.curr()) { // +10.
				p.state = TokenizerStateSignedDecimalFraction
				p.next()
				if p.err != nil {
					return p.err
				}
				// Digits after dot are optional if followed by exponent (R7RS: <digit>+ . <digit>* <suffix>)
				if isDigit(r, p.curr()) { // +10.00
					p.readBaseNInteger(r, 0) //nolint:errcheck
					if p.err != nil {
						return p.err
					}
				} else if !isExponent(p.curr()) {
					return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
				}
				p.mayReadExponent(r) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			} else if p.curr() == '/' { // +10/10
				p.state = TokenizerStateSignedRationalFraction
				p.next()
				if p.err != nil {
					return p.err
				}
				if !isDigit(r, p.curr()) { // +10/10
					return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
				}
				p.readBaseNInteger(r, 0) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			} else if isExponent(p.curr()) { // +1e10, -1e10
				p.state = TokenizerStateSignedDecimalFraction
				p.mayReadExponent(r) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			}
			// Check for trailing 'i' to make this an imaginary number (+3i, +3.5i)
			if isImaginary(p.curr()) {
				p.state = TokenizerStateSignedImaginary
				p.next()
				if p.err != nil {
					return p.err
				}
			} else if isExplicitSign(p.curr()) { // -1+2i, +3-4i - complex with signed real
				p.state = TokenizerStateSignedComplex
				p.mayReadSignedImaginaryPart(r) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			} else if isComplexPolar(p.curr()) { // -1@1.5708 - polar form with signed magnitude
				p.state = TokenizerStateSignedComplexPolar
				p.mayReadPolarPart(r) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			}
		}
	} else if !p.isEOF() && isDot(p.curr()) { // default sign: .10, cons: '.', ellipsis: '...'
		p.state = TokenizerStateCons
		// consume '.'
		p.next()
		if p.err != nil {
			return p.err
		}
		if isDotSubsequent(p.curr()) {
			p.state = TokenizerStateSymbol
			// TODO Symbol
			// consume '.' or
			p.next()
			if p.err != nil {
				return p.err
			}
			for p.err == nil && isSubsequent(p.curr()) {
				p.next()
			}
			if p.err != nil {
				return p.err
			}
		} else if isDigit(r, p.curr()) {
			p.state = TokenizerStateUnsignedDecimalFraction
			p.next()
			if p.err != nil {
				return p.err
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		}
	} else if isDigit(r, p.curr()) { // 1.2, 1/2 12
		p.state = TokenizerStateUnsignedInteger
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
		if p.curr() == '/' { // 10/10
			p.state = TokenizerStateUnsignedRationalFraction
			p.next()
			if p.err != nil {
				return p.err
			}
			if !isDigit(r, p.curr()) {
				p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
				return p.err
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		} else if isDot(p.curr()) { // 10.10
			p.state = TokenizerStateUnsignedDecimalFraction
			p.next()
			if p.err != nil {
				return p.err
			}
			// Digits after dot are optional if followed by exponent (R7RS: <digit>+ . <digit>* <suffix>)
			if isDigit(r, p.curr()) {
				p.readBaseNInteger(r, 0) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			} else if !isExponent(p.curr()) {
				p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
				return p.err
			}
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		} else if isExponent(p.curr()) { // 000e+10
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		}
		// Check for trailing 'i' to make this an imaginary number (+3i, +3.5i)
		if isImaginary(p.curr()) {
			p.state = TokenizerStateUnsignedImaginary
			p.next()
			if p.err != nil {
				return p.err
			}
			// Check for complex number suffix: <real>+<ureal>i or <real>-<ureal>i
		} else if isExplicitSign(p.curr()) {
			p.state = TokenizerStateUnsignedComplex
			p.next()                                                 // skip the sign
			p.mayUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if !isImaginary(p.curr()) {
				p.err = NewTokenizerError(MessageExpectingImaginary, p.tokenStart, p.tokenEnd)
				return p.err
			}
			p.next()
		} else if isComplexPolar(p.curr()) { // <real>@<real> - polar form
			p.state = TokenizerStateUnsignedComplexPolar
			p.mayReadPolarPart(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		}
	} else {
		p.err = NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
		return p.err
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

func (p *Tokenizer) mayUnsignedFractionalRealNumberOrRationalRealNumber(r int) error {
	var n int
	// Branch 1: Starts with explicit sign (+/-)
	if p.curr() == 'i' { // +inf.0
		n, _ = p.scan([]byte("inf"))
		if p.err != nil {
			return p.err
		}
		if n != 0 {
			return NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
		}
		if !isDot(p.curr()) { // +inf.0
			return NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
		}
		p.next()
		if p.err != nil {
			return p.err
		}
		p.mustReadUnsignedInteger(r) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
	} else if p.curr() == 'n' { // +nan.0
		n, _ = p.scan([]byte("nan"))
		if p.err != nil {
			return p.err
		}
		if n != 0 {
			for p.err == nil && isSignSubsequent(p.curr()) {
				p.next()
			}
		}
		if !isDot(p.curr()) {
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return p.err
		}
		p.next()
		if p.err != nil {
			return p.err
		}
		if !isDigit(r, p.curr()) {
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return p.err
		}
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
	} else if isDot(p.curr()) { // +.10
		p.next()
		if p.err != nil {
			return p.err
		}
		if isDotSubsequent(p.curr()) {
			// TODO Symbol
			p.next()
			if p.err != nil {
				return p.err
			}
			p.readSymbol() //nolint:errcheck
		} else if !isDigit(r, p.curr()) { // +.10
			// TODO Peculiar identifier
			p.err = NewTokenizerError(MessageExpectingDecimalFraction, p.tokenStart, p.tokenEnd)
			return p.err
		}
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
		p.mayReadExponent(r) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
	} else if isDigit(r, p.curr()) {
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
		if isDot(p.curr()) { // +10.
			p.next()
			if p.err != nil {
				return p.err
			}
			if !isDigit(r, p.curr()) { // +10.00
				return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		} else if isExponent(p.curr()) { // 000e+10
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		} else if p.curr() == '/' { // +10/10
			p.next()
			if p.err != nil {
				return p.err
			}
			if !isDigit(r, p.curr()) { // +10/10
				return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		}
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

// mayReadDotMustUnsignedNumber reads a dot followed by unsigned digits if present.
// Returns true if digits were read, false if no dot was found.
func (p *Tokenizer) mayReadDotMustUnsignedNumber(r int) (bool, error) {
	if !isDot(p.curr()) {
		return true, nil
	}
	return p.mustReadUnsignedInteger(r)
}

// mayReadExponent reads an optional exponent part (e.g., e10, E-5).
// Returns true if an exponent was found and consumed.
func (p *Tokenizer) mayReadExponent(r int) (bool, error) {
	if !isExponent(p.curr()) { // +.10e5
		return false, nil
	}
	p.next() // consume 'E'
	if p.err != nil {
		return false, p.err
	}
	ok, _ := p.mayReadSignedInteger(r)
	if p.err != nil {
		return false, p.err
	}
	if ok {
		return true, nil
	}
	return p.mustReadUnsignedInteger(r)
}

// mayReadSignedImaginaryPart reads an optional imaginary part for complex numbers.
// Called when current character is '+' or '-' after reading a real number.
// Handles patterns like: +3i, +3.5i, +i, -2i, -inf.0i, -nan.0i
// The complexState parameter specifies which state to set on success
// (SignedComplex or UnsignedComplex depending on whether the real part was signed).
func (p *Tokenizer) mayReadSignedImaginaryPart(r int) error {
	if !isExplicitSign(p.curr()) {
		return nil
	}
	// Save position in case we need to backtrack (but we don't support backtracking,
	// so we commit once we see the sign followed by valid imaginary syntax)
	p.next() // consume '+' or '-'
	if p.err != nil {
		return p.err
	}

	// Check for +i or -i (pure imaginary unit) or +inf or -inf
	if p.curr() == 'i' {
		p.next()
		if p.err != nil {
			return p.err
		}
		// Check for +inf.0i or -inf.0i
		n, _ := p.scan([]byte("nf"))
		if p.err != nil {
			return p.err
		}
		if n == 0 && isDot(p.curr()) {
			p.next()
			if p.err != nil {
				return p.err
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if isImaginary(p.curr()) {
				p.next()
				if p.err != nil && p.err != io.EOF {
					return p.err
				}
			}
		}
		return nil
	} else if p.curr() == 'n' {
		// Check for +nan.0i or -nan.0i
		n, _ := p.scan([]byte("nan"))
		if p.err != nil {
			return p.err
		}
		if n == 0 && isDot(p.curr()) {
			p.next()
			if p.err != nil {
				return p.err
			}
			p.readBaseNInteger(r, 0) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
			if isImaginary(p.curr()) {
				p.next()
				if p.err != nil && p.err != io.EOF {
					return p.err
				}
			}
		}
		return nil
	}

	// Check for numeric coefficient: +3i, +3.5i, etc.
	if isDigit(r, p.curr()) {
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
		// Check for decimal part
		if isDot(p.curr()) {
			p.next()
			if p.err != nil {
				return p.err
			}
			if isDigit(r, p.curr()) {
				p.readBaseNInteger(r, 0) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			}
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		} else {
			p.mayReadExponent(r) //nolint:errcheck
			if p.err != nil {
				return p.err
			}
		}
		// Must end with 'i'
		if isImaginary(p.curr()) {
			p.next()
			if p.err != nil && p.err != io.EOF {
				return p.err
			}
		}
		return nil
	}

	return nil
}

// mayReadPolarPart reads an optional polar angle part for complex numbers.
// Called when current character is '@' after reading a real number (the magnitude).
// Handles patterns like: @1.5708, @0.785, @-1.5708
// If successful, sets state to TokenizerStateUnsignedComplexPolar.
func (p *Tokenizer) mayReadPolarPart(r int) error {
	if p.curr() != '@' {
		return nil
	}
	p.next() // consume '@'
	if p.err != nil {
		return p.err
	}

	// The angle can be a signed or unsigned real number
	// Check for explicit sign first
	if isExplicitSign(p.curr()) {
		p.next() // consume '+' or '-'
		if p.err != nil {
			return p.err
		}
	}

	// Must have digits or a dot followed by digits
	if isDot(p.curr()) {
		p.next() // consume '.'
		if p.err != nil {
			return p.err
		}
		if !isDigit(r, p.curr()) {
			return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
		}
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
	} else if isDigit(r, p.curr()) {
		p.readBaseNInteger(r, 0) //nolint:errcheck
		if p.err != nil {
			return p.err
		}
		// Check for decimal part
		if isDot(p.curr()) {
			p.next()
			if p.err != nil {
				return p.err
			}
			if isDigit(r, p.curr()) {
				p.readBaseNInteger(r, 0) //nolint:errcheck
				if p.err != nil {
					return p.err
				}
			}
		}
	} else {
		return NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
	}

	// Check for exponent
	p.mayReadExponent(r) //nolint:errcheck
	if p.err != nil && p.err != io.EOF {
		return p.err
	}

	p.state = TokenizerStateUnsignedComplexPolar
	return nil
}

// mayReadSignedInteger reads an optional signed integer (+N or -N).
// Returns true if a sign was found and digits were consumed.
func (p *Tokenizer) mayReadSignedInteger(r int) (bool, error) {
	if !isExplicitSign(p.curr()) && p.err == nil {
		return false, nil
	}
	p.next() // consume '+'/'-'
	if p.err != nil {
		return false, p.err
	}
	return p.mustReadUnsignedInteger(r)
}

// mustReadUnsignedInteger reads one or more digits in the given radix.
// Returns an error if no digits are found.
func (p *Tokenizer) mustReadUnsignedInteger(r int) (bool, error) {
	if !isDigit(r, p.curr()) {
		return false, NewTokenizerError(MessageExpectingNumber, p.tokenStart, p.tokenEnd)
	}
	p.next()
	for p.err == nil && isDigit(r, p.curr()) {
		p.next()
	}
	return true, p.err
}

// readSymbolOrNumber reads either a symbol or a number based on the current character.
// Parameter s indicates if a sign was already consumed, r is the radix.
func (p *Tokenizer) readSymbolOrNumber(s bool, r int) error {
	if isLetter(p.curr()) || isSymbolSubsequent(r, p.curr()) {
		p.state = TokenizerStateSymbol
		return p.readSymbol()
	} else if isNumberInitial(r, p.curr()) {
		if s {
			p.state = TokenizerStateSignedInteger
		} else {
			p.state = TokenizerStateUnsignedInteger
		}
		_, err := p.readBaseNInteger(r, 18)
		return err
	}
	return ErrNotALiteral
}

// readSymbol consumes subsequent characters that are valid in a symbol.
// Called after the initial character has been read.
func (p *Tokenizer) readSymbol() error {
	for p.err == nil && isSymbolSubsequent(10, p.curr()) {
		p.next()
	}
	if p.err != nil {
		return p.err
	}
	return nil
}

// readBaseNInteger reads up to maxn digits in the given base.
// Returns the parsed integer value. If maxn <= 0, no limit is applied.
func (p *Tokenizer) readBaseNInteger(base int, maxn int) (int64, error) {
	n := 0
	var s string
	for p.err == nil && (isDigit(base, p.curr()) && (maxn <= 0 || n < maxn)) {
		s = s + string(p.curr())
		p.next()
		n++
	}
	if p.err != nil {
		return 0, p.err
	}
	q, err := strconv.ParseInt(s, base, 64)
	if err != nil {
		return 0, NewTokenizerErrorWithWrap(err, MessageExpectingNumber, p.tokenStart, p.tokenEnd)
	}
	return q, nil
}

// scan attempts to match the given byte sequence against the input.
// Returns the number of unmatched bytes (0 = complete match).
// Consumes runes from input as they match.
func (p *Tokenizer) scan(s []byte) (int, error) {
	k := len(s)
	i := 0
	for {
		r, n := utf8.DecodeRune(s[i:])
		if n == 0 && r == utf8.RuneError {
			return k, nil
		}
		i += n
		if !p.ci {
			if p.curr() != r {
				return k, nil
			}
		} else {
			if unicode.ToUpper(p.curr()) != r && unicode.ToLower(p.curr()) != r {
				return k, nil
			}
		}
		k--
		p.next()
		if p.err != nil {
			return k, p.err
		}
	}
}

// scanCaseInsensitive matches bytes case-insensitively (always, regardless of p.ci).
// Used for R7RS-required case-insensitive tokens like booleans.
// Returns the number of unmatched bytes (0 = complete match).
func (p *Tokenizer) scanCaseInsensitive(s []byte) (int, error) {
	k := len(s)
	i := 0
	for {
		r, n := utf8.DecodeRune(s[i:])
		if n == 0 && r == utf8.RuneError {
			return k, nil
		}
		i += n
		// Always case-insensitive: compare both upper and lower case
		if unicode.ToLower(p.curr()) != unicode.ToLower(r) {
			return k, nil
		}
		k--
		p.next()
		if p.err != nil {
			return k, p.err
		}
	}
}

// readToken is similar to scan but used for token matching.
// Returns the number of unmatched bytes (0 = complete match).
func (p *Tokenizer) readToken(s []byte) (int, error) {
	k := len(s)
	i := 0
	for {
		r, n := utf8.DecodeRune(s[i:])
		if n == 0 && r == utf8.RuneError {
			return k, nil
		}
		i += n
		if p.curr() != r {
			return k, nil
		}
		k--
		p.next()
		if p.err != nil {
			return k, p.err
		}
	}
}

// readArrayWordLength reads up to 3 digits for array word length specification.
func (p *Tokenizer) readArrayWordLength() error {
	i := 0
	if p.err == nil && p.curr() >= 0 && p.curr() <= '9' && i < 3 {
		p.next()
		i++
	}
	return nil
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

// scanIntermediateToken reads letters and dashes, returning the count of characters consumed.
// Used for scanning multi-character tokens like character names.
func (p *Tokenizer) scanIntermediateToken() (int, error) {
	i := 0
	for p.err == nil && (isLetter(p.curr()) || isNegativeSign(p.curr())) {
		i++
		p.next()
	}
	if p.err != nil {
		return i, p.err
	}
	return i, p.err
}

// scanLineEnding consumes a line ending (\n, \r, or \r\n).
// Returns true if a line ending was consumed.
func (p *Tokenizer) scanLineEnding() (bool, error) {
	switch p.curr() {
	case '\n':
		p.next()
		return true, p.err
	case '\r':
		p.next() // consume '\r'
		if p.err != nil {
			return true, p.err
		}
		if p.curr() != '\n' {
			return true, nil
		}
		p.next() // consume '\n'
		if p.err != nil {
			return true, p.err
		}
		return true, nil
	}
	return false, nil
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
	p.scratch = p.scratch[:0]
	p.tokenStart = p.runeStart
	p.tokenEnd = p.runeStart
}

// term terminates the current token, setting its end position and text.
func (p *Tokenizer) term() {
	p.tokenEnd = p.runeStart
	p.text = string(p.scratch)
}

// isEOF returns true if the tokenizer has reached end of input.
func (p *Tokenizer) isEOF() bool {
	return p.err == io.EOF
}

// span returns the accumulated source text for the current token.
func (p *Tokenizer) span() string {
	return string(p.scratch)
}

// this returns the current rune and any error.
func (p *Tokenizer) this() (rune, error) {
	return p.cur, nil
}

// next advances to the next rune, appending the current rune to scratch.
// Updates position tracking and handles line endings and EOF.
func (p *Tokenizer) next() {
	n := 0
	p.scratch = append(p.scratch, p.cur)
	p.cur, n, p.err = p.rdr.ReadRune()
	p.runeStart = p.runeEnd
	p.runeEnd.Inc(n)
	if isNewLine(p.cur) {
		p.runeEnd.NewLine()
	}
	if n == 0 {
		p.cur = utf8.RuneError
		p.err = io.EOF
	} else if p.cur == utf8.RuneError {
		p.err = NewTokenizerError(MessageRuneError, p.tokenStart, p.tokenEnd)
	}
}

// isLetter returns true if c is an ASCII letter (A-Z or a-z).
func isLetter(c rune) bool {
	return unicode.IsLetter(c)
}

// isSymbolInitial returns true if c can start a symbol (letter or special initial).
func isSymbolInitial(c rune) bool {
	if isLetter(c) {
		return true
	}
	if isSpecialInitial(c) {
		return true
	}
	return false
}

// isSymbolSubsequent returns true if c can appear after the first character of a symbol.
func isSymbolSubsequent(r int, c rune) bool {
	for isIdentifierInitial(c) || isDigit(r, c) || isSpecialSubsequent(c) {
		return true
	}
	return false
}

// isIdentifierInitial returns true if c can start an identifier.
func isIdentifierInitial(c rune) bool {
	return isSpecialInitial(c) || isLetter(c)
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
	if r == 10 && c <= '9' {
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

// isImaginary returns true if c is 'i'.
func isImaginary(c rune) bool {
	return c == 'i'
}

// isExponent returns true if c is an 'E' for exponent.
func isExponent(c rune) bool {
	return c == 'e' || c == 'E'
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
	return isLetter(c) || isSpecialInitial(c)
}

// isSubsequent returns true if c can appear after the first character of an identifier.
func isSubsequent(c rune) bool {
	return isInitial(c) || isDigit(10, c) || isSpecialSubsequent(c)
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
