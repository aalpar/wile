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
	"unicode/utf8"
)

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
		p.value = utf8.AppendRune(p.value[:0], p.readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape())
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
		p.err = NewTokenizerError("invalid character after #")
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
			// R7RS §7.1.1: #u introduces nothing but a #u8( byte vector. Falling
			// through with neither a state nor an error made `#u9` read as the
			// PREVIOUS token's type and the input parse as if the #u were absent.
			p.err = NewTokenizerError(MessageExpectingByteVectorPrefix)
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
		p.readBigNum(isExtendedExponentMarker)
		return
	case p.curr() == 'z' || p.curr() == 'Z': // big int #z (R7RS-adjacent; always decimal)
		p.state = TokenizerStateBigInteger
		p.tokRadix = 10
		p.readBigNum(isExtendedExponentMarker)
		return
	default:
		p.state = TokenizerStateMarker
		for p.err == nil && isSubsequent(p.curr()) {
			p.next()
		}
		return
	}
}

func (p *Tokenizer) readDirective() {
	// #!backspace, #!b, ...
	// drop the '!'
	p.next()
	if p.err != nil {
		p.err = NewTokenizerError(MessageExpectingDirective)
		return
	}
	// traditional identifier - letter followed by letter or number or dash
	if isUnicodeLetter(p.curr()) {
		p.next()
	} else {
		p.err = NewTokenizerError(MessageExpectingDirective)
		return
	}
	// read letters, digits, dashes. this applies to directives like "fold-case"
	for p.err == nil && (isUnicodeLetter(p.curr()) || isDigit(10, p.curr()) || isNegativeSign(p.curr())) {
		p.next()
	}
}

// readSpecialNumber reads inf.0 or nan.0 special number literals.
// If onMismatch is provided, it's called when the keyword doesn't match;
