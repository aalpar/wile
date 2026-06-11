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
	"strings"
	"unicode"
	"unicode/utf8"
)

var stringEscapes = map[rune]string{
	'a':  "\a",
	'b':  "\b",
	't':  "\t",
	'n':  "\n",
	'r':  "\r",
	'\\': "\\",
	'"':  "\"",
	'|':  "|",
}

// CharMnemonics maps R7RS §6.6 character mnemonic names to their rune values.
// Keys must be lowercase — lookup normalizes via strings.ToLower.
var CharMnemonics = map[string]rune{
	"alarm":     '\a',
	"backspace": '\b',
	"delete":    '\x7F',
	"escape":    '\x1B',
	"newline":   '\n',
	"null":      '\x00',
	"return":    '\r',
	"space":     ' ',
	"tab":       '\t',
}

func validateCodePoint(x int64) error {
	if x > 0x10FFFF {
		return NewTokenizerError(MessageCodePointExceedsUnicodeMaximum)
	}
	if x >= 0xD800 && x <= 0xDFFF {
		return NewTokenizerError(MessageCodePointIsSurrogate)
	}
	return nil
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
		p.err = NewTokenizerError(MessageExpectingHexDigit)
		return
	}
	if p.cur != ';' {
		p.err = NewTokenizerError(MessageExpectingHexSequenceTerminator)
		return
	}
	err := validateCodePoint(x)
	if err != nil {
		p.err = err
		return
	}
	p.next()
	if p.err != nil {
		return
	}
	// code point validated - x will always be a valid Unicode code point that can be converted to a rune without error
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
	if isIntralineWhitespace(p.curr()) || isLineEnding(p.curr()) {
		p.skipLineContinuation()
		return
	}
	replacement, ok := stringEscapes[p.curr()]
	if ok {
		p.value += replacement
		p.next()
		return
	}
	p.err = NewTokenizerError(MessageExpectingEscape)
}

// readDelimited reads content enclosed by a terminator rune (e.g. '"' or '|'),
// processing backslash escape sequences along the way. On EOF before the closing
// terminator, it converts the error to a TokenizerError with unterminatedMsg.
// Called after the opening delimiter has already been consumed.
// Returns true if the terminator was found and consumed.
func (p *Tokenizer) readDelimited(terminator rune, unterminatedMsg string) bool {
	p.value = ""
	for p.err == nil && p.curr() != terminator {
		if isBackSlash(p.curr()) {
			p.next()
			if p.err != nil {
				if errors.Is(p.err, io.EOF) {
					p.err = NewTokenizerError(unterminatedMsg)
				}
				return false
			}
			p.readEscapeSequence()
			if p.err != nil {
				return false
			}
			continue
		}
		p.value += string(p.curr())
		p.next()
		if p.err != nil {
			if errors.Is(p.err, io.EOF) {
				p.err = NewTokenizerError(unterminatedMsg)
			}
			return false
		}
	}
	p.next() // consume terminator — may set p.err to io.EOF, which is fine
	return true
}

// readString reads a string literal until the closing double-quote.
// Builds the processed string content in p.value (without quotes, with escapes converted).
func (p *Tokenizer) readString() {
	ok := p.readDelimited('"', MessageUnterminatedString)
	if ok {
		p.state = TokenizerStateString
	}
}

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
		p.err = NewTokenizerError(MessageExpectingLineEnding)
		return
	}
	// scanLineEnding() already advanced past the line ending
	// skip intra-line whitespace after line ending
	for p.err == nil && isIntralineWhitespace(p.curr()) {
		p.next()
	}
}

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
		x, n := p.readUnsignedBaseNInteger(16, 32) //nolint:errcheck
		if n == 0 {
			p.err = NewTokenizerErrorWithWrap(p.err, MessageInvalidCharacterHexEscape)
			return utf8.RuneError
		}
		err := validateCodePoint(x)
		if err != nil {
			p.err = err
			return utf8.RuneError
		}
		// code point validated - x will always be a valid Unicode code point that can be converted to a rune without error
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
		r, ok := CharMnemonics[strings.ToLower(mnemonic)]
		if ok {
			return r
		}
		p.err = NewTokenizerError(MessageInvalidCharacterMnemonic)
		return utf8.RuneError

	case unicode.IsGraphic(p.curr()):
		p.state = TokenizerStateCharGraphic
		qrune = p.curr()
		p.next()
		return qrune
	}
	p.err = NewTokenizerError(MessageExpectingCharacterMnemonicOrHexEscape)
	return utf8.RuneError
}

func (p *Tokenizer) readSymbol() {
	for p.err == nil && isSymbolSubsequent(p.curr()) {
		p.value += string(p.curr())
		p.next()
	}
}

// readExtendedSymbol reads an extended symbol enclosed in vertical bars (|symbol|).
// Called after the opening '|' has been consumed.
func (p *Tokenizer) readExtendedSymbol() {
	if !p.readDelimited('|', MessageUnterminatedExtendedSymbol) {
		return
	}
}
