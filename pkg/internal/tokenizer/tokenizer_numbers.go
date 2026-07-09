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
)

// effectiveRadix maps the scanning radix to the base the parser will use.
// The scanning radix uses 0 for "default decimal" (no prefix); both 0 and an
// explicit #d prefix (10) parse in base 10.
func effectiveRadix(r int) int {
	if r == 0 {
		return 10
	}
	return r
}

// integerState records the effective parse base on the current token (read back
// via Token.Radix()) and returns the signed or unsigned integer state. Base no
// longer lives in the state name: a single Signed/UnsignedInteger pair covers
// every radix, and the parser reads Radix() instead of re-deriving a base.
func (p *Tokenizer) integerState(signed bool) TokenizerState {
	p.tokRadix = effectiveRadix(p.radix)
	return signedState(signed, TokenizerStateSignedInteger, TokenizerStateUnsignedInteger)
}

// signedState returns the signed or unsigned variant of a token state.
func signedState(signed bool, s, u TokenizerState) TokenizerState {
	if signed {
		return s
	}
	return u
}

func (p *Tokenizer) readSpecialNumber(s string, r int, mismatchErr string, onMismatch func()) {
	n := p.scanCaseInsensitive([]byte(s))
	if p.err != nil {
		return
	}
	if n != 0 {
		if onMismatch != nil {
			onMismatch()
			return
		}
		p.err = NewTokenizerError(mismatchErr)
		return
	}
	if !isDot(p.curr()) {
		p.err = NewTokenizerError(MessageExpectingDecimalFraction)
		return
	}
	p.next()
	if !isDigit(r, p.curr()) {
		p.err = NewTokenizerError(MessageExpectingDecimalFraction)
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
}

// readNan attempts to parse a NaN literal (e.g. "nan.0"). Returns true if
// the full literal was parsed successfully. On text mismatch, consumes
// remaining sign-subsequent characters (symbol fallback) and returns false
// without setting an error. On text match but malformed suffix (e.g. "nan,0"),
// returns false with p.err set.
func (p *Tokenizer) readNan(s string, r int) bool {
	matched := true
	p.readSpecialNumber(s, r, "", func() {
		matched = false
		for p.err == nil && isSignSubsequent(p.curr()) {
			p.next()
		}
	})
	// Text matched but suffix was malformed (not ".0") — not a valid NaN.
	// EOF is excluded: it means we hit end-of-input after a valid "nan.0".
	if matched && p.err != nil && !errors.Is(p.err, io.EOF) {
		matched = false
	}
	return matched
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
	p.mayConsumeSign()
	if p.err != nil {
		return
	}
	// Integer part
	p.readUnsignedBaseNNumber(10)
	// Optional decimal point
	if p.err == nil && p.curr() == '.' {
		p.next()
	}
	if p.err != nil {
		return
	}
	// Fractional part
	p.readUnsignedBaseNNumber(10)
	if p.err != nil {
		return
	}
	// Optional exponent
	if isExpMarker(p.curr()) {
		p.next()
		if p.err != nil {
			return
		}
		p.mayConsumeSign()
		if p.err != nil {
			return
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
		p.err = NewTokenizerError(MessageExpectingNumber)
		return
	}
	p.readDigitsAndHash(r)
}

func (p *Tokenizer) readDecimalFractionWithExponent(r int) {
	hadHash := p.hashDigit
	// consume '.'
	p.next()
	if p.err != nil {
		return
	}
	p.readOptionalDecimalPart(r, hadHash)
}

// readOptionalDecimalPart reads fractional digits after a decimal point.
// Caller must have already consumed the dot. If hadHash is true (hash digits
// preceded the dot per R7RS §7.1.1 production 4), only hash digits are allowed.
func (p *Tokenizer) readOptionalDecimalPart(r int, hadHash bool) {
	if hadHash {
		p.readHashDigits()
	} else if isDigit(r, p.curr()) {
		p.readDigitsAndHash(r)
	}
	if p.err != nil {
		return
	}
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
	p.readInf("nf", r)
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
	if !p.readNan("nan", r) {
		return
	}
	p.state = TokenizerStateSignedNan
	if p.mayConsumeImaginarySuffix() {
		p.state = TokenizerStateSignedImaginaryNan
	}
}

func (p *Tokenizer) readSignedDecimalFractionOrExponentWithImaginary(r int) {
	p.next()
	if p.err != nil {
		return
	}
	switch {
	case p.readDotSubsequentSymbol():
		// Symbol consumed; falls through to no-op digit/exponent reads below
	case !isDigit(r, p.curr()):
		p.err = NewTokenizerError(MessageExpectingDecimalFraction)
		return
	default:
		p.state = TokenizerStateSignedDecimalFraction
	}
	// read decimal fractional part
	p.readDigitsAndHash(r)
	if p.err != nil {
		return
	}
	// read optional exponent
	p.mayReadExponent(r) //nolint:errcheck
	if p.err != nil {
		return
	}
	p.readSignedComplexSuffix(r)
}

func (p *Tokenizer) readIntegerAndFraction(signed bool, r int) {
	p.state = p.integerState(signed)
	p.readDigitsAndHash(r)
	if p.err != nil {
		return
	}
	switch {
	case isDot(p.curr()):
		p.state = signedState(signed, TokenizerStateSignedDecimalFraction, TokenizerStateUnsignedDecimalFraction)
		p.readDecimalFractionWithExponent(r)
	case p.curr() == '/':
		p.state = signedState(signed, TokenizerStateSignedRationalFraction, TokenizerStateUnsignedRationalFraction)
		p.readDiv(r) //nolint:errcheck
	case isExponentMarkerForRadix(p.curr(), r):
		p.state = signedState(signed, TokenizerStateSignedScientificNotation, TokenizerStateUnsignedScientificNotation)
		p.mayReadExponent(r) //nolint:errcheck
	}
	if p.err != nil {
		return
	}
	if signed {
		p.readSignedComplexSuffix(r)
		return
	}
	switch {
	case isImaginary(p.curr()):
		p.state = TokenizerStateUnsignedImaginary
		p.next()
	case isExplicitSign(p.curr()):
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
				p.readSpecialNumber("nf", r, MessageExpectingInf, nil)
				if p.err != nil {
					return
				}
				p.mayConsumeImaginarySuffix()
			}
			// Otherwise just unit imaginary, already consumed 'i'
			return
		}
		p.mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
		if p.err != nil {
			return
		}
		if !isImaginary(p.curr()) {
			p.err = NewTokenizerError(MessageExpectingImaginary)
			return
		}
		p.next()
	case isComplexPolar(p.curr()):
		p.state = TokenizerStateUnsignedComplexPolar
		p.mayReadPolarPart(r) //nolint:errcheck
	}
}

func (p *Tokenizer) readConsOrDecimalFractionWithExponent(r int) {
	p.state = TokenizerStateCons
	p.next()
	if p.err != nil {
		return
	}
	if p.readDotSubsequentSymbol() {
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
	p.readDigitsAndHash(r)
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
		p.err = NewTokenizerError(MessageExpectingNumber)
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
		p.readSpecialNumber("nan", r, MessageExpectingNan, nil)
		return
	case isDot(p.curr()):
		// consume dot
		// p.readDecimalFractionWithExponent(r) is very similar
		// decimal fraction - use p.signed to determine correct state
		p.next()
		if p.err != nil {
			return
		}
		if !p.readDotSubsequentSymbol() && !isDigit(r, p.curr()) {
			p.err = NewTokenizerError(MessageExpectingDecimalFraction)
			return
		}
		p.readDigitsAndHash(r)
		if p.err != nil {
			return
		}
		p.mayReadExponent(r) //nolint:errcheck
		return
	case isDigit(r, p.curr()):
		p.readDigitsAndHash(r)
		if p.err != nil {
			return
		}
		switch {
		case isDot(p.curr()):
			p.readDecimalFractionWithExponent(r)
		case isExponentMarkerForRadix(p.curr(), r):
			p.mayReadExponent(r) // nolint:errcheck
		case p.curr() == '/':
			p.readDiv(r) // nolint:errcheck
		}
		return
	}
}

func (p *Tokenizer) mayReadExponent(r int) {
	if !isExponentMarkerForRadix(p.curr(), r) {
		return
	}
	p.next() // consume exponent marker
	if p.err != nil {
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingExponentDigits)
		return
	}
	// Don't return early on EOF - we need to check for required digits below

	// Optional sign
	p.mayConsumeSign()
	if p.err != nil {
		p.err = NewTokenizerErrorWithWrap(p.err, MessageExpectingExponentDigits)
		return
	}

	// R7RS requires at least one digit after exponent marker (and optional sign)
	// When p.err is set (including io.EOF), p.curr() returns RuneError which isDigit rejects
	if !isDigit(r, p.curr()) {
		p.err = NewTokenizerError(MessageExpectingExponentDigits)
		return
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
}

// mayReadSignedImaginaryPart reads an optional imaginary part for complex numbers.
// Called when current character is '+' or '-' after reading a real number.
// Handles patterns like: +3i, +3.5i, +i, -2i, -inf.0i, -nan.0i
func (p *Tokenizer) mayReadSignedImaginaryPart(r int) {
	p.mayConsumeSign()
	if p.err != nil {
		return
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
		p.readSpecialNumber("nf", r, MessageExpectingInf, nil)
		if p.err != nil {
			return
		}
		p.mayConsumeImaginarySuffix()
		return
	} else if p.curr() == 'n' {
		// Check for +nan.0i or -nan.0i
		p.readSpecialNumber("nan", r, MessageExpectingNan, nil)
		if p.err != nil {
			return
		}
		p.mayConsumeImaginarySuffix()
		return
	}

	// Check for numeric coefficient: +3i, +3.5i, +3/4i, etc.
	if !isDigit(r, p.curr()) {
		return
	}
	p.readDigitsAndHash(r)
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
		p.readOptionalDecimalPart(r, hadHash)
		if p.err != nil {
			return
		}
	case p.curr() == '/':
		// Rational: +3/4i
		p.readDiv(r)
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
	p.mayConsumeImaginarySuffix()
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
	p.mayConsumeSign()
	if p.err != nil {
		return
	}

	// Must have digits or a dot followed by digits
	switch {
	case isDot(p.curr()):
		p.readDiv(r) //nolint:errcheck
		if p.err != nil {
			return
		}
	case isDigit(r, p.curr()):
		p.readDigitsAndHash(r)
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
		p.readOptionalDecimalPart(r, hadHash)
		if p.err != nil {
			return
		}
	default:
		p.err = NewTokenizerError(MessageExpectingNumber)
		return
	}

	// Check for exponent (no-op if readOptionalDecimalPart already consumed it)
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
	var s strings.Builder
	for p.err == nil && (isDigit(r, p.curr()) && (maxn <= 0 || n < maxn)) {
		s.WriteString(string(p.curr()))
		p.next()
		n++
	}
	if n == 0 {
		return 0, n
	}
	var q int64
	var err error
	// always attempt to parse s, even on error
	q, err = strconv.ParseInt(s.String(), r, 64)
	if err != nil {
		err = NewTokenizerErrorWithWrap(err, MessageCannotParseNumber)
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

// readDigitsAndHash reads a run of digits in the given radix followed by
// optional '#' placeholders. This is the R7RS §7.1.1 production for digit
// sequences where hash digit placeholders always follow real digits.
func (p *Tokenizer) readDigitsAndHash(r int) {
	p.readUnsignedBaseNNumber(r)
	p.readHashDigits()
}

// readDotSubsequentSymbol checks if the current character is a dot-subsequent
// (making the preceding dot part of a symbol like `...` or `.foo`). If so, it
// sets the state to Symbol, consumes the rest of the symbol, and returns true.
// Returns false without side effects if the current character is not a dot-subsequent.
func (p *Tokenizer) readDotSubsequentSymbol() bool {
	if !isDotSubsequent(p.curr()) {
		return false
	}
	p.state = TokenizerStateSymbol
	p.next()
	if p.err != nil {
		return true
	}
	p.readSymbol()
	return true
}

// mayConsumeSign checks if the current character is an explicit sign (+/-)
// and, if so, consumes it. Callers must check p.err afterward.
func (p *Tokenizer) mayConsumeSign() {
	if isExplicitSign(p.curr()) {
		p.next()
	}
}

// readSignedComplexSuffix dispatches on the complex suffix after a signed real
// number has been read. Handles trailing 'i' (imaginary), explicit sign
// (rectangular complex), and '@' (polar complex).
func (p *Tokenizer) readSignedComplexSuffix(r int) {
	switch {
	case p.mayConsumeImaginarySuffix():
		p.state = TokenizerStateSignedImaginary
	case isExplicitSign(p.curr()):
		p.state = TokenizerStateSignedComplex
		p.mayReadSignedImaginaryPart(r)
	case isComplexPolar(p.curr()):
		p.state = TokenizerStateSignedComplexPolar
		p.mayReadPolarPart(r)
	}
}

// mayConsumeImaginarySuffix checks if the current character is the imaginary
// marker 'i' and, if so, consumes it. Returns true if 'i' was consumed.
func (p *Tokenizer) mayConsumeImaginarySuffix() bool {
	if !isImaginary(p.curr()) {
		return false
	}
	p.next()
	return true
}
