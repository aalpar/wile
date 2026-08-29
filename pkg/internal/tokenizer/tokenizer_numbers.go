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

// readSpecialNumber reads an <infnan> keyword (inf.0, nan.0) with its mandatory
// '.' and at least one digit in radix r. It returns "" when the literal was
// read, and otherwise the message describing why the run is not one — without
// raising it, and without touching p.err.
//
// Reporting rather than raising is what makes the scanner speculative. The
// sign-initial arms discard the message and hand the run to the symbol scanner
// (a peculiar identifier is still possible there); the arms inside a complex
// literal, where no identifier can follow, raise it through
// requireSpecialNumber. Setting p.err on a path that then reports "not a
// number" is what turned `+nan`, `+nane` and `+nan_x` into hard read errors.
func (p *Tokenizer) readSpecialNumber(s string, r int, mismatchMsg string) string {
	// The keyword check precedes any error check. At end of input scanWith
	// reports a short match AND io.EOF, so returning on the error first accepted
	// a truncated keyword: `+in` at end of input read as +inf.0, `+n` as +nan.0.
	n := p.scanCaseInsensitive([]byte(s))
	if n != 0 {
		return mismatchMsg
	}
	// A complete keyword still needs its ".0". At end of input curr() is the
	// RuneError sentinel, which fails this test the same way ')' does — that is
	// the second early-out, and it is why `+inf` at end of input read as +inf.0
	// while `(list '+inf)` errored.
	if !isDot(p.curr()) {
		return MessageExpectingDecimalFraction
	}
	p.next()
	if !isDigit(r, p.curr()) {
		return MessageExpectingDecimalFraction
	}
	p.readUnsignedBaseNNumber(r) //nolint:errcheck
	return ""
}

// requireSpecialNumber reads an <infnan> in a position that admits no identifier
// fallback — the imaginary part of a complex literal — and raises the mismatch
// as a lexical fault.
//
// A pending fault is left alone: it is the more specific one and already carries
// its stamp, and suppressing the mismatch diagnostic under one is what the
// pre-fix reader did by returning before the keyword was checked.
func (p *Tokenizer) requireSpecialNumber(s string, r int, mismatchMsg string) {
	msg := p.readSpecialNumber(s, r, mismatchMsg)
	if msg == "" || p.err != nil {
		return
	}
	p.fail(msg)
}

// atNumberEnd reports whether the scanner sits at a legal end for a numeral that
// terminates implicitly: a delimiter, a '#' prefix, or end of input.
//
// R7RS §7.1.1 exempts only +i, -i and <infnan> from <peculiar identifier>, so a
// complete one of those followed by anything else is an identifier, not a number
// and a second datum — the rule CLAUDE.local.md already records for
// radix-prefixed numerals. A pending diagnostic is not an end: the run goes back
// to the symbol scanner with the fault still on p.err.
func (p *Tokenizer) atNumberEnd() bool {
	if p.err != nil {
		return errors.Is(p.err, io.EOF)
	}
	return isDelimiterOrMarker(p.curr())
}

// fallBackToSymbol abandons a speculative numeric scan: the run is re-typed as a
// peculiar identifier, its remaining subsequents are consumed, and the token
// value is restored from the raw text.
//
// The restore is the point. Each arm used to mint its own symbol from wherever
// the scanner had reached, dropping everything before it — `+.abc` became the
// symbol `bc`, `+nabc` and `+node` became the empty symbol, and distinct
// identifiers turned eq?.
//
// Nothing is un-read, so the package's no-backtracking rule holds, and p.err is
// never cleared: the arms report "not a number" by returning false, so any error
// still on the field is a real lexical fault and stays.
func (p *Tokenizer) fallBackToSymbol() {
	p.state = TokenizerStateSymbol
	p.readSymbol()
	p.value = append(p.value[:0], p.text...)
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

// requireDelimiterAfterRadixNumeral enforces R7RS §7.1.1 implicit termination
// for a numeral carrying an explicit radix prefix: once #b/#o/#d/#x has fixed
// the digit set, the numeral has to end at a delimiter.
//
// Without it the scanner treats an out-of-radix digit as a token *boundary*
// rather than a fault, so `#b19` scans as 1 followed by 9 and `(#b19)` is the
// two-element list (1 9). That also makes the digit set unenforceable from the
// parser, which never sees the two halves as one numeral.
//
// r == 0 (no prefix) is deliberately exempt: `1abc` still scans as 1 then abc.
// Extending implicit termination to every numeral is a separate change with an
// unmeasured blast radius — TODO.md, "Delimiter termination for decimal
// numerals".
func (p *Tokenizer) requireDelimiterAfterRadixNumeral(r int) {
	if r == 0 || p.err != nil || isDelimiterOrMarker(p.curr()) {
		return
	}
	p.fail(MessageExpectingDelimiterAfterNumber)
}

func (p *Tokenizer) readDiv(r int) {
	p.next()
	if p.err != nil {
		return
	}
	if !isDigit(r, p.curr()) { // +10/10
		p.fail(MessageExpectingNumber)
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

// readImaginaryOrSignedInfinity scans a sign followed by 'i': the unit imaginary
// +i/-i, the infinities +inf.0/-inf.0, and the complex literals built on them.
// It reports whether the run is a number; false leaves it to the caller to
// rescan as a peculiar identifier.
func (p *Tokenizer) readImaginaryOrSignedInfinity(r int) bool {
	p.next() // consume 'i'
	if p.err == nil && (p.curr() == 'n' || p.curr() == 'N') {
		return p.readSignedInfinity(r)
	}
	// +i and -i are the unit imaginary only when the token ends here; `+i2`,
	// `+ifoo` and `-ibar` are single identifiers, not a number and a second
	// datum. Extending requireDelimiterAfterRadixNumeral to r == 0 would make
	// them read errors instead, which is equally nonconformant.
	if !p.atNumberEnd() {
		return false
	}
	p.state = TokenizerStateSignedImaginary
	return true
}

// readSignedInfinity scans +inf.0/-inf.0 and the complex literals extending
// them (+inf.0i, +inf.0+2i). Reports whether the run is a number.
func (p *Tokenizer) readSignedInfinity(r int) bool {
	if p.readSpecialNumber("nf", r, MessageExpectingInf) != "" {
		return false
	}
	p.state = TokenizerStateSignedInf
	if isExplicitSign(p.curr()) {
		// +inf.0+2i — rectangular complex with an <infnan> real part.
		p.state = TokenizerStateSignedComplex
		p.next()
		if p.err != nil {
			return false
		}
		p.mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r) //nolint:errcheck
		p.mayConsumeImaginarySuffix()
		return p.atNumberEnd()
	}
	if isComplexPolar(p.curr()) {
		// +inf.0@1 — <infnan> is a <real R>, so it is a legal polar magnitude.
		p.state = TokenizerStateSignedComplexPolar
		p.mayReadPolarPart(r)
		return p.atNumberEnd()
	}
	if p.mayConsumeImaginarySuffix() {
		p.state = TokenizerStateSignedImaginaryInf
	}
	return p.atNumberEnd()
}

// readSignedNan scans +nan.0/-nan.0 and the imaginary form +nan.0i. Reports
// whether the run is a number.
func (p *Tokenizer) readSignedNan(r int) bool {
	if p.readSpecialNumber("nan", r, MessageExpectingNan) != "" {
		return false
	}
	p.state = TokenizerStateSignedNan
	if isComplexPolar(p.curr()) {
		// +nan.0@1 — see readSignedInfinity.
		p.state = TokenizerStateSignedComplexPolar
		p.mayReadPolarPart(r)
		return p.atNumberEnd()
	}
	if p.mayConsumeImaginarySuffix() {
		p.state = TokenizerStateSignedImaginaryNan
	}
	return p.atNumberEnd()
}

// readSignedDecimalFractionOrExponentWithImaginary scans a sign followed by '.':
// the decimal fraction +.5, and the complex literals extending it. Reports
// whether the run is a number.
func (p *Tokenizer) readSignedDecimalFractionOrExponentWithImaginary(r int) bool {
	p.next() // consume '.'
	// A digit in the current radix is a digit, not a symbol character. The two
	// sets overlap only in hex, where a-f are dot-subsequents as well: under #x,
	// `-.f` is the fraction -0.9375 rather than the peculiar identifier -.f.
	//
	// Anything else — a dot-subsequent, a delimiter, end of input — is not a
	// fraction, and the caller rescans the whole run. Consuming the identifier
	// here instead is what made `'+.abc` the symbol `bc` and `'-.f` the empty
	// symbol.
	if p.err != nil || !isDigit(r, p.curr()) {
		return false
	}
	p.state = TokenizerStateSignedDecimalFraction
	p.tokRadix = effectiveRadix(r)
	// Past the point of no return: a fault from here on is a fault in a number,
	// not a reason to reinterpret the run.
	p.readDigitsAndHash(r)
	if p.err != nil {
		return true
	}
	p.mayReadExponent(r) //nolint:errcheck
	if p.err != nil {
		return true
	}
	p.readSignedComplexSuffix(r)
	return true
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
				p.requireSpecialNumber("nf", r, MessageExpectingInf)
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
			p.fail(MessageExpectingImaginary)
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
	// Digit before symbol, for the reason given in
	// readSignedDecimalFractionOrExponentWithImaginary: under #x, `.f` is a
	// fraction and not the peculiar identifier .f.
	if !isDigit(r, p.curr()) {
		if p.readDotSubsequentSymbol() {
			p.value = append(p.value[:0], p.text...)
		}
		return
	}
	p.state = TokenizerStateUnsignedDecimalFraction
	p.tokRadix = effectiveRadix(r)
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
//	  ├─ 'e'/'E' after digits → scientific notation (1e10), in radix 0/10 only
//	  └─ otherwise → unsigned integer
func (p *Tokenizer) readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(r int) {
	// Branch 1: Starts with explicit sign (+/-)
	switch {
	case isExplicitSign(p.curr()):
		p.signed = true
		// The token type is assigned by whichever arm reaches its point of no
		// return, never before: this used to say TokenizerStateSymbol here, so a
		// rune error one character into a numeral was stamped `symbol` in the
		// diagnostic. mark() leaves the state at TokenizerStateFailed, which is
		// the honest answer while the run is still undecided.
		p.next()
		if p.err != nil {
			// Bare sign at end of input, or a fault on the next rune. Either
			// way the run is the symbol + or -.
			p.state = TokenizerStateSymbol
			p.value = append(p.value[:0], p.text...)
			return
		}
		switch {
		case isImaginary(p.curr()):
			if !p.readImaginaryOrSignedInfinity(r) {
				p.fallBackToSymbol()
			}
			return
		case p.curr() == 'n' || p.curr() == 'N':
			if !p.readSignedNan(r) {
				p.fallBackToSymbol()
			}
			return
		case isDigit(r, p.curr()):
			p.readIntegerAndFraction(true, r)
			return
		case isDot(p.curr()):
			if !p.readSignedDecimalFractionOrExponentWithImaginary(r) {
				p.fallBackToSymbol()
			}
			return
		case isSignSubsequent(p.curr()):
			p.fallBackToSymbol()
			return
		}
		// Bare sign (+/-) as symbol
		p.state = TokenizerStateSymbol
		p.value = append(p.value[:0], p.text...)
		return
	case isDot(p.curr()):
		p.readConsOrDecimalFractionWithExponent(r)
		return
	case isDigit(r, p.curr()):
		p.readIntegerAndFraction(false, r)
		return
	default:
		p.fail(MessageExpectingNumber)
		return
	}
}

func (p *Tokenizer) mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r int) {
	// Branch 1: Starts with explicit sign (+/-)
	switch {
	case p.curr() == 'i':
		// The imaginary part is either the unit imaginary (+inf.0+i) or another
		// infinity (+inf.0+inf.0i); only the second continues into the keyword.
		// The sole caller that reaches this arm is readSignedInfinity, which
		// does NOT pre-handle the unit 'i' — scanning "inf" unconditionally
		// consumed it and then mismatched, and `+inf.0+i` read as a number only
		// because end of input suppressed the fault. `+inf.0+i)` was a read
		// error. Same shape as readIntegerAndFraction and
		// mayReadSignedImaginaryPart, which already peek this way.
		p.next() // consume 'i'
		if p.err != nil || p.curr() != 'n' {
			return
		}
		p.requireSpecialNumber("nf", r, MessageExpectingInf)
		return
	case p.curr() == 'n':
		p.requireSpecialNumber("nan", r, MessageExpectingNan)
		return
	case isDot(p.curr()):
		// consume dot
		// p.readDecimalFractionWithExponent(r) is very similar
		// decimal fraction - use p.signed to determine correct state
		p.next()
		if p.err != nil {
			return
		}
		// Digit before symbol, as in the two readers above.
		if !isDigit(r, p.curr()) && !p.readDotSubsequentSymbol() {
			p.fail(MessageExpectingDecimalFraction)
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
		p.failWrap(p.err, MessageExpectingExponentDigits)
		return
	}
	// Don't return early on EOF - we need to check for required digits below

	// Optional sign
	p.mayConsumeSign()
	if p.err != nil {
		p.failWrap(p.err, MessageExpectingExponentDigits)
		return
	}

	// R7RS requires at least one digit after exponent marker (and optional sign)
	// When p.err is set (including io.EOF), p.curr() returns RuneError which isDigit rejects
	if !isDigit(r, p.curr()) {
		p.fail(MessageExpectingExponentDigits)
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
		p.requireSpecialNumber("nf", r, MessageExpectingInf)
		if p.err != nil {
			return
		}
		p.mayConsumeImaginarySuffix()
		return
	} else if p.curr() == 'n' {
		// Check for +nan.0i or -nan.0i
		p.requireSpecialNumber("nan", r, MessageExpectingNan)
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

// mayReadPolarPart reads the angle of a polar complex literal, entered with the
// scanner on the '@' that follows the magnitude. Handles @1.5708, @-1.5708,
// @1/2, @+inf.0.
//
// R7RS §7.1.1: <polar R> -> <real R> @ <real R>, and <real R> is
// <sign> <ureal R> | <infnan>. Both alternatives are exactly what
// mayReadUnsignedFractionalRealNumberOrRationalRealNumber scans, so the angle
// delegates rather than re-deriving it. Open-coding three of that scanner's five
// arms is what made `1@+inf.0` a read error and silently truncated `1@1/2` to
// `1@1` plus a stray `/2` symbol — a wrong answer, not a diagnostic.
//
// The i/n arms are reachable only behind a consumed sign, because <infnan>
// carries its own: `1@inf.0` and `1@i` are identifiers in Chez and Racket alike.
//
// The state is the caller's: both callers set it before calling (polar magnitudes
// are signed or unsigned and only they know which), and overwriting it here made
// the same literal tokenize two ways depending on whether it ended at EOF.
func (p *Tokenizer) mayReadPolarPart(r int) {
	if p.curr() != '@' {
		return
	}
	p.next() // consume '@'
	if p.err != nil {
		return
	}

	signed := isExplicitSign(p.curr())
	p.mayConsumeSign()
	if p.err != nil {
		return
	}

	atInfnan := signed && (p.curr() == 'i' || p.curr() == 'n')
	if !atInfnan && !isDot(p.curr()) && !isDigit(r, p.curr()) {
		p.fail(MessageExpectingNumber)
		return
	}
	p.mayReadUnsignedFractionalRealNumberOrRationalRealNumber(r)
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
	// An earlier fault is the more specific one and already carries its stamp;
	// do not overwrite it with this one.
	if err != nil && p.err == nil {
		p.failWrap(err, MessageCannotParseNumber)
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
