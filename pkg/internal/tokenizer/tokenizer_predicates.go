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

import "unicode"

func isUnicodeLetter(c rune) bool {
	return unicode.IsLetter(c) || unicode.Is(unicode.Nl, c)
}

func isUnicodeDigit(c rune) bool {
	return unicode.IsDigit(c) || unicode.IsOneOf([]*unicode.RangeTable{unicode.Nd}, c)
}

// isSymbolSubsequent returns true if c can appear after the first character of a symbol.
func isSymbolSubsequent(c rune) bool {
	return isInitial(c) || isSpecialSubsequent(c) || isExtendedSubsequent(c) || isUnicodeDigit(c)
}

// isVerticalLine returns true if c is the vertical line character (|).
func isVerticalLine(c rune) bool {
	return c == '|'
}

// isDelimiter returns true if c is a token delimiter per R7RS 7.1.1.
// Delimiters are: whitespace, |, (, ), ", and ;
//
// R7RS §2.1 makes [ and ] equivalent to ( and ), so they delimit too. Omitting
// them broke the #-boolean forms, whose scanners are the ones that consult this
// predicate: `#t]` scanned as a bare marker, so `(let ([x #t]) ...)` would not
// parse. Self-terminating tokens (numbers, characters, vectors) were unaffected.
func isDelimiter(c rune) bool {
	return isIntralineWhitespace(c) || isVerticalLine(c) || isLineEnding(c) ||
		c == '(' || c == ')' || c == '[' || c == ']' || c == '"' || c == ';'
}

// isDelimiterOrMarker returns true if c is a token delimiter per R7RS 7.1.1.
// Delimiters are: whitespace, |, (, ), [, ], ", and ;
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

// isMarker returns true if c is a marker character (#).
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

// isQuote returns true if c is a single quote (').
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

// isExponentMarkerForRadix reports whether c begins an exponent in a literal of
// the given radix. Exponent markers are a decimal-only grammar (R7RS §7.1.1:
// <suffix> appears only in <decimal 10>); in radixes 2/8/16 the same letters
// are either ordinary digits (e/d/f in hex, consumed by the digit reader before
// any marker check) or simply invalid. Gating on decimal keeps a binary/octal/
// hex integer from mis-reading a trailing e/s/f/d/l as scientific notation.
//
// Decimal is r == 10 (explicit #d) or r == 0 (the unset default radix, used for
// a bare number with no #-prefix); see effectiveRadix.
func isExponentMarkerForRadix(c rune, r int) bool {
	return (r == 0 || r == 10) && isExtendedExponentMarker(c)
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
