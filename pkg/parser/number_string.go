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

package parser

import (
	"math"
	"strings"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ParseSpecialFloat checks if s is +inf.0, -inf.0, +nan.0, or -nan.0
// and returns the corresponding Float value.
// Returns (nil, false) if s is not a special-value string.
func ParseSpecialFloat(s string) (*values.Float, bool) {
	switch strings.ToLower(s) {
	case "+inf.0":
		return values.NewFloat(math.Inf(1)), true
	case "-inf.0":
		return values.NewFloat(math.Inf(-1)), true
	case "+nan.0", "-nan.0":
		return values.NewFloat(math.NaN()), true
	}
	return nil, false
}

// ParseImaginaryStringNumber parses a pure imaginary string (ending in 'i')
// and returns the resulting complex number.
// Handles "+3i", "-2.5i", "+i", "-i", "+inf.0i", "-nan.0i", etc.
//
// On reject it returns a non-nil error carrying a werr sentinel (ErrInvalidFormat
// for a shape that is not this grammar; the coefficient parser's own sentinel —
// ErrInvalidFormat / ErrDivisionByZero — for a malformed coefficient). Callers
// choose how to treat it: string->number discards it and tries the next grammar
// (R7RS §6.2.7 returns #f), while the reader wraps it with a source location so
// the cause stays reachable via errors.Is / errors.Unwrap.
func ParseImaginaryStringNumber(s string) (values.Number, error) {
	if !hasSuffixFoldedI(s) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseImaginary: missing imaginary suffix: %s", s)
	}
	trim := s[:len(s)-1]

	// Handle pure sign cases: "+i" and "-i"
	if trim == "+" || trim == "" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		), nil
	}
	if trim == "-" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(-1),
		), nil
	}

	// Special imaginary values: +inf.0i, -inf.0i, +nan.0i, -nan.0i
	sf, ok := ParseSpecialFloat(trim)
	if ok {
		return values.NewComplexFromParts(0, sf.Value), nil
	}

	// Exact coefficient — integer OR rational (R7RS §6.2.5: the coefficient of
	// a pure imaginary is an exact <ureal R>, so "3/4" in "+3/4i" stays exact).
	// parseExactPart yields a BigInteger or Rational as appropriate.
	if isExactPartString(trim) {
		iam, err := parseExactPart(trim)
		if err != nil {
			return nil, err
		}
		return values.NewBigComplex(values.NewBigIntegerFromInt64(0), iam), nil
	}

	// Inexact coefficient (decimal / exponent / rational-with-decimal).
	f, err := parseFloatOrInfnan(trim)
	if err != nil {
		return nil, err
	}
	return values.NewComplexFromParts(0, f), nil
}

// findComplexSignSplit returns the index of the +/- that separates the real and
// imaginary parts of a rectangular complex string s (with the trailing 'i'
// already removed), or -1 if there is none. The scan starts at index 1 to skip a
// leading sign on the real part, ignores exponent-marker signs (e/s/f/d/l, per
// R7RS §7.1.1), and ignores the signs embedded in inf.0 / nan.0. Shared by
// parseComplex (parser_number.go) and ParseComplexStringNumber.
func findComplexSignSplit(s string) int {
	for i := 1; i < len(s); i++ {
		if s[i] != '+' && s[i] != '-' {
			continue
		}
		prev := s[i-1]
		if prev == 'e' || prev == 'E' ||
			prev == 's' || prev == 'S' ||
			prev == 'f' || prev == 'F' ||
			prev == 'd' || prev == 'D' ||
			prev == 'l' || prev == 'L' {
			continue
		}
		rest := s[i:]
		if strings.HasPrefix(rest, "+inf.0") || strings.HasPrefix(rest, "-inf.0") ||
			strings.HasPrefix(rest, "+nan.0") || strings.HasPrefix(rest, "-nan.0") ||
			rest == "+" || rest == "-" ||
			(len(rest) > 1 && (rest[1] >= '0' && rest[1] <= '9' || rest[1] == '.' || rest[1] == '/')) {
			return i
		}
	}
	return -1
}

// ParseComplexStringNumber parses a rectangular complex number string ending in 'i'.
// Handles "3+4i", "1.5-2.5i", "1+inf.0i", "0+3/4i", etc.
//
// On reject it returns a non-nil error carrying a werr sentinel (ErrInvalidFormat
// for a shape that is not this grammar; the part parser's own sentinel for a
// malformed real/imaginary part). See ParseImaginaryStringNumber for how the two
// callers treat the error differently.
func ParseComplexStringNumber(s string) (values.Number, error) {
	if !hasSuffixFoldedI(s) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseComplex: missing imaginary suffix: %s", s)
	}
	// Remove trailing 'i'
	trim := s[:len(s)-1]

	// Find the sign separating real and imaginary parts.
	signPos := findComplexSignSplit(trim)
	if signPos == -1 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseComplex: no real/imaginary separator: %s", s)
	}

	realStr := trim[:signPos]
	imagStr := trim[signPos:] // includes the sign

	// Exact path: both parts integer or rational → exact BigComplex.
	if isExactPartString(realStr) && isExactPartString(imagStr) {
		realNum, err := parseExactPart(realStr)
		if err != nil {
			return nil, err
		}
		imagNum, err := parseExactPart(imagStr)
		if err != nil {
			return nil, err
		}
		return values.NewBigComplex(realNum, imagNum), nil
	}

	// Inexact path.
	rel, err := parseFloatOrInfnan(realStr)
	if err != nil {
		return nil, err
	}

	// Exact zero imaginary → collapse to real.
	if isExactPartString(imagStr) {
		parsedImag, parseErr := parseExactPart(imagStr)
		if parseErr == nil && parsedImag.IsZero() {
			return values.NewFloat(rel), nil
		}
	}

	var img float64
	switch imagStr {
	case "+":
		img = 1
	case "-":
		img = -1
	default:
		img, err = parseFloatOrInfnan(imagStr)
		if err != nil {
			return nil, err
		}
	}
	return values.NewComplexFromParts(rel, img), nil
}

// hasSuffixFoldedI returns true if s ends with 'i' or 'I'.
func hasSuffixFoldedI(s string) bool {
	return len(s) > 0 && (s[len(s)-1] == 'i' || s[len(s)-1] == 'I')
}
