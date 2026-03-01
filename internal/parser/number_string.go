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

	"github.com/aalpar/wile/values"
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
// Returns (nil, false) if s cannot be parsed as a pure imaginary number.
func ParseImaginaryStringNumber(s string) (values.Number, bool) {
	if !hasSuffixFoldedI(s) {
		return nil, false
	}
	trim := s[:len(s)-1]

	// Handle pure sign cases: "+i" and "-i"
	if trim == "+" || trim == "" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		), true
	}
	if trim == "-" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(-1),
		), true
	}

	// Special imaginary values: +inf.0i, -inf.0i, +nan.0i, -nan.0i
	sf, ok := ParseSpecialFloat(trim)
	if ok {
		return values.NewComplexFromParts(0, sf.Value), true
	}

	// Exact integer coefficient
	if isIntegerString(trim) {
		iam, err := parseExactPart(trim)
		if err != nil {
			return nil, false
		}
		return values.NewBigComplex(values.NewBigIntegerFromInt64(0), iam), true
	}

	// Inexact coefficient
	f, err := parseFloatOrInfnan(trim)
	if err != nil {
		return nil, false
	}
	return values.NewComplexFromParts(0, f), true
}

// ParseComplexStringNumber parses a rectangular complex number string ending in 'i'.
// Handles "3+4i", "1.5-2.5i", "1+inf.0i", "0+3/4i", etc.
// Returns (nil, false) if s cannot be parsed as a complex number.
func ParseComplexStringNumber(s string) (values.Number, bool) {
	if !hasSuffixFoldedI(s) {
		return nil, false
	}
	// Remove trailing 'i'
	trim := s[:len(s)-1]

	// Find the sign separating real and imaginary parts (same logic as parseComplex).
	signPos := -1
	for i := 1; i < len(trim); i++ {
		if trim[i] != '+' && trim[i] != '-' {
			continue
		}
		// Don't split on exponent markers.
		prev := trim[i-1]
		if prev == 'e' || prev == 'E' ||
			prev == 's' || prev == 'S' ||
			prev == 'f' || prev == 'F' ||
			prev == 'd' || prev == 'D' ||
			prev == 'l' || prev == 'L' {
			continue
		}
		// The sign must precede a valid imaginary part.
		rest := trim[i:]
		if strings.HasPrefix(rest, "+inf.0") || strings.HasPrefix(rest, "-inf.0") ||
			strings.HasPrefix(rest, "+nan.0") || strings.HasPrefix(rest, "-nan.0") ||
			rest == "+" || rest == "-" ||
			(len(rest) > 1 && (rest[1] >= '0' && rest[1] <= '9' || rest[1] == '.' || rest[1] == '/')) {
			signPos = i
			break
		}
	}

	if signPos == -1 {
		return nil, false
	}

	realStr := trim[:signPos]
	imagStr := trim[signPos:] // includes the sign

	// Exact path: both parts integer or rational → exact BigComplex.
	if isExactPartString(realStr) && isExactPartString(imagStr) {
		realNum, err := parseExactPart(realStr)
		if err != nil {
			return nil, false
		}
		imagNum, err := parseExactPart(imagStr)
		if err != nil {
			return nil, false
		}
		return values.NewBigComplex(realNum, imagNum), true
	}

	// Inexact path.
	rel, err := parseFloatOrInfnan(realStr)
	if err != nil {
		return nil, false
	}

	// Exact zero imaginary → collapse to real.
	if isExactPartString(imagStr) {
		parsedImag, parseErr := parseExactPart(imagStr)
		if parseErr == nil && parsedImag.IsZero() {
			return values.NewFloat(rel), true
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
			return nil, false
		}
	}
	return values.NewComplexFromParts(rel, img), true
}

// hasSuffixFoldedI returns true if s ends with 'i' or 'I'.
func hasSuffixFoldedI(s string) bool {
	return len(s) > 0 && (s[len(s)-1] == 'i' || s[len(s)-1] == 'I')
}
