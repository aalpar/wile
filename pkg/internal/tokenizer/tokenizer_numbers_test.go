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
	"fmt"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// ---------------------------------------------------------------------------
// Binary literals (#b prefix)
// ---------------------------------------------------------------------------

func TestNumbers_BinaryLiterals(t *testing.T) {
	tcs := []struct {
		name          string
		input         string
		markerType    TokenizerState
		numberType    TokenizerState
		numberText    string
		expectEOFNext bool
	}{
		{
			name:          "unsigned binary",
			input:         "#b101",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateUnsignedInteger,
			numberText:    "101",
			expectEOFNext: true,
		},
		{
			name:          "signed negative binary",
			input:         "#b-101",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateSignedInteger,
			numberText:    "-101",
			expectEOFNext: true,
		},
		{
			name:          "signed positive binary",
			input:         "#b+1100",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateSignedInteger,
			numberText:    "+1100",
			expectEOFNext: true,
		},
		{
			name:          "binary zero",
			input:         "#b0",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateUnsignedInteger,
			numberText:    "0",
			expectEOFNext: true,
		},
		{
			name:          "binary all ones",
			input:         "#b11111111",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateUnsignedInteger,
			numberText:    "11111111",
			expectEOFNext: true,
		},
		{
			name:       "binary followed by delimiter",
			input:      "#b101)",
			markerType: TokenizerStateMarkerBase2,
			numberType: TokenizerStateUnsignedInteger,
			numberText: "101",
		},
		{
			name:          "binary case insensitive prefix",
			input:         "#B110",
			markerType:    TokenizerStateMarkerBase2,
			numberType:    TokenizerStateUnsignedInteger,
			numberText:    "110",
			expectEOFNext: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: base marker
			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, tc.markerType)

			// Second token: number
			number, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(number.Type(), qt.Equals, tc.numberType)
			c.Assert(number.String(), qt.Equals, tc.numberText)

			if tc.expectEOFNext {
				_, err = tok.Next()
				c.Assert(err, qt.ErrorIs, io.EOF)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Octal literals (#o prefix)
// ---------------------------------------------------------------------------

func TestNumbers_OctalLiterals(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		numberType TokenizerState
		numberText string
	}{
		{
			name:       "unsigned octal 177",
			input:      "#o177",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "177",
		},
		{
			name:       "signed negative octal",
			input:      "#o-77",
			numberType: TokenizerStateSignedInteger,
			numberText: "-77",
		},
		{
			name:       "signed positive octal",
			input:      "#o+10",
			numberType: TokenizerStateSignedInteger,
			numberText: "+10",
		},
		{
			name:       "octal zero",
			input:      "#o0",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "0",
		},
		{
			name:       "case insensitive prefix",
			input:      "#O377",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "377",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: base marker
			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, TokenizerStateMarkerBase8)

			// Second token: number
			number, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(number.Type(), qt.Equals, tc.numberType)
			c.Assert(number.String(), qt.Equals, tc.numberText)
		})
	}
}

// ---------------------------------------------------------------------------
// Hex literals (#x prefix)
// ---------------------------------------------------------------------------

func TestNumbers_HexLiterals(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		numberType TokenizerState
		numberText string
	}{
		{
			name:       "hex uppercase FF",
			input:      "#xFF",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "FF",
		},
		{
			name:       "hex lowercase ff",
			input:      "#xff",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "ff",
		},
		{
			name:       "hex signed negative",
			input:      "#x-1A",
			numberType: TokenizerStateSignedInteger,
			numberText: "-1A",
		},
		{
			name:       "hex signed positive",
			input:      "#x+FF",
			numberType: TokenizerStateSignedInteger,
			numberText: "+FF",
		},
		{
			name:       "hex zero",
			input:      "#x0",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "0",
		},
		{
			name:       "hex mixed case",
			input:      "#xAbCd",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "AbCd",
		},
		{
			name:       "case insensitive prefix",
			input:      "#XFF",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "FF",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, TokenizerStateMarkerBase16)

			number, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(number.Type(), qt.Equals, tc.numberType)
			c.Assert(number.String(), qt.Equals, tc.numberText)
		})
	}
}

// ---------------------------------------------------------------------------
// Exactness markers (#e, #i)
// ---------------------------------------------------------------------------

func TestNumbers_ExactnessMarkers(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		markerType TokenizerState
		nextType   TokenizerState
		nextText   string
	}{
		{
			name:       "exact marker followed by float",
			input:      "#e1.5",
			markerType: TokenizerStateMarkerNumberExact,
			nextType:   TokenizerStateUnsignedDecimalFraction,
			nextText:   "1.5",
		},
		{
			name:       "inexact marker followed by integer",
			input:      "#i3",
			markerType: TokenizerStateMarkerNumberInexact,
			nextType:   TokenizerStateUnsignedInteger,
			nextText:   "3",
		},
		{
			name:       "exact marker uppercase",
			input:      "#E1.5",
			markerType: TokenizerStateMarkerNumberExact,
			nextType:   TokenizerStateUnsignedDecimalFraction,
			nextText:   "1.5",
		},
		{
			name:       "inexact marker uppercase",
			input:      "#I42",
			markerType: TokenizerStateMarkerNumberInexact,
			nextType:   TokenizerStateUnsignedInteger,
			nextText:   "42",
		},
		{
			name:       "exact marker followed by rational",
			input:      "#e3/4",
			markerType: TokenizerStateMarkerNumberExact,
			nextType:   TokenizerStateUnsignedRationalFraction,
			nextText:   "3/4",
		},
		{
			name:       "inexact marker followed by rational",
			input:      "#i1/2",
			markerType: TokenizerStateMarkerNumberInexact,
			nextType:   TokenizerStateUnsignedRationalFraction,
			nextText:   "1/2",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, tc.markerType)

			next, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(next.Type(), qt.Equals, tc.nextType)
			c.Assert(next.String(), qt.Equals, tc.nextText)
		})
	}
}

// ---------------------------------------------------------------------------
// Combined prefixes (#e#x, #x#e, etc.)
// ---------------------------------------------------------------------------

func TestNumbers_CombinedPrefixes(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		types []TokenizerState
		texts []string
	}{
		{
			name:  "exact then hex: #e#x1A",
			input: "#e#x1A",
			types: []TokenizerState{
				TokenizerStateMarkerNumberExact,
				TokenizerStateMarkerBase16,
				TokenizerStateUnsignedInteger,
			},
			texts: []string{"#e", "#x", "1A"},
		},
		{
			name:  "hex then exact: #x#e1A",
			input: "#x#e1A",
			types: []TokenizerState{
				TokenizerStateMarkerBase16,
				TokenizerStateMarkerNumberExact,
				TokenizerStateUnsignedInteger,
			},
			texts: []string{"#x", "#e", "1A"},
		},
		{
			name:  "inexact then binary: #i#b101",
			input: "#i#b101",
			types: []TokenizerState{
				TokenizerStateMarkerNumberInexact,
				TokenizerStateMarkerBase2,
				TokenizerStateUnsignedInteger,
			},
			texts: []string{"#i", "#b", "101"},
		},
		{
			name:  "binary then inexact: #b#i101",
			input: "#b#i101",
			types: []TokenizerState{
				TokenizerStateMarkerBase2,
				TokenizerStateMarkerNumberInexact,
				TokenizerStateUnsignedInteger,
			},
			texts: []string{"#b", "#i", "101"},
		},
		{
			name:  "exact then octal: #e#o77",
			input: "#e#o77",
			types: []TokenizerState{
				TokenizerStateMarkerNumberExact,
				TokenizerStateMarkerBase8,
				TokenizerStateUnsignedInteger,
			},
			texts: []string{"#e", "#o", "77"},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			for i, expectedType := range tc.types {
				token, err := tok.Next()
				c.Assert(err, qt.IsNil, qt.Commentf("token %d", i))
				c.Assert(token.Type(), qt.Equals, expectedType, qt.Commentf("token %d", i))
				c.Assert(token.String(), qt.Equals, tc.texts[i], qt.Commentf("token %d", i))
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Special values: +inf.0, -inf.0, +nan.0, -nan.0
// ---------------------------------------------------------------------------

func TestNumbers_SpecialValues(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
		text  string
	}{
		{
			name:  "positive infinity",
			input: "+inf.0",
			state: TokenizerStateSignedInf,
			text:  "+inf.0",
		},
		{
			name:  "negative infinity",
			input: "-inf.0",
			state: TokenizerStateSignedInf,
			text:  "-inf.0",
		},
		{
			name:  "positive nan",
			input: "+nan.0",
			state: TokenizerStateSignedNan,
			text:  "+nan.0",
		},
		{
			name:  "negative nan",
			input: "-nan.0",
			state: TokenizerStateSignedNan,
			text:  "-nan.0",
		},
		{
			name:  "positive infinity imaginary",
			input: "+inf.0i",
			state: TokenizerStateSignedImaginaryInf,
			text:  "+inf.0i",
		},
		{
			name:  "negative infinity imaginary",
			input: "-inf.0i",
			state: TokenizerStateSignedImaginaryInf,
			text:  "-inf.0i",
		},
		{
			name:  "positive nan imaginary",
			input: "+nan.0i",
			state: TokenizerStateSignedImaginaryNan,
			text:  "+nan.0i",
		},
		{
			name:  "negative nan imaginary",
			input: "-nan.0i",
			state: TokenizerStateSignedImaginaryNan,
			text:  "-nan.0i",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.text)
		})
	}
}

// ---------------------------------------------------------------------------
// Complex numbers (rectangular form)
// ---------------------------------------------------------------------------

func TestNumbers_ComplexRectangular(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		// Basic integer complex
		{name: "1+2i", input: "1+2i", state: TokenizerStateUnsignedComplex},
		{name: "3-4i", input: "3-4i", state: TokenizerStateUnsignedComplex},
		{name: "0+1i", input: "0+1i", state: TokenizerStateUnsignedComplex},
		{name: "0-1i", input: "0-1i", state: TokenizerStateUnsignedComplex},

		// Decimal complex
		{name: "1.5+2.5i", input: "1.5+2.5i", state: TokenizerStateUnsignedComplex},
		{name: "3.14-2.71i", input: "3.14-2.71i", state: TokenizerStateUnsignedComplex},

		// Unit imaginary
		{name: "5+i", input: "5+i", state: TokenizerStateUnsignedComplex},
		{name: "5-i", input: "5-i", state: TokenizerStateUnsignedComplex},

		// Signed real with complex
		{name: "+1+2i", input: "+1+2i", state: TokenizerStateSignedComplex},
		{name: "-1+2i", input: "-1+2i", state: TokenizerStateSignedComplex},
		{name: "+1-2i", input: "+1-2i", state: TokenizerStateSignedComplex},
		{name: "-1-2i", input: "-1-2i", state: TokenizerStateSignedComplex},

		// Complex with inf/nan imaginary parts
		{name: "1+inf.0i", input: "1+inf.0i", state: TokenizerStateUnsignedComplex},
		{name: "1-inf.0i", input: "1-inf.0i", state: TokenizerStateUnsignedComplex},
		{name: "1+nan.0i", input: "1+nan.0i", state: TokenizerStateUnsignedComplex},

		// Inf as real part with normal imaginary
		{name: "+inf.0+1i", input: "+inf.0+1i", state: TokenizerStateSignedComplex},
		{name: "-inf.0-1i", input: "-inf.0-1i", state: TokenizerStateSignedComplex},

		// Both inf/nan
		{name: "+inf.0+nan.0i", input: "+inf.0+nan.0i", state: TokenizerStateSignedComplex},
		{name: "-inf.0-nan.0i", input: "-inf.0-nan.0i", state: TokenizerStateSignedComplex},

		// Rational components
		{name: "1/2+3/4i", input: "1/2+3/4i", state: TokenizerStateUnsignedComplex},
		{name: "1/2-3/4i", input: "1/2-3/4i", state: TokenizerStateUnsignedComplex},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.input)
		})
	}
}

// ---------------------------------------------------------------------------
// Rational numbers (N/D)
// ---------------------------------------------------------------------------

func TestNumbers_RationalFractions(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{name: "simple 3/4", input: "3/4", state: TokenizerStateUnsignedRationalFraction},
		{name: "simple 1/2", input: "1/2", state: TokenizerStateUnsignedRationalFraction},
		{name: "large numerator", input: "100/3", state: TokenizerStateUnsignedRationalFraction},
		{name: "large denominator", input: "1/1000", state: TokenizerStateUnsignedRationalFraction},
		{name: "signed positive", input: "+3/4", state: TokenizerStateSignedRationalFraction},
		{name: "signed negative", input: "-3/4", state: TokenizerStateSignedRationalFraction},
		{name: "zero numerator", input: "0/1", state: TokenizerStateUnsignedRationalFraction},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.input)
		})
	}
}

// ---------------------------------------------------------------------------
// Hash digits (R7RS §7.1.1)
// ---------------------------------------------------------------------------

func TestNumbers_HashDigits(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		state      TokenizerState
		text       string
		hasHash    bool
		usePrefix  bool
		markerType TokenizerState
	}{
		{
			name:    "integer with hash 1##",
			input:   "1##",
			state:   TokenizerStateUnsignedInteger,
			text:    "1##",
			hasHash: true,
		},
		{
			name:    "decimal with hash 1.2##",
			input:   "1.2##",
			state:   TokenizerStateUnsignedDecimalFraction,
			text:    "1.2##",
			hasHash: true,
		},
		{
			name:    "integer hash then dot 1##.",
			input:   "1##.",
			state:   TokenizerStateUnsignedDecimalFraction,
			text:    "1##.",
			hasHash: true,
		},
		{
			name:    "full hash decimal 1##.##",
			input:   "1##.##",
			state:   TokenizerStateUnsignedDecimalFraction,
			text:    "1##.##",
			hasHash: true,
		},
		{
			name:    "dot-initial with hash .5##",
			input:   ".5##",
			state:   TokenizerStateUnsignedDecimalFraction,
			text:    ".5##",
			hasHash: true,
		},
		{
			name:    "rational numerator hash 1##/3",
			input:   "1##/3",
			state:   TokenizerStateUnsignedRationalFraction,
			text:    "1##/3",
			hasHash: true,
		},
		{
			name:    "rational denominator hash 1/3##",
			input:   "1/3##",
			state:   TokenizerStateUnsignedRationalFraction,
			text:    "1/3##",
			hasHash: true,
		},
		{
			name:    "scientific with hash 1##e2",
			input:   "1##e2",
			state:   TokenizerStateUnsignedScientificNotation,
			text:    "1##e2",
			hasHash: true,
		},
		{
			name:    "no hash",
			input:   "123",
			state:   TokenizerStateUnsignedInteger,
			text:    "123",
			hasHash: false,
		},
		{
			name:       "binary with hash #b1#",
			input:      "#b1#",
			state:      TokenizerStateUnsignedInteger,
			text:       "1#",
			hasHash:    true,
			usePrefix:  true,
			markerType: TokenizerStateMarkerBase2,
		},
		{
			name:       "hex with hash #xf#",
			input:      "#xf#",
			state:      TokenizerStateUnsignedInteger,
			text:       "f#",
			hasHash:    true,
			usePrefix:  true,
			markerType: TokenizerStateMarkerBase16,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			if tc.usePrefix {
				// Skip the base marker token
				marker, err := tok.Next()
				c.Assert(err, qt.IsNil)
				c.Assert(marker.Type(), qt.Equals, tc.markerType)
			}

			token, err := tok.Next()
			if err == io.EOF {
				// Tokenize function treats EOF differently; use Tokenize helper instead
				ts, tErr := Tokenize(tc.input, false)
				c.Assert(tErr, qt.ErrorIs, io.EOF)
				if tc.usePrefix {
					c.Assert(len(ts), qt.Equals, 2)
					token = ts[1]
				} else {
					c.Assert(len(ts) >= 1, qt.IsTrue)
					token = ts[0]
				}
			} else {
				c.Assert(err, qt.IsNil)
			}
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.text)
			c.Assert(token.HasHashDigit(), qt.Equals, tc.hasHash)
		})
	}
}

// ---------------------------------------------------------------------------
// Decimal prefix (#d)
// ---------------------------------------------------------------------------

func TestNumbers_DecimalPrefix(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		numberType TokenizerState
		numberText string
	}{
		{
			name:       "decimal prefix unsigned",
			input:      "#d42",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "42",
		},
		{
			name:       "decimal prefix signed negative",
			input:      "#d-42",
			numberType: TokenizerStateSignedInteger,
			numberText: "-42",
		},
		{
			name:       "decimal prefix case insensitive",
			input:      "#D99",
			numberType: TokenizerStateUnsignedInteger,
			numberText: "99",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, TokenizerStateMarkerBase10)

			number, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(number.Type(), qt.Equals, tc.numberType)
			c.Assert(number.String(), qt.Equals, tc.numberText)
		})
	}
}

// ---------------------------------------------------------------------------
// Scientific notation
// ---------------------------------------------------------------------------

func TestNumbers_ScientificNotation(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{name: "integer exponent", input: "1e10", state: TokenizerStateUnsignedScientificNotation},
		{name: "signed exponent plus", input: "1e+10", state: TokenizerStateUnsignedScientificNotation},
		{name: "signed exponent minus", input: "1e-5", state: TokenizerStateUnsignedScientificNotation},
		{name: "uppercase E", input: "1E10", state: TokenizerStateUnsignedScientificNotation},
		{name: "signed negative mantissa", input: "-3e2", state: TokenizerStateSignedScientificNotation},
		{name: "signed positive mantissa", input: "+2e5", state: TokenizerStateSignedScientificNotation},
		// R7RS exponent markers: s, f, d, l
		{name: "short exponent s", input: "1s10", state: TokenizerStateUnsignedScientificNotation},
		{name: "float exponent f", input: "1f10", state: TokenizerStateUnsignedScientificNotation},
		{name: "double exponent d", input: "1d10", state: TokenizerStateUnsignedScientificNotation},
		{name: "long exponent l", input: "1l10", state: TokenizerStateUnsignedScientificNotation},
		// Decimal with exponent (becomes decimal fraction, not scientific notation)
		{name: "decimal with e", input: "1.5e10", state: TokenizerStateUnsignedDecimalFraction},
		{name: "decimal with E", input: "1.5E-5", state: TokenizerStateUnsignedDecimalFraction},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.input)
		})
	}
}

// ---------------------------------------------------------------------------
// Imaginary numbers (pure imaginary)
// ---------------------------------------------------------------------------

func TestNumbers_PureImaginary(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{name: "positive unit", input: "+i", state: TokenizerStateSignedImaginary},
		{name: "negative unit", input: "-i", state: TokenizerStateSignedImaginary},
		{name: "positive integer", input: "+3i", state: TokenizerStateSignedImaginary},
		{name: "negative integer", input: "-7i", state: TokenizerStateSignedImaginary},
		{name: "positive decimal", input: "+3.5i", state: TokenizerStateSignedImaginary},
		{name: "negative decimal", input: "-2.5i", state: TokenizerStateSignedImaginary},
		{name: "dot initial", input: "+.5i", state: TokenizerStateSignedImaginary},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.input)
		})
	}
}

// ---------------------------------------------------------------------------
// Polar complex numbers (R@theta)
// ---------------------------------------------------------------------------

// TestNumbers_PolarComplex asserts the whole token, not just its type, and that
// the input is consumed to end of stream.
//
// Both are load-bearing. The type is set before the angle is scanned, so a
// truncated or faulted angle still produces the right Type(); and Tokenizer.Next
// reports a fault inside a token on the *following* call, so `err IsNil` on the
// first Next() is vacuous for a lexical fault. Under the weaker assertions
// `1@1/2` passed while tokenizing as `1@1` plus a symbol `/2`, and `1@+inf.0`
// passed while being a read error.
func TestNumbers_PolarComplex(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{name: "integer@integer", input: "1@2", state: TokenizerStateUnsignedComplexPolar},
		{name: "decimal@decimal", input: "1.5@0.5", state: TokenizerStateUnsignedComplexPolar},
		{name: "rational@integer", input: "3/4@1", state: TokenizerStateUnsignedComplexPolar},
		{name: "signed positive", input: "+1@2", state: TokenizerStateSignedComplexPolar},
		{name: "signed negative", input: "-1@2", state: TokenizerStateSignedComplexPolar},
		{name: "with inf angle", input: "1@+inf.0", state: TokenizerStateUnsignedComplexPolar},
		{name: "with negative inf angle", input: "1@-inf.0", state: TokenizerStateUnsignedComplexPolar},
		{name: "with nan angle", input: "1@+nan.0", state: TokenizerStateUnsignedComplexPolar},
		{name: "with negative nan angle", input: "1@-nan.0", state: TokenizerStateUnsignedComplexPolar},
		// <ureal R> includes <uinteger>/<uinteger>, in the angle as in the magnitude.
		{name: "integer@rational", input: "1@1/2", state: TokenizerStateUnsignedComplexPolar},
		{name: "rational@rational", input: "1/2@3/4", state: TokenizerStateUnsignedComplexPolar},
		{name: "signed angle rational", input: "1@-1/2", state: TokenizerStateUnsignedComplexPolar},
		{name: "exponent angle", input: "1@1e2", state: TokenizerStateUnsignedComplexPolar},
		{name: "dot angle", input: "1@.5", state: TokenizerStateUnsignedComplexPolar},
		// <infnan> is a <real R>, so it is a legal magnitude too.
		{name: "inf magnitude", input: "+inf.0@1", state: TokenizerStateSignedComplexPolar},
		{name: "negative inf magnitude", input: "-inf.0@1", state: TokenizerStateSignedComplexPolar},
		{name: "nan magnitude", input: "+nan.0@1", state: TokenizerStateSignedComplexPolar},
		{name: "infnan both sides", input: "+inf.0@+inf.0", state: TokenizerStateSignedComplexPolar},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, tc.input)
			_, err = tok.Next()
			c.Assert(err, qt.Equals, io.EOF, qt.Commentf("input not fully consumed"))
		})
	}
}

// TestNumbers_PolarStateIsTheCallersChoice pins the signed/unsigned distinction
// against the input's *suffix*. mayReadPolarPart used to end by assigning
// TokenizerStateUnsignedComplexPolar unconditionally, so `-1@2` was signed at end
// of input (an early return fired first) and unsigned inside a list.
func TestNumbers_PolarStateIsTheCallersChoice(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{name: "signed at eof", input: "-1@2", state: TokenizerStateSignedComplexPolar},
		{name: "signed delimited", input: "-1@2)", state: TokenizerStateSignedComplexPolar},
		{name: "signed delimited decimal", input: "-1@1.5)", state: TokenizerStateSignedComplexPolar},
		{name: "signed delimited dot angle", input: "-1@.5)", state: TokenizerStateSignedComplexPolar},
		{name: "plus signed delimited", input: "+1@2)", state: TokenizerStateSignedComplexPolar},
		{name: "unsigned delimited", input: "1@2)", state: TokenizerStateUnsignedComplexPolar},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
			c.Assert(token.String(), qt.Equals, strings.TrimSuffix(tc.input, ")"))
		})
	}
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestNumbers_ErrorCases(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{name: "empty exponent", input: "1e"},
		{name: "exponent with plus only", input: "1e+"},
		{name: "exponent with minus only", input: "1e-"},
		{name: "decimal empty exponent", input: "1.5e"},
		{name: "decimal exponent plus only", input: "1.5e+"},
		{name: "dot-initial empty exponent", input: ".5e"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Assert(p.err, qt.Not(qt.IsNil), qt.Commentf("expected error for input %q", tc.input))
			var tokErr *TokenizerError
			c.Assert(p.err, qt.ErrorAs, &tokErr)
		})
	}
}

// ---------------------------------------------------------------------------
// Malformed special values: verify no error (may parse as symbol or number)
// ---------------------------------------------------------------------------

func TestNumbers_MalformedSpecials(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		// Malformed inf/nan: some fall back to symbols, others don't.
		// The key invariant is that they parse without error.
		{name: "malformed inf: +inx", input: "+inx"},
		{name: "malformed inf: -inx", input: "-inx"},
		{name: "malformed nan: +nax", input: "+nax"},
		{name: "malformed nan: -nax", input: "-nax"},
		{name: "partial inf: +in", input: "+in"},
		{name: "partial nan: +na", input: "+na"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token, qt.IsNotNil)
		})
	}
}

// ---------------------------------------------------------------------------
// Numbers using the Tokenize convenience function
// ---------------------------------------------------------------------------

func TestNumbers_TokenizeConvenience(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		numTokens int
		types     []TokenizerState
	}{
		{
			name:      "simple integer",
			input:     "42",
			numTokens: 1,
			types:     []TokenizerState{TokenizerStateUnsignedInteger},
		},
		{
			name:      "binary with prefix",
			input:     "#b101",
			numTokens: 2,
			types: []TokenizerState{
				TokenizerStateMarkerBase2,
				TokenizerStateUnsignedInteger,
			},
		},
		{
			name:      "multiple numbers",
			input:     "1 2 3",
			numTokens: 3,
			types: []TokenizerState{
				TokenizerStateUnsignedInteger,
				TokenizerStateUnsignedInteger,
				TokenizerStateUnsignedInteger,
			},
		},
		{
			name:      "complex in expression",
			input:     "(1+2i)",
			numTokens: 3,
			types: []TokenizerState{
				TokenizerStateOpenParen,
				TokenizerStateUnsignedComplex,
				TokenizerStateCloseParen,
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tokens, err := Tokenize(tc.input, false)
			c.Assert(err, qt.ErrorIs, io.EOF)
			c.Assert(len(tokens), qt.Equals, tc.numTokens)
			for i, expectedType := range tc.types {
				c.Assert(tokens[i].Type(), qt.Equals, expectedType,
					qt.Commentf("token %d: got %v", i, tokens[i].Type()))
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Base-specific rational fractions in tokenization
// ---------------------------------------------------------------------------

func TestNumbers_BaseRationals(t *testing.T) {
	tcs := []struct {
		name       string
		input      string
		markerType TokenizerState
		numberType TokenizerState
		numberText string
	}{
		{
			name:       "binary rational",
			input:      "#b101/10",
			markerType: TokenizerStateMarkerBase2,
			numberType: TokenizerStateUnsignedRationalFraction,
			numberText: "101/10",
		},
		{
			name:       "octal rational",
			input:      "#o7/3",
			markerType: TokenizerStateMarkerBase8,
			numberType: TokenizerStateUnsignedRationalFraction,
			numberText: "7/3",
		},
		{
			name:       "hex rational",
			input:      "#x10/8",
			markerType: TokenizerStateMarkerBase16,
			numberType: TokenizerStateUnsignedRationalFraction,
			numberText: "10/8",
		},
		{
			name:       "signed binary rational",
			input:      "#b-110/11",
			markerType: TokenizerStateMarkerBase2,
			numberType: TokenizerStateSignedRationalFraction,
			numberText: "-110/11",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			marker, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(marker.Type(), qt.Equals, tc.markerType)

			number, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(number.Type(), qt.Equals, tc.numberType)
			c.Assert(number.String(), qt.Equals, tc.numberText)
		})
	}
}

// ---------------------------------------------------------------------------
// Signed integers and trailing dot
// ---------------------------------------------------------------------------

func TestNumbers_SignedIntegers(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
	}{
		{"+0", TokenizerStateSignedInteger},
		{"-0", TokenizerStateSignedInteger},
		{"+1", TokenizerStateSignedInteger},
		{"-1", TokenizerStateSignedInteger},
		{"+999", TokenizerStateSignedInteger},
		{"-999", TokenizerStateSignedInteger},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
		})
	}
}

// ---------------------------------------------------------------------------
// Trailing dot decimals (R7RS allows "1.")
// ---------------------------------------------------------------------------

func TestNumbers_TrailingDotDecimals(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
	}{
		{"1.", TokenizerStateUnsignedDecimalFraction},
		{"+1.", TokenizerStateSignedDecimalFraction},
		{"-1.", TokenizerStateSignedDecimalFraction},
		{"0.", TokenizerStateUnsignedDecimalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil)
			c.Assert(token.Type(), qt.Equals, tc.state)
		})
	}
}
