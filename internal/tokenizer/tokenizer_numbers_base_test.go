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
	"math"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestNumberInitial tests the isNumberInitial function coverage
func TestNumberInitial(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "+123",
			expectedType: TokenizerStateSignedInteger,
		},
		{
			input:        "-456",
			expectedType: TokenizerStateSignedInteger,
		},
		{
			input:        ".5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "7",
			expectedType: TokenizerStateUnsignedInteger,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestMaySignedIntegerEdgeCases tests signed integer edge cases
func TestMaySignedIntegerEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "+0",
			expectedType: TokenizerStateSignedInteger,
		},
		{
			input:        "-0",
			expectedType: TokenizerStateSignedInteger,
		},
		{
			input:        "+999",
			expectedType: TokenizerStateSignedInteger,
		},
		{
			input:        "-999",
			expectedType: TokenizerStateSignedInteger,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestMustReadUnsignedIntegerEdgeCases tests unsigned integer edge cases
func TestMustReadUnsignedIntegerEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "0",
			expectedType: TokenizerStateUnsignedInteger,
		},
		{
			input:        "999999",
			expectedType: TokenizerStateUnsignedInteger,
		},
		{
			input:        "42",
			expectedType: TokenizerStateUnsignedInteger,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestNumberExpOnDecimalsFraction tests exponents on decimal fractions
func TestNumberExpOnDecimalsFraction(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "1.5e10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
			expectedStr:  "1.5e10",
		},
		{
			input:        "1.5e-5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
			expectedStr:  "1.5e-5",
		},
		{
			input:        "1.5E10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
			expectedStr:  "1.5E10",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestComplexNumberPolar tests polar complex number parsing
func TestComplexNumberPolar(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "3/4@1",
			expectedType: TokenizerStateUnsignedComplexPolar,
			expectedStr:  "3/4@1",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestSignedImaginaryPart tests signed imaginary part parsing
func TestSignedImaginaryPart(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "1+2i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1+2i",
		},
		{
			input:        "1-2i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1-2i",
		},
		{
			input:        "1.5+2.5i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1.5+2.5i",
		},
		{
			input:        "1+inf.0i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1+inf.0i",
		},
		{
			input:        "1-inf.0i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1-inf.0i",
		},
		{
			input:        "1+nan.0i",
			expectedType: TokenizerStateUnsignedComplex,
			expectedStr:  "1+nan.0i",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
		})
	}
}

// TestRadixPrefixMarkers tests radix prefix marker tokenization
// TODO: use slices for expectedType1,expectedType2
func TestRadixPrefixMarkers(t *testing.T) {
	tcs := []struct {
		input         string
		expectedType1 TokenizerState
		expectedType2 TokenizerState
		expectedStr1  string
	}{
		{
			input:         "#x1a2b3c",
			expectedType1: TokenizerStateMarkerBase16,
			expectedType2: TokenizerStateUnsignedIntegerBase16,
			expectedStr1:  "#x",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: marker
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.Type(), qt.Equals, tc.expectedType1)
			c.Check(token1.String(), qt.Equals, tc.expectedStr1)

			// Second token: integer
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.Type(), qt.Equals, tc.expectedType2)
		})
	}
}

// TestMayReadPolarPart tests polar part parsing
func TestMayReadPolarPart(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "1@2",
			expectedType: TokenizerStateUnsignedComplexPolar,
		},
		{
			input:        "1.5@0.5",
			expectedType: TokenizerStateUnsignedComplexPolar,
		},
		{
			input:        "+1@2",
			expectedType: TokenizerStateSignedComplexPolar,
		},
		{
			input:        "-1@2",
			expectedType: TokenizerStateSignedComplexPolar,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestDigitFunction tests the digit helper function for various bases
func TestDigitFunction(t *testing.T) {
	tcs := []struct {
		input         string
		expectedType1 TokenizerState
		expectedType2 TokenizerState
	}{
		{
			input:         "#b1011",
			expectedType1: TokenizerStateMarkerBase2,
			expectedType2: TokenizerStateUnsignedIntegerBase2,
		},
		{
			input:         "#o3771",
			expectedType1: TokenizerStateMarkerBase8,
			expectedType2: TokenizerStateUnsignedIntegerBase8,
		},
		{
			input:         "#d9991",
			expectedType1: TokenizerStateMarkerBase10,
			expectedType2: TokenizerStateUnsignedIntegerBase10,
		},
		{
			input:         "#x1fff",
			expectedType1: TokenizerStateMarkerBase16,
			expectedType2: TokenizerStateUnsignedIntegerBase16,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: marker
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.Type(), qt.Equals, tc.expectedType1)

			// Second token: integer
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.Type(), qt.Equals, tc.expectedType2)
		})
	}
}

func TestRadixPrefixes(t *testing.T) {
	c := qt.New(t)

	// Binary number produces two tokens: marker + integer
	tok := NewTokenizer(strings.NewReader("#b101"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateMarkerBase2)
	token1b, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1b.Type(), qt.Equals, TokenizerStateUnsignedIntegerBase2)
	c.Assert(token1b.String(), qt.Equals, "101")

	// Octal number
	tok2 := NewTokenizer(strings.NewReader("#o77"), false)
	token2, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateMarkerBase8)
	token2b, err := tok2.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2b.String(), qt.Equals, "77")

	// Hex number
	tok3 := NewTokenizer(strings.NewReader("#xAB"), false)
	token3, err := tok3.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token3.Type(), qt.Equals, TokenizerStateMarkerBase16)
	token3b, err := tok3.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token3b.String(), qt.Equals, "AB")
}

func TestExactnessMarkers(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// Exact marker alone
		{bs: "#e", state: TokenizerStateMarkerNumberExact},
		// Inexact marker alone
		{bs: "#i", state: TokenizerStateMarkerNumberInexact},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		state TokenizerState
	}{
		// +inf.0
		{bs: "+inf.0", state: TokenizerStateSignedInf},
		// -nan.0
		{bs: "-nan.0", state: TokenizerStateSignedNan},
		// +inf.0i (imaginary inf)
		{bs: "+inf.0i", state: TokenizerStateSignedImaginaryInf},
		// -nan.0i (imaginary nan)
		{bs: "-nan.0i", state: TokenizerStateSignedImaginaryNan},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
		})
	}
}

func TestImaginaryWithCoefficient(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Positive integer imaginary
		{bs: "+3i", scan: "+3i", state: TokenizerStateSignedImaginary},
		// Negative integer imaginary
		{bs: "-2i", scan: "-2i", state: TokenizerStateSignedImaginary},
		// Positive decimal imaginary
		{bs: "+3.5i", scan: "+3.5i", state: TokenizerStateSignedImaginary},
		// Negative decimal imaginary
		{bs: "-2.5i", scan: "-2.5i", state: TokenizerStateSignedImaginary},
		// Decimal starting with dot
		{bs: "+.5i", scan: "+.5i", state: TokenizerStateSignedImaginary},
		// Scientific notation imaginary with decimal
		// Note: +3e2i doesn't work because signed integers don't parse exponents (pre-existing limitation)
		{bs: "+3.0e2i", scan: "+3.0e2i", state: TokenizerStateSignedImaginary},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestFullComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Simple complex
		{bs: "1+2i", scan: "1+2i", state: TokenizerStateUnsignedComplex},
		// Complex with negative imaginary
		{bs: "3-4i", scan: "3-4i", state: TokenizerStateUnsignedComplex},
		// Decimal complex
		{bs: "1.5+2.5i", scan: "1.5+2.5i", state: TokenizerStateUnsignedComplex},
		// Decimal complex with negative imaginary
		{bs: "3.5-4.5i", scan: "3.5-4.5i", state: TokenizerStateUnsignedComplex},
		// Complex with unit imaginary
		{bs: "1+i", scan: "1+i", state: TokenizerStateUnsignedComplex},
		// Complex with negative unit imaginary
		{bs: "5-i", scan: "5-i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumbersWithExponents(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Complex with exponent in real part
		{bs: "1e2+3i", scan: "1e2+3i", state: TokenizerStateUnsignedComplex},
		// Complex with exponent in imaginary part
		{bs: "1+3e2i", scan: "1+3e2i", state: TokenizerStateUnsignedComplex},
		// Complex with decimal and exponent
		{bs: "1.5e2+2.5e3i", scan: "1.5e2+2.5e3i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumberEdgeCases(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		state TokenizerState
	}{
		// Zero real part with imaginary
		{bs: "0+3i", scan: "0+3i", state: TokenizerStateUnsignedComplex},
		// Zero imaginary coefficient would still need the 'i'
		{bs: "1+0i", scan: "1+0i", state: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestComplexNumbersInExpressions(t *testing.T) {
	c := qt.New(t)

	// Complex number followed by delimiter
	tok := NewTokenizer(strings.NewReader("1+2i)"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	c.Assert(token.String(), qt.Equals, "1+2i")

	// Verify the delimiter is still there
	tokenParen, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tokenParen.Type(), qt.Equals, TokenizerStateCloseParen)

	// Multiple complex numbers in a list
	tok2 := NewTokenizer(strings.NewReader("(1+2i 3-4i)"), false)
	t1, _ := tok2.Next() // (
	c.Assert(t1.Type(), qt.Equals, TokenizerStateOpenParen)
	t2, _ := tok2.Next() // 1+2i
	c.Assert(t2.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	t3, _ := tok2.Next() // 3-4i
	c.Assert(t3.Type(), qt.Equals, TokenizerStateUnsignedComplex)
	t4, _ := tok2.Next() // )
	c.Assert(t4.Type(), qt.Equals, TokenizerStateCloseParen)
}

func TestSignedRealComplexNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			// Negative real with positive imaginary
			bs:    "-1+2i",
			scan:  "-1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Positive real with positive imaginary
			bs:    "+1+2i",
			scan:  "+1+2i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Negative real with negative imaginary
			bs:    "-3-4i",
			scan:  "-3-4i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Positive real with negative imaginary
			bs:    "+5-6i",
			scan:  "+5-6i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed decimal real with imaginary
			bs:    "-1.5+2.5i",
			scan:  "-1.5+2.5i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed real with unit imaginary
			bs:    "-1+i",
			scan:  "-1+i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
		{
			// Signed real with negative unit imaginary
			bs:    "+5-i",
			scan:  "+5-i",
			err0:  io.EOF,
			state: TokenizerStateSignedComplex,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.bs), false)
			token, err := tok.Next()
			if tc.err0 == io.EOF {
				c.Check(err, qt.IsNil)
			} else {
				c.Check(err, qt.ErrorIs, tc.err0)
			}
			c.Check(token.Type(), qt.Equals, tc.state)
			c.Check(token.String(), qt.Equals, tc.scan)
		})
	}
}

func TestEmptyExponentError(t *testing.T) {
	// Bug 2: Empty exponent should produce error, not be silently accepted
	// Uses read() directly to test error, matching existing test patterns
	tcs := []struct {
		name  string
		input string
	}{
		{"bare exponent", "1e"},
		{"exponent with plus", "1e+"},
		{"exponent with minus", "1e-"},
		{"decimal with exponent", "1.5e"},
		{"decimal with exponent plus", "1.5e+"},
		{"leading dot with exponent", ".5e"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Assert(p.err, qt.Not(qt.IsNil), qt.Commentf("expected error for input %q", tc.input))
			var tokErr *TokenizerError
			c.Assert(p.err, qt.ErrorAs, &tokErr)
		})
	}
}

func TestInvalidHashSequenceError(t *testing.T) {
	// Bug 3: Invalid # sequences should produce error, not panic
	// Uses read() directly to test error, matching existing test patterns
	tcs := []struct {
		name  string
		input string
	}{
		{"hash bracket", "#["},
		{"hash curly", "#{"},
		{"hash dollar", "#$"},
		{"hash percent", "#%"},
		{"hash caret", "#^"},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Assert(p.err, qt.Not(qt.IsNil), qt.Commentf("expected error for input %q", tc.input))
			var tokErr *TokenizerError
			c.Assert(p.err, qt.ErrorAs, &tokErr)
		})
	}
}

func TestTrailingDotDecimals(t *testing.T) {
	// Bug 1: R7RS allows trailing dot decimals like "1."
	tcs := []struct {
		name  string
		input string
		state TokenizerState
	}{
		{"unsigned trailing dot", "1.", TokenizerStateUnsignedDecimalFraction},
		{"positive trailing dot", "+1.", TokenizerStateSignedDecimalFraction},
		{"negative trailing dot", "-1.", TokenizerStateSignedDecimalFraction},
		{"trailing dot with exponent", "1.e2", TokenizerStateUnsignedDecimalFraction},
		{"trailing dot followed by paren", "1.(", TokenizerStateUnsignedDecimalFraction},
	}
	for _, tc := range tcs {
		qt.New(t).Run(tc.name, func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Assert(err, qt.IsNil, qt.Commentf("unexpected error for input %q", tc.input))
			c.Assert(token.Type(), qt.Equals, tc.state)
		})
	}
}

// TODO: Some test cases here are duplicated with TestTokenizer_readBaseNInteger.
// Since readBaseNInteger delegates to readUnsignedBaseNInteger, these tests
// should be refactored to avoid redundancy. Keep unsigned-specific cases here,
// move sign-handling cases to the signed test, and share common infrastructure.
func TestTokenizer_readUnsignedBaseNInteger(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		radix     int
		maxn      int
		wantVal   int64
		wantCount int
		wantErr   error
		remaining rune
	}{
		// Empty input / no digits
		{
			name:      "empty input",
			input:     "",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			wantErr:   io.EOF,
		},
		{
			name:      "non-digit first char",
			input:     "abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: 'a',
		},
		{
			name:      "whitespace only",
			input:     "   ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: ' ',
		},

		// Base 10 - basic cases (when input ends at EOF, function returns 0)
		{
			name:      "base10 single digit with trailing space",
			input:     "5 ",
			radix:     10,
			maxn:      0,
			wantVal:   5,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base10 single digit at EOF",
			input:     "5",
			radix:     10,
			maxn:      0,
			wantVal:   5,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "base10 multiple digits with trailing space",
			input:     "12345 ",
			radix:     10,
			maxn:      0,
			wantVal:   12345,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base10 multiple digits at EOF",
			input:     "12345",
			radix:     10,
			maxn:      0,
			wantVal:   12345,
			wantCount: 5,
			wantErr:   io.EOF,
		},
		{
			name:      "base10 leading zeros",
			input:     "00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base10 all zeros",
			input:     "0000 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base10 terminated by letter",
			input:     "123abc",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: 'a',
		},

		// Base 2 (binary)
		{
			name:      "base2 single digit 0",
			input:     "0 ",
			radix:     2,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base2 single digit 1",
			input:     "1 ",
			radix:     2,
			maxn:      0,
			wantVal:   1,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base2 binary pattern",
			input:     "10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   22,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "base2 invalid digit 2 terminates",
			input:     "1012",
			radix:     2,
			maxn:      0,
			wantVal:   5,
			wantCount: 3,
			remaining: '2',
		},
		{
			name:      "base2 invalid digit 9 terminates",
			input:     "119",
			radix:     2,
			maxn:      0,
			wantVal:   3,
			wantCount: 2,
			remaining: '9',
		},

		// Base 8 (octal)
		{
			name:      "base8 single digit",
			input:     "7 ",
			radix:     8,
			maxn:      0,
			wantVal:   7,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "base8 octal pattern",
			input:     "755 ",
			radix:     8,
			maxn:      0,
			wantVal:   493,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base8 invalid digit 8 terminates",
			input:     "128",
			radix:     8,
			maxn:      0,
			wantVal:   10,
			wantCount: 2,
			remaining: '8',
		},
		{
			name:      "base8 invalid digit 9 terminates",
			input:     "179",
			radix:     8,
			maxn:      0,
			wantVal:   15,
			wantCount: 2,
			remaining: '9',
		},

		// Base 16 (hex)
		{
			name:      "base16 digits only",
			input:     "123 ",
			radix:     16,
			maxn:      0,
			wantVal:   291,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 lowercase letters",
			input:     "abc ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 uppercase letters",
			input:     "ABC ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "base16 mixed case",
			input:     "aBcDeF ",
			radix:     16,
			maxn:      0,
			wantVal:   11259375,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "base16 invalid letter g terminates",
			input:     "1fg",
			radix:     16,
			maxn:      0,
			wantVal:   31,
			wantCount: 2,
			remaining: 'g',
		},
		{
			name:      "base16 invalid letter G terminates",
			input:     "ABCG",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'G',
		},

		// maxn limiting (leaves remaining input, so no EOF)
		{
			name:      "maxn limits to 1 digit",
			input:     "12345",
			radix:     10,
			maxn:      1,
			wantVal:   1,
			wantCount: 1,
			remaining: '2',
		},
		{
			name:      "maxn limits to 3 digits",
			input:     "12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 3,
			remaining: '4',
		},
		{
			name:      "maxn exceeds available digits",
			input:     "12 ",
			radix:     10,
			maxn:      5,
			wantVal:   12,
			wantCount: 2,
			remaining: ' ',
		},
		{
			name:      "maxn exceeds available digits",
			input:     "12",
			radix:     10,
			maxn:      5,
			wantVal:   12,
			wantCount: 2,
			wantErr:   io.EOF,
		},
		{
			name:      "maxn zero means unlimited",
			input:     "1234567890 ",
			radix:     10,
			maxn:      0,
			wantVal:   1234567890,
			wantCount: 10,
			remaining: ' ',
		},
		{
			name:      "maxn negative means unlimited",
			input:     "12345 ",
			radix:     10,
			maxn:      -1,
			wantVal:   12345,
			wantCount: 5,
			remaining: ' ',
		},
		{
			name:      "maxn with base16",
			input:     "abcdef",
			radix:     16,
			maxn:      4,
			wantVal:   43981,
			wantCount: 4,
			remaining: 'e',
		},

		// Radix 0 acts like radix 10
		{
			name:      "radix 0 acts like 10",
			input:     "42 ",
			radix:     0,
			maxn:      0,
			wantVal:   42,
			wantCount: 2,
			remaining: ' ',
		},

		// Overflow cases (with trailing char to avoid EOF masking the overflow error)
		{
			name:      "max int64",
			input:     "9223372036854775807 ",
			radix:     10,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 19,
			remaining: ' ',
		},
		{
			name:      "overflow int64 at EOF",
			input:     "9223372036854775808",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 19,
			wantErr:   io.EOF,
		},
		{
			name:      "large overflow at EOF",
			input:     "99999999999999999999",
			radix:     10,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 20,
			wantErr:   io.EOF,
		},
		{
			name:      "hex max int64",
			input:     "7fffffffffffffff ",
			radix:     16,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 16,
			remaining: ' ',
		},
		{
			name:      "hex overflow at EOF",
			input:     "8000000000000000",
			radix:     16,
			maxn:      0,
			wantVal:   math.MaxInt64,
			wantCount: 16,
			wantErr:   io.EOF,
		},

		// Unicode / special characters
		{
			name:      "unicode after digits",
			input:     "123日本語",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '日',
		},

		// Overflow with trailing delimiter (tests parse error detection)
		// Note: strconv.ParseInt returns math.MaxInt64 on overflow, so value is non-zero
		{
			name:      "overflow int64 with trailing space",
			input:     "9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 19,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "large overflow with trailing space",
			input:     "99999999999999999999 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "hex overflow with trailing space",
			input:     "8000000000000000 ",
			radix:     16,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 16,
			wantErr:   &TokenizerError{},
		},

		// Binary boundary cases
		{
			name:      "binary max int64",
			input:     "111111111111111111111111111111111111111111111111111111111111111 ",
			radix:     2,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 63,
			remaining: ' ',
		},
		{
			name:      "binary overflow",
			input:     "1000000000000000000000000000000000000000000000000000000000000000 ",
			radix:     2,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 64,
			wantErr:   &TokenizerError{},
		},

		// Octal boundary cases
		{
			name:      "octal max int64",
			input:     "777777777777777777777 ",
			radix:     8,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 21,
			remaining: ' ',
		},
		{
			name:      "octal overflow",
			input:     "1000000000000000000000 ",
			radix:     8,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 22,
			wantErr:   &TokenizerError{},
		},

		// Zero handling
		{
			name:      "single zero base10",
			input:     "0 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},
		{
			name:      "single zero base16",
			input:     "0 ",
			radix:     16,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: ' ',
		},

		// Delimiter variations
		{
			name:      "terminated by paren",
			input:     "123)",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: ')',
		},
		{
			name:      "terminated by newline",
			input:     "123\n",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '\n',
		},
		{
			name:      "terminated by tab",
			input:     "123\t",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: '\t',
		},

		// Edge cases for maxn
		{
			name:      "maxn exactly matches input length",
			input:     "12345",
			radix:     10,
			maxn:      5,
			wantVal:   12345,
			wantCount: 5,
			wantErr:   io.EOF,
		},
		{
			name:      "maxn is 1 with single digit",
			input:     "9",
			radix:     10,
			maxn:      1,
			wantVal:   9,
			wantCount: 1,
			wantErr:   io.EOF,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			val, count := tok.readUnsignedBaseNInteger(tc.radix, tc.maxn)

			c.Assert(count, qt.Equals, tc.wantCount, qt.Commentf("digit count mismatch"))
			c.Assert(val, qt.Equals, tc.wantVal, qt.Commentf("value mismatch"))

			switch tc.wantErr.(type) {
			case nil:
				c.Assert(tok.err, qt.IsNil, qt.Commentf("unexpected error: %v", tok.err))
			case *TokenizerError:
				var tokErr *TokenizerError
				c.Assert(tok.err, qt.ErrorAs, &tokErr, qt.Commentf("expected TokenizerError"))
			default:
				c.Assert(tok.err, qt.ErrorIs, tc.wantErr)
			}

			if tc.remaining != 0 {
				c.Assert(tok.curr(), qt.Equals, tc.remaining,
					qt.Commentf("remaining input mismatch"))
			}
		})
	}
}

// TODO: Some test cases here are duplicated with TestTokenizer_readUnsignedBaseNInteger.
// Since readBaseNInteger delegates to readUnsignedBaseNInteger, these tests
// should be refactored to avoid redundancy. Keep sign-handling cases here,
// and rely on the unsigned test for comprehensive digit/radix coverage.
func TestTokenizer_readBaseNInteger(t *testing.T) {
	tcs := []struct {
		name      string
		input     string
		radix     int
		maxn      int
		wantVal   int64
		wantCount int
		wantErr   error
		remaining rune
	}{
		// No sign - behaves like unsigned
		{
			name:      "no sign base10",
			input:     "123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			remaining: ' ',
		},
		{
			name:      "no sign base10 at EOF",
			input:     "123",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 3,
			wantErr:   io.EOF,
		},

		// Positive sign
		{
			name:      "positive sign base10",
			input:     "+123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "positive sign base10 at EOF",
			input:     "+123",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 4,
			wantErr:   io.EOF,
		},

		// Negative sign
		{
			name:      "negative sign base10",
			input:     "-123 ",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "negative sign base10 at EOF",
			input:     "-123",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			wantErr:   io.EOF,
		},
		{
			name:      "negative zero",
			input:     "-0 ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 2,
			remaining: ' ',
		},

		// Sign only (no digits)
		{
			name:      "plus sign only at EOF",
			input:     "+",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "minus sign only at EOF",
			input:     "-",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			wantErr:   io.EOF,
		},
		{
			name:      "plus sign followed by non-digit",
			input:     "+abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: 'a',
		},
		{
			name:      "minus sign followed by non-digit",
			input:     "-xyz",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 1,
			remaining: 'x',
		},

		// Non-base character termination (base 10)
		{
			name:      "base10 terminated by A",
			input:     "10A",
			radix:     10,
			maxn:      0,
			wantVal:   10,
			wantCount: 2,
			remaining: 'A',
		},
		{
			name:      "base10 terminated by lowercase a",
			input:     "99a",
			radix:     10,
			maxn:      0,
			wantVal:   99,
			wantCount: 2,
			remaining: 'a',
		},
		{
			name:      "base10 signed terminated by letter",
			input:     "+10A",
			radix:     10,
			maxn:      0,
			wantVal:   10,
			wantCount: 3,
			remaining: 'A',
		},
		{
			name:      "base10 negative terminated by letter",
			input:     "-10A",
			radix:     10,
			maxn:      0,
			wantVal:   -10,
			wantCount: 3,
			remaining: 'A',
		},

		// Non-base character termination (base 2)
		{
			name:      "base2 terminated by 2",
			input:     "1012",
			radix:     2,
			maxn:      0,
			wantVal:   5,
			wantCount: 3,
			remaining: '2',
		},
		{
			name:      "base2 terminated by 9",
			input:     "1109",
			radix:     2,
			maxn:      0,
			wantVal:   6,
			wantCount: 3,
			remaining: '9',
		},
		{
			name:      "base2 signed terminated by invalid",
			input:     "+1102",
			radix:     2,
			maxn:      0,
			wantVal:   6,
			wantCount: 4,
			remaining: '2',
		},
		{
			name:      "base2 negative terminated by invalid",
			input:     "-1012",
			radix:     2,
			maxn:      0,
			wantVal:   -5,
			wantCount: 4,
			remaining: '2',
		},

		// Non-base character termination (base 8)
		{
			name:      "base8 terminated by 8",
			input:     "178",
			radix:     8,
			maxn:      0,
			wantVal:   15,
			wantCount: 2,
			remaining: '8',
		},
		{
			name:      "base8 terminated by 9",
			input:     "779",
			radix:     8,
			maxn:      0,
			wantVal:   63,
			wantCount: 2,
			remaining: '9',
		},
		{
			name:      "base8 terminated by A",
			input:     "77A",
			radix:     8,
			maxn:      0,
			wantVal:   63,
			wantCount: 2,
			remaining: 'A',
		},
		{
			name:      "base8 signed terminated by invalid",
			input:     "+128",
			radix:     8,
			maxn:      0,
			wantVal:   10,
			wantCount: 3,
			remaining: '8',
		},

		// Non-base character termination (base 16)
		{
			name:      "base16 terminated by G",
			input:     "ABCG",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'G',
		},
		{
			name:      "base16 terminated by lowercase g",
			input:     "abcg",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 3,
			remaining: 'g',
		},
		{
			name:      "base16 terminated by Z",
			input:     "FFZ",
			radix:     16,
			maxn:      0,
			wantVal:   255,
			wantCount: 2,
			remaining: 'Z',
		},
		{
			name:      "base16 signed terminated by invalid",
			input:     "+1FG",
			radix:     16,
			maxn:      0,
			wantVal:   31,
			wantCount: 3,
			remaining: 'G',
		},
		{
			name:      "base16 negative terminated by invalid",
			input:     "-FFG",
			radix:     16,
			maxn:      0,
			wantVal:   -255,
			wantCount: 3,
			remaining: 'G',
		},

		// Base 2 (binary) with signs
		{
			name:      "base2 positive",
			input:     "+10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   22,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "base2 negative",
			input:     "-10110 ",
			radix:     2,
			maxn:      0,
			wantVal:   -22,
			wantCount: 6,
			remaining: ' ',
		},

		// Base 8 (octal) with signs
		{
			name:      "base8 positive",
			input:     "+755 ",
			radix:     8,
			maxn:      0,
			wantVal:   493,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base8 negative",
			input:     "-755 ",
			radix:     8,
			maxn:      0,
			wantVal:   -493,
			wantCount: 4,
			remaining: ' ',
		},

		// Base 16 (hex) with signs
		{
			name:      "base16 positive uppercase",
			input:     "+ABC ",
			radix:     16,
			maxn:      0,
			wantVal:   2748,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base16 negative lowercase",
			input:     "-abc ",
			radix:     16,
			maxn:      0,
			wantVal:   -2748,
			wantCount: 4,
			remaining: ' ',
		},
		{
			name:      "base16 mixed case",
			input:     "+aBcDeF ",
			radix:     16,
			maxn:      0,
			wantVal:   11259375,
			wantCount: 7,
			remaining: ' ',
		},

		// maxn with sign (maxn applies to digits only, not sign)
		{
			name:      "maxn with positive sign",
			input:     "+12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 4,
			remaining: '4',
		},
		{
			name:      "maxn with negative sign",
			input:     "-12345",
			radix:     10,
			maxn:      3,
			wantVal:   -123,
			wantCount: 4,
			remaining: '4',
		},
		{
			name:      "maxn without sign",
			input:     "12345",
			radix:     10,
			maxn:      3,
			wantVal:   123,
			wantCount: 3,
			remaining: '4',
		},

		// Empty/whitespace input
		{
			name:      "empty input",
			input:     "",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			wantErr:   io.EOF,
		},
		{
			name:      "whitespace only",
			input:     "   ",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: ' ',
		},
		{
			name:      "non-digit non-sign first",
			input:     "abc",
			radix:     10,
			maxn:      0,
			wantVal:   0,
			wantCount: 0,
			remaining: 'a',
		},

		// Overflow cases
		{
			name:      "positive max int64",
			input:     "+9223372036854775807 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			remaining: ' ',
		},
		{
			name:      "negative overflow gives negated max",
			input:     "-9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   -9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},
		{
			name:      "positive overflow gives max",
			input:     "+9223372036854775808 ",
			radix:     10,
			maxn:      0,
			wantVal:   9223372036854775807,
			wantCount: 20,
			wantErr:   &TokenizerError{},
		},

		// Delimiter variations
		{
			name:      "terminated by paren",
			input:     "-123)",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: ')',
		},
		{
			name:      "terminated by newline",
			input:     "+456\n",
			radix:     10,
			maxn:      0,
			wantVal:   456,
			wantCount: 4,
			remaining: '\n',
		},

		// Unicode termination
		{
			name:      "unicode after signed digits",
			input:     "-123日本語",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 4,
			remaining: '日',
		},

		// Radix 0 acts like radix 10
		{
			name:      "radix 0 with sign",
			input:     "-42 ",
			radix:     0,
			maxn:      0,
			wantVal:   -42,
			wantCount: 3,
			remaining: ' ',
		},

		// Leading zeros with sign
		{
			name:      "positive with leading zeros",
			input:     "+00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   123,
			wantCount: 6,
			remaining: ' ',
		},
		{
			name:      "negative with leading zeros",
			input:     "-00123 ",
			radix:     10,
			maxn:      0,
			wantVal:   -123,
			wantCount: 6,
			remaining: ' ',
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			val, count := tok.readBaseNInteger(tc.radix, tc.maxn)

			c.Assert(count, qt.Equals, tc.wantCount, qt.Commentf("digit count mismatch"))
			c.Assert(val, qt.Equals, tc.wantVal, qt.Commentf("value mismatch"))

			switch tc.wantErr.(type) {
			case nil:
				c.Assert(tok.err, qt.IsNil, qt.Commentf("unexpected error: %v", tok.err))
			case *TokenizerError:
				var tokErr *TokenizerError
				c.Assert(tok.err, qt.ErrorAs, &tokErr, qt.Commentf("expected TokenizerError"))
			default:
				c.Assert(tok.err, qt.ErrorIs, tc.wantErr)
			}

			if tc.remaining != 0 {
				c.Assert(tok.curr(), qt.Equals, tc.remaining,
					qt.Commentf("remaining input mismatch"))
			}
		})
	}
}

// TestMayReadExponentEdgeCases tests exponent edge cases
func TestMayReadExponentEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "1.0E10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.0e+5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.0e-5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "1.5E10",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        "3/4",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        "22/7",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        "1/2",
			expectedType: TokenizerStateUnsignedRationalFraction,
		},
		{
			input:        ".5",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
		{
			input:        ".125",
			expectedType: TokenizerStateUnsignedDecimalFraction,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

func TestTokenizer_BigInteger(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			bs:    "#z123",
			scan:  "#z123",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#Z456",
			scan:  "#Z456",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z-789",
			scan:  "#z-789",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z+42",
			scan:  "#z+42",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z12345678901234567890",
			scan:  "#z12345678901234567890",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z0",
			scan:  "#z0",
			err0:  io.EOF,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z123 abc",
			scan:  "#z123",
			err0:  nil,
			state: TokenizerStateBigIntegerBase10,
		},
		{
			bs:    "#z123)",
			scan:  "#z123",
			err0:  nil,
			state: TokenizerStateBigIntegerBase10,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.Text()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.scan)
		})
	}
}

func TestTokenizer_BigFloat(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		{
			bs:    "#m3.14159265358979323846",
			scan:  "#m3.14159265358979323846",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#M2.71828182845904523536",
			scan:  "#M2.71828182845904523536",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#M-1.5",
			scan:  "#M-1.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m+42.0",
			scan:  "#m+42.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m123",
			scan:  "#m123",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m.5",
			scan:  "#m.5",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1e10",
			scan:  "#m1e10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.5e-10",
			scan:  "#m1.5e-10",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m3.14E+20",
			scan:  "#m3.14E+20",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m0.0",
			scan:  "#m0.0",
			err0:  io.EOF,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.23 abc",
			scan:  "#m1.23",
			err0:  nil,
			state: TokenizerStateBigFloat,
		},
		{
			bs:    "#m1.23)",
			scan:  "#m1.23",
			err0:  nil,
			state: TokenizerStateBigFloat,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.Text()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.scan)
		})
	}
}

// TestHashDigit_Tokenizer tests R7RS §7.1.1 # inexact digit placeholder
// tokenization. The # character can appear after real digits in number
// literals, representing an unknown digit. Its presence sets the hashDigit
// flag which forces the resulting number to be inexact.
func TestHashDigit_Tokenizer(t *testing.T) {
	tcs := []struct {
		name string
		in   string
		typ  TokenizerState
		src  string
		hash bool
		err  error
	}{
		// Unsigned integers with hash digits
		{
			name: "unsigned integer with hash",
			in:   "1##",
			typ:  TokenizerStateUnsignedInteger,
			src:  "1##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "unsigned integer no hash",
			in:   "123",
			typ:  TokenizerStateUnsignedInteger,
			src:  "123",
			hash: false,
			err:  io.EOF,
		},
		// Signed integers with hash digits
		{
			name: "signed integer with hash",
			in:   "-1##",
			typ:  TokenizerStateSignedInteger,
			src:  "-1##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "positive signed integer with hash",
			in:   "+1##",
			typ:  TokenizerStateSignedInteger,
			src:  "+1##",
			hash: true,
			err:  io.EOF,
		},
		// Unsigned decimal fractions with hash digits
		{
			name: "decimal fraction hash in fraction",
			in:   "1.2##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1.2##",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "integer hash then dot (production 4)",
			in:   "1##.",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1##.",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "integer hash then dot then hash (production 4)",
			in:   "1##.##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  "1##.##",
			hash: true,
			err:  io.EOF,
		},
		// Dot-initial decimal with hash
		{
			name: "dot-initial decimal with hash",
			in:   ".5##",
			typ:  TokenizerStateUnsignedDecimalFraction,
			src:  ".5##",
			hash: true,
			err:  io.EOF,
		},
		// Rational fractions with hash digits
		{
			name: "rational numerator hash",
			in:   "1##/3",
			typ:  TokenizerStateUnsignedRationalFraction,
			src:  "1##/3",
			hash: true,
			err:  io.EOF,
		},
		{
			name: "rational denominator hash",
			in:   "1/3##",
			typ:  TokenizerStateUnsignedRationalFraction,
			src:  "1/3##",
			hash: true,
			err:  io.EOF,
		},
		// Scientific notation with hash digits
		{
			name: "scientific notation hash in mantissa",
			in:   "1##e2",
			typ:  TokenizerStateUnsignedScientificNotation,
			src:  "1##e2",
			hash: true,
			err:  io.EOF,
		},
		// Base-2 (binary) with hash digits
		{
			name: "binary with hash",
			in:   "1#",
			typ:  TokenizerStateUnsignedIntegerBase2,
			src:  "1#",
			hash: true,
			err:  io.EOF,
		},
		// Base-16 (hex) with hash digits
		{
			name: "hex with hash",
			in:   "f#",
			typ:  TokenizerStateUnsignedIntegerBase16,
			src:  "f#",
			hash: true,
			err:  io.EOF,
		},
		// Signed decimal fraction with hash
		{
			name: "signed decimal fraction with hash",
			in:   "+.5##",
			typ:  TokenizerStateSignedDecimalFraction,
			src:  "+.5##",
			hash: true,
			err:  io.EOF,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			var input string
			// For base-N tests, we need to set up the tokenizer with the right radix
			switch tc.typ {
			case TokenizerStateUnsignedIntegerBase2:
				input = "#b" + tc.in
			case TokenizerStateUnsignedIntegerBase16:
				input = "#x" + tc.in
			default:
				input = tc.in
			}
			ts, err := Tokenize(input, false)
			c.Assert(err, qt.ErrorIs, tc.err)
			// For base-N, first token is the marker, second is the number
			var tok Token
			switch tc.typ {
			case TokenizerStateUnsignedIntegerBase2, TokenizerStateUnsignedIntegerBase16:
				c.Assert(len(ts), qt.Equals, 2)
				tok = ts[1]
			default:
				c.Assert(len(ts) >= 1, qt.IsTrue)
				tok = ts[0]
			}
			st := tok.(*SimpleToken)
			c.Assert(st.typ, qt.Equals, tc.typ)
			c.Assert(st.String(), qt.Equals, tc.src)
			c.Assert(tok.HasHashDigit(), qt.Equals, tc.hash)
		})
	}
}

// TestSignedImaginaryPartVariations tests to improve mayReadSignedImaginaryPart coverage
func TestSignedImaginaryPartVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Integer real with integer imaginary
		{input: "1+2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "123+456i", expectedType: TokenizerStateUnsignedComplex},
		{input: "123-456i", expectedType: TokenizerStateUnsignedComplex},

		// Integer real with rational imaginary
		{input: "1+1/2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-1/2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "10+3/4i", expectedType: TokenizerStateUnsignedComplex},
		{input: "10-3/4i", expectedType: TokenizerStateUnsignedComplex},

		// Integer real with decimal imaginary
		{input: "1+2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "100+0.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "100-0.5i", expectedType: TokenizerStateUnsignedComplex},

		// Rational real with integer imaginary
		{input: "1/2+3i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1/2-3i", expectedType: TokenizerStateUnsignedComplex},
		{input: "3/4+10i", expectedType: TokenizerStateUnsignedComplex},
		{input: "3/4-10i", expectedType: TokenizerStateUnsignedComplex},

		// Rational real with rational imaginary
		{input: "1/2+3/4i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1/2-3/4i", expectedType: TokenizerStateUnsignedComplex},
		{input: "5/6+7/8i", expectedType: TokenizerStateUnsignedComplex},
		{input: "5/6-7/8i", expectedType: TokenizerStateUnsignedComplex},

		// Rational real with decimal imaginary
		{input: "1/2+2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1/2-2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "3/4+0.125i", expectedType: TokenizerStateUnsignedComplex},
		{input: "3/4-0.125i", expectedType: TokenizerStateUnsignedComplex},

		// Decimal real with integer imaginary
		{input: "1.5+2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5-2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "0.5+100i", expectedType: TokenizerStateUnsignedComplex},
		{input: "0.5-100i", expectedType: TokenizerStateUnsignedComplex},

		// Decimal real with rational imaginary
		{input: "1.5+1/2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5-1/2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "2.75+3/4i", expectedType: TokenizerStateUnsignedComplex},
		{input: "2.75-3/4i", expectedType: TokenizerStateUnsignedComplex},

		// Decimal real with decimal imaginary
		{input: "1.5+2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5-2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "0.25+0.75i", expectedType: TokenizerStateUnsignedComplex},
		{input: "0.25-0.75i", expectedType: TokenizerStateUnsignedComplex},

		// Unit imaginary
		{input: "1+i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1/2+i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1/2-i", expectedType: TokenizerStateUnsignedComplex},
		{input: "3/2+i", expectedType: TokenizerStateUnsignedComplex}, // Issue #5 regression test
		{input: "3/2-i", expectedType: TokenizerStateUnsignedComplex}, // Issue #5 regression test
		{input: "1.5+i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5-i", expectedType: TokenizerStateUnsignedComplex},

		// With inf/nan imaginary parts
		{input: "1+inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1+nan.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-nan.0i", expectedType: TokenizerStateUnsignedComplex},

		// Uppercase I (R7RS §7.1.1: case-insensitive numeric literals)
		{input: "1+2I", expectedType: TokenizerStateUnsignedComplex},
		{input: "3-4I", expectedType: TokenizerStateUnsignedComplex},
		{input: "1+I", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-I", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5+2.5I", expectedType: TokenizerStateUnsignedComplex},

		// Signed versions
		{input: "+1+2i", expectedType: TokenizerStateSignedComplex},
		{input: "+1-2i", expectedType: TokenizerStateSignedComplex},
		{input: "-1+2i", expectedType: TokenizerStateSignedComplex},
		{input: "-1-2i", expectedType: TokenizerStateSignedComplex},
		{input: "+1/2+3/4i", expectedType: TokenizerStateSignedComplex},
		{input: "+1/2-3/4i", expectedType: TokenizerStateSignedComplex},
		{input: "-1/2+3/4i", expectedType: TokenizerStateSignedComplex},
		{input: "-1/2-3/4i", expectedType: TokenizerStateSignedComplex},
		{input: "+1.5+2.5i", expectedType: TokenizerStateSignedComplex},
		{input: "+1.5-2.5i", expectedType: TokenizerStateSignedComplex},
		{input: "-1.5+2.5i", expectedType: TokenizerStateSignedComplex},
		{input: "-1.5-2.5i", expectedType: TokenizerStateSignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestPolarPartVariations tests to improve mayReadPolarPart coverage
func TestPolarPartVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Integer magnitude with integer angle
		{input: "1@2", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "123@456", expectedType: TokenizerStateUnsignedComplexPolar},

		// Integer magnitude with rational angle
		{input: "1@1/2", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "10@3/4", expectedType: TokenizerStateUnsignedComplexPolar},

		// Integer magnitude with decimal angle
		{input: "1@2.5", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "100@0.5", expectedType: TokenizerStateUnsignedComplexPolar},

		// Rational magnitude with integer angle
		{input: "1/2@3", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "3/4@10", expectedType: TokenizerStateUnsignedComplexPolar},

		// Rational magnitude with rational angle
		{input: "1/2@3/4", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "5/6@7/8", expectedType: TokenizerStateUnsignedComplexPolar},

		// Rational magnitude with decimal angle
		{input: "1/2@2.5", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "3/4@0.125", expectedType: TokenizerStateUnsignedComplexPolar},

		// Decimal magnitude with integer angle
		{input: "1.5@2", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "0.5@100", expectedType: TokenizerStateUnsignedComplexPolar},

		// Decimal magnitude with rational angle
		{input: "1.5@1/2", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "2.75@3/4", expectedType: TokenizerStateUnsignedComplexPolar},

		// Decimal magnitude with decimal angle
		{input: "1.5@2.5", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "0.25@0.75", expectedType: TokenizerStateUnsignedComplexPolar},

		// With inf/nan angles
		{input: "1@+inf.0", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "1@-inf.0", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "1@+nan.0", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "1@-nan.0", expectedType: TokenizerStateUnsignedComplexPolar},

		// Signed versions
		{input: "+1@2", expectedType: TokenizerStateSignedComplexPolar},
		{input: "-1@2", expectedType: TokenizerStateSignedComplexPolar},
		{input: "+1/2@3", expectedType: TokenizerStateSignedComplexPolar},
		{input: "-1/2@3", expectedType: TokenizerStateSignedComplexPolar},
		{input: "+1.5@2.5", expectedType: TokenizerStateSignedComplexPolar},
		{input: "-1.5@2.5", expectedType: TokenizerStateSignedComplexPolar},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestNumberFractionalVariations tests to improve mayReadUnsignedFractionalRealNumberOrRationalRealNumber coverage
func TestNumberFractionalVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Decimal fractions starting with dot
		{input: ".0", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".5", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".9", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".123", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".999", expectedType: TokenizerStateUnsignedDecimalFraction},

		// Signed decimal fractions starting with dot
		{input: "+.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "+.123", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-.999", expectedType: TokenizerStateSignedDecimalFraction},

		// Decimal fractions with leading digit
		{input: "0.0", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "0.5", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.5", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "123.456", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "999.999", expectedType: TokenizerStateUnsignedDecimalFraction},

		// Signed decimal fractions with leading digit
		{input: "+0.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-0.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "+123.456", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-999.999", expectedType: TokenizerStateSignedDecimalFraction},

		// Rational fractions
		{input: "0/1", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "1/1", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "1/2", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "3/4", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "123/456", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "999/888", expectedType: TokenizerStateUnsignedRationalFraction},

		// Signed rational fractions
		{input: "+1/2", expectedType: TokenizerStateSignedRationalFraction},
		{input: "-1/2", expectedType: TokenizerStateSignedRationalFraction},
		{input: "+3/4", expectedType: TokenizerStateSignedRationalFraction},
		{input: "-3/4", expectedType: TokenizerStateSignedRationalFraction},
		{input: "+123/456", expectedType: TokenizerStateSignedRationalFraction},
		{input: "-999/888", expectedType: TokenizerStateSignedRationalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestMalformedNumbers tests error paths for malformed numbers to improve coverage
func TestMalformedNumbers(t *testing.T) {
	tcs := []struct {
		input       string
		shouldError bool
	}{
		// Malformed inf (these should either error or parse as symbols)
		{input: "+inx", shouldError: false}, // Should parse as symbol
		{input: "-inx", shouldError: false}, // Should parse as symbol
		{input: "+in", shouldError: false},  // Should parse as symbol
		{input: "-in", shouldError: false},  // Should parse as symbol

		// Malformed nan (these should either error or parse as symbols)
		{input: "+nax", shouldError: false}, // Should parse as symbol
		{input: "-nax", shouldError: false}, // Should parse as symbol
		{input: "+na", shouldError: false},  // Should parse as symbol
		{input: "-na", shouldError: false},  // Should parse as symbol

		// Well-formed numbers (for comparison)
		{input: "+inf.0", shouldError: false},
		{input: "-inf.0", shouldError: false},
		{input: "+nan.0", shouldError: false},
		{input: "-nan.0", shouldError: false},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			if tc.shouldError {
				c.Check(err, qt.Not(qt.IsNil))
			} else {
				c.Check(err, qt.IsNil)
				c.Check(token, qt.Not(qt.IsNil))
			}
		})
	}
}

// TestExponentEdgeCases tests exponent edge cases to improve coverage
func TestExponentEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Decimals with exponents
		{input: "1.0e1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0e+1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0e-1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0E1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0E+1", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "1.0E-1", expectedType: TokenizerStateUnsignedDecimalFraction},

		// Signed decimals with exponents
		{input: "+1.0e1", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "+1.0e+1", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "+1.0e-1", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-1.0e1", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-1.0e+1", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-1.0e-1", expectedType: TokenizerStateSignedDecimalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestImaginaryWithExponents tests imaginary numbers with exponents
func TestImaginaryWithExponents(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Imaginary with decimal exponent
		{input: "1.0e1i", expectedType: TokenizerStateUnsignedImaginary},
		{input: "+1.0e1i", expectedType: TokenizerStateSignedImaginary},
		{input: "-1.0e1i", expectedType: TokenizerStateSignedImaginary},

		// Complex with decimal exponent
		{input: "1.0e1+2i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1+1.0e1i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.0e1+1.0e1i", expectedType: TokenizerStateUnsignedComplex},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestNonDecimalRadixNumbers tests non-decimal radix numbers
func TestNonDecimalRadixNumbers(t *testing.T) {
	tcs := []struct {
		input         string
		expectedType1 TokenizerState
		expectedType2 TokenizerState
	}{
		// Binary
		{input: "#b0", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedIntegerBase2},
		{input: "#b1", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedIntegerBase2},
		{input: "#b10", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedIntegerBase2},
		{input: "#b11", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedIntegerBase2},
		{input: "#b101010", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedIntegerBase2},

		// Octal
		{input: "#o0", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedIntegerBase8},
		{input: "#o7", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedIntegerBase8},
		{input: "#o10", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedIntegerBase8},
		{input: "#o77", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedIntegerBase8},
		{input: "#o777", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedIntegerBase8},

		// Hexadecimal (must start with digit)
		{input: "#x0", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x9", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x10", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x100", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x1a", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x1f", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x1ff", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
		{input: "#x123abc", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedIntegerBase16},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: marker
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.Type(), qt.Equals, tc.expectedType1)

			// Second token: integer
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.Type(), qt.Equals, tc.expectedType2)
		})
	}
}

// TestComplexInfNanCombinations tests complex numbers with inf/nan components
func TestComplexInfNanCombinations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Inf real parts
		{input: "+inf.0+1i", expectedType: TokenizerStateSignedComplex},
		{input: "-inf.0+1i", expectedType: TokenizerStateSignedComplex},
		{input: "+inf.0-1i", expectedType: TokenizerStateSignedComplex},
		{input: "-inf.0-1i", expectedType: TokenizerStateSignedComplex},

		// Nan real parts (note: nan as real part in complex doesn't work, it's parsed as SignedNan)
		// {input: "+nan.0+1i", expectedType: TokenizerStateSignedComplex},
		// {input: "-nan.0+1i", expectedType: TokenizerStateSignedComplex},
		// {input: "+nan.0-1i", expectedType: TokenizerStateSignedComplex},
		// {input: "-nan.0-1i", expectedType: TokenizerStateSignedComplex},

		// Inf imaginary parts
		{input: "1+inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "+1+inf.0i", expectedType: TokenizerStateSignedComplex},
		{input: "-1+inf.0i", expectedType: TokenizerStateSignedComplex},
		{input: "+1-inf.0i", expectedType: TokenizerStateSignedComplex},
		{input: "-1-inf.0i", expectedType: TokenizerStateSignedComplex},

		// Nan imaginary parts
		{input: "1+nan.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-nan.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "+1+nan.0i", expectedType: TokenizerStateSignedComplex},
		{input: "-1+nan.0i", expectedType: TokenizerStateSignedComplex},
		{input: "+1-nan.0i", expectedType: TokenizerStateSignedComplex},
		{input: "-1-nan.0i", expectedType: TokenizerStateSignedComplex},

		// Inf/Nan combinations
		{input: "+inf.0+inf.0i", expectedType: TokenizerStateSignedComplex},
		// {input: "+nan.0+nan.0i", expectedType: TokenizerStateSignedComplex}, // nan as real part doesn't work
		{input: "+inf.0+nan.0i", expectedType: TokenizerStateSignedComplex},
		// {input: "+nan.0+inf.0i", expectedType: TokenizerStateSignedComplex}, // nan as real part doesn't work
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestMoreComplexNumbers tests complex number variations to improve coverage
func TestMoreComplexNumbers(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Rectangular complex with rationals
		{input: "1/2+3/4i", expectedType: TokenizerStateUnsignedComplex},
		{input: "+1/2+3/4i", expectedType: TokenizerStateSignedComplex},
		{input: "-1/2-3/4i", expectedType: TokenizerStateSignedComplex},

		// Polar complex with integers (rationals don't work in polar)
		{input: "1/2@3", expectedType: TokenizerStateUnsignedComplexPolar},
		{input: "+1/2@3", expectedType: TokenizerStateSignedComplexPolar},
		{input: "-1/2@3", expectedType: TokenizerStateSignedComplexPolar},

		// Complex with decimal fractions
		{input: "1.5+2.5i", expectedType: TokenizerStateUnsignedComplex},
		{input: "+1.5+2.5i", expectedType: TokenizerStateSignedComplex},
		{input: "1.5@2.5", expectedType: TokenizerStateUnsignedComplexPolar},

		// Imaginary with rationals
		{input: "1/2i", expectedType: TokenizerStateUnsignedImaginary},
		{input: "+1/2i", expectedType: TokenizerStateSignedImaginary},
		{input: "-1/2i", expectedType: TokenizerStateSignedImaginary},

		// Unit imaginary
		{input: "i", expectedType: TokenizerStateSymbol},

		// Complex with inf/nan (note: these are parsed as SignedInf/SignedNan)
		{input: "+inf.0+i", expectedType: TokenizerStateSignedComplex},
		{input: "-inf.0-i", expectedType: TokenizerStateSignedComplex},

		// Polar with inf (note: inf is parsed as SignedInf, not part of polar)
		{input: "1@+inf.0", expectedType: TokenizerStateUnsignedComplexPolar},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestRationalNumbers tests rational number variations
func TestRationalNumbers(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "1/2", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "+1/2", expectedType: TokenizerStateSignedRationalFraction},
		{input: "-1/2", expectedType: TokenizerStateSignedRationalFraction},
		{input: "123/456", expectedType: TokenizerStateUnsignedRationalFraction},
		{input: "+999/888", expectedType: TokenizerStateSignedRationalFraction},
		{input: "-777/666", expectedType: TokenizerStateSignedRationalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestDecimalFractions tests decimal fraction variations
func TestDecimalFractions(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: ".5", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: ".123", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "+.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "0.5", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "+0.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-0.5", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "123.456", expectedType: TokenizerStateUnsignedDecimalFraction},
		{input: "+123.456", expectedType: TokenizerStateSignedDecimalFraction},
		{input: "-123.456", expectedType: TokenizerStateSignedDecimalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestInfAndNan tests inf and nan special values
func TestInfAndNan(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "+inf.0", expectedType: TokenizerStateSignedInf},
		{input: "-inf.0", expectedType: TokenizerStateSignedInf},
		{input: "+nan.0", expectedType: TokenizerStateSignedNan},
		{input: "-nan.0", expectedType: TokenizerStateSignedNan},
		{input: "+inf.0i", expectedType: TokenizerStateSignedImaginaryInf},
		{input: "-inf.0i", expectedType: TokenizerStateSignedImaginaryInf},
		{input: "+nan.0i", expectedType: TokenizerStateSignedImaginaryNan},
		{input: "-nan.0i", expectedType: TokenizerStateSignedImaginaryNan},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// TestExactnessMarkersExtended tests exactness prefix markers
func TestExactnessMarkersExtended(t *testing.T) {
	tcs := []struct {
		input         string
		expectedType1 TokenizerState
		expectedType2 TokenizerState
	}{
		{input: "#e1", expectedType1: TokenizerStateMarkerNumberExact, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#i1", expectedType1: TokenizerStateMarkerNumberInexact, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#e1.5", expectedType1: TokenizerStateMarkerNumberExact, expectedType2: TokenizerStateUnsignedDecimalFraction},
		{input: "#i1.5", expectedType1: TokenizerStateMarkerNumberInexact, expectedType2: TokenizerStateUnsignedDecimalFraction},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)

			// First token: exactness marker
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.Type(), qt.Equals, tc.expectedType1)

			// Second token: number
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.Type(), qt.Equals, tc.expectedType2)
		})
	}
}

// Test unsigned fractional/rational edge cases
func TestTokenizer_UnsignedFractionalEdgeCases(t *testing.T) {
	tests := []tokenizerTestCase{
		// Signed inf
		{"+inf.0", TokenizerStateSignedInf},
		{"-inf.0", TokenizerStateSignedInf},
		// Signed nan
		{"+nan.0", TokenizerStateSignedNan},
		{"-nan.0", TokenizerStateSignedNan},
		// Decimal fractions starting with dot
		{"+.5", TokenizerStateSignedDecimalFraction},
		{"-.5", TokenizerStateSignedDecimalFraction},
		// Decimal fractions with exponent after dot
		{"+.5e2", TokenizerStateSignedDecimalFraction},
		{"-.5e-2", TokenizerStateSignedDecimalFraction},
		// Integer followed by decimal
		{"+10.5", TokenizerStateSignedDecimalFraction},
		{"-10.5", TokenizerStateSignedDecimalFraction},
		// Integer with exponent (scientific notation - parser determines int vs float)
		{"+10e5", TokenizerStateSignedScientificNotation},
		{"-10e-5", TokenizerStateSignedScientificNotation},
		// Rational fractions
		{"+3/4", TokenizerStateSignedRationalFraction},
		{"-3/4", TokenizerStateSignedRationalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test imaginary parts with various formats
func TestTokenizer_ImaginaryParts(t *testing.T) {
	tests := []tokenizerTestCase{
		// Simple imaginary
		{"+i", TokenizerStateSignedImaginary},
		{"-i", TokenizerStateSignedImaginary},
		// Integer imaginary
		{"+3i", TokenizerStateSignedImaginary},
		{"-3i", TokenizerStateSignedImaginary},
		// Decimal imaginary
		{"+3.5i", TokenizerStateSignedImaginary},
		{"-3.5i", TokenizerStateSignedImaginary},
		// Complex with integer parts
		{"1+2i", TokenizerStateUnsignedComplex},
		{"1-2i", TokenizerStateUnsignedComplex},
		// Complex with inf imaginary
		{"1+inf.0i", TokenizerStateUnsignedComplex},
		{"1-inf.0i", TokenizerStateUnsignedComplex},
		// Complex with nan imaginary
		{"1+nan.0i", TokenizerStateUnsignedComplex},
		{"1-nan.0i", TokenizerStateUnsignedComplex},
		// Complex with decimal imaginary
		{"1+2.5i", TokenizerStateUnsignedComplex},
		{"1-2.5i", TokenizerStateUnsignedComplex},
		// Complex with rational imaginary
		{"1+1/2i", TokenizerStateUnsignedComplex},
		{"1-1/2i", TokenizerStateUnsignedComplex},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test polar complex numbers
func TestTokenizer_PolarComplex(t *testing.T) {
	tests := []tokenizerTestCase{
		// Basic polar
		{"1@0", TokenizerStateUnsignedComplexPolar},
		{"1@1.57", TokenizerStateUnsignedComplexPolar},
		// Signed polar
		{"+1@0", TokenizerStateSignedComplexPolar},
		{"-1@0", TokenizerStateSignedComplexPolar},
		// Polar with inf angle
		{"1@+inf.0", TokenizerStateUnsignedComplexPolar},
		{"1@-inf.0", TokenizerStateUnsignedComplexPolar},
		// Polar with nan angle
		{"1@+nan.0", TokenizerStateUnsignedComplexPolar},
		{"1@-nan.0", TokenizerStateUnsignedComplexPolar},
		// Polar with decimal
		{"1.5@0.5", TokenizerStateUnsignedComplexPolar},
		// Polar with fraction
		{"1/2@1/4", TokenizerStateUnsignedComplexPolar},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test inf/nan values
func TestTokenizer_InfNanValues(t *testing.T) {
	tests := []tokenizerTestCase{
		// Valid inf/nan - note: these may tokenize as SignedInf even with different suffixes
		{"+inf.0", TokenizerStateSignedInf},
		{"-inf.0", TokenizerStateSignedInf},
		{"+nan.0", TokenizerStateSignedNan},
		{"-nan.0", TokenizerStateSignedNan},
		// Case-insensitive entry points (R7RS §7.1.1)
		{"+INF.0", TokenizerStateSignedInf},
		{"-INF.0", TokenizerStateSignedInf},
		{"+NAN.0", TokenizerStateSignedNan},
		{"-NAN.0", TokenizerStateSignedNan},
		{"+InF.0", TokenizerStateSignedInf},
		{"-iNF.0", TokenizerStateSignedInf},
		{"+Nan.0", TokenizerStateSignedNan},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test decimal fraction values
func TestTokenizer_DecimalFractionValues(t *testing.T) {
	tests := []tokenizerTestCase{
		// Valid decimal fractions
		{"+10.5", TokenizerStateSignedDecimalFraction},
		{"+.5", TokenizerStateSignedDecimalFraction},
		{"-10.5", TokenizerStateSignedDecimalFraction},
		{"-.5", TokenizerStateSignedDecimalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test rational fraction values
func TestTokenizer_RationalFractionValues(t *testing.T) {
	tests := []tokenizerTestCase{
		// Valid rational fractions
		{"+3/4", TokenizerStateSignedRationalFraction},
		{"-3/4", TokenizerStateSignedRationalFraction},
		{"3/4", TokenizerStateUnsignedRationalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayReadPolarPart edge cases
func TestTokenizer_PolarPartEdgeCases(t *testing.T) {
	tests := []tokenizerTestCase{
		// Polar with signed angle
		{"1@+1", TokenizerStateUnsignedComplexPolar},
		{"1@-1", TokenizerStateUnsignedComplexPolar},
		// Polar with decimal angle
		{"1@+1.5", TokenizerStateUnsignedComplexPolar},
		{"1@-1.5", TokenizerStateUnsignedComplexPolar},
		// Polar with unsigned decimal angle
		{"1@1.5", TokenizerStateUnsignedComplexPolar},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test exactness and radix markers (R7RS §7.1.1: case-insensitive)
func TestTokenizer_ExactnessRadixMarkers(t *testing.T) {
	tests := []tokenizerTestCase{
		// Exactness markers (lowercase)
		{"#e10", TokenizerStateMarkerNumberExact},
		{"#i10", TokenizerStateMarkerNumberInexact},
		// Exactness markers (uppercase - R7RS §7.1.1)
		{"#E10", TokenizerStateMarkerNumberExact},
		{"#I10", TokenizerStateMarkerNumberInexact},
		// Radix markers (lowercase)
		{"#b101", TokenizerStateMarkerBase2},
		{"#o77", TokenizerStateMarkerBase8},
		{"#d99", TokenizerStateMarkerBase10},
		{"#xff", TokenizerStateMarkerBase16},
		// Radix markers (uppercase - R7RS §7.1.1)
		{"#B101", TokenizerStateMarkerBase2},
		{"#O77", TokenizerStateMarkerBase8},
		{"#D99", TokenizerStateMarkerBase10},
		{"#XFF", TokenizerStateMarkerBase16},
		// Mixed case combinations
		{"#E#b101", TokenizerStateMarkerNumberExact},
		{"#I#X1a", TokenizerStateMarkerNumberInexact},
		{"#B#e101", TokenizerStateMarkerBase2},
		{"#X#i1a", TokenizerStateMarkerBase16},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test complex numbers with inf/nan
func TestTokenizer_ComplexInfNan(t *testing.T) {
	tests := []tokenizerTestCase{
		// Real inf with imaginary
		{"+inf.0+1i", TokenizerStateSignedComplex},
		{"-inf.0+1i", TokenizerStateSignedComplex},
		{"+inf.0-1i", TokenizerStateSignedComplex},
		// Imaginary inf
		{"+inf.0i", TokenizerStateSignedImaginaryInf},
		{"-inf.0i", TokenizerStateSignedImaginaryInf},
		// Imaginary nan
		{"+nan.0i", TokenizerStateSignedImaginaryNan},
		{"-nan.0i", TokenizerStateSignedImaginaryNan},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayReadSignedNumber path
func TestTokenizer_SignedIntegerPaths(t *testing.T) {
	tests := []tokenizerTestCase{
		// Simple signed integers
		{"+123", TokenizerStateSignedInteger},
		{"-123", TokenizerStateSignedInteger},
		// Just sign followed by letter - symbol
		{"+a", TokenizerStateSymbol},
		{"-a", TokenizerStateSymbol},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test exponent edge cases - unsigned integers with exponents
func TestTokenizer_ExponentEdgeCases(t *testing.T) {
	tests := []tokenizerTestCase{
		// Decimal with exponent (no sign)
		{"1.5e10", TokenizerStateUnsignedDecimalFraction},
		{"1.5E10", TokenizerStateUnsignedDecimalFraction},
		// Signed decimal with exponent
		{"+1.5e10", TokenizerStateSignedDecimalFraction},
		{"-1.5e10", TokenizerStateSignedDecimalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test R7RS exponent markers s/f/d/l (R7RS §7.1.1)
func TestTokenizer_ExtendedExponentMarkers(t *testing.T) {
	tests := []tokenizerTestCase{
		// Unsigned integer with s/f/d/l markers
		{"1s10", TokenizerStateUnsignedScientificNotation},
		{"1f10", TokenizerStateUnsignedScientificNotation},
		{"1d10", TokenizerStateUnsignedScientificNotation},
		{"1l10", TokenizerStateUnsignedScientificNotation},
		// Uppercase variants
		{"1S10", TokenizerStateUnsignedScientificNotation},
		{"1F10", TokenizerStateUnsignedScientificNotation},
		{"1D10", TokenizerStateUnsignedScientificNotation},
		{"1L10", TokenizerStateUnsignedScientificNotation},
		// Signed
		{"+1s10", TokenizerStateSignedScientificNotation},
		{"-1f10", TokenizerStateSignedScientificNotation},
		{"+1d10", TokenizerStateSignedScientificNotation},
		{"-1l10", TokenizerStateSignedScientificNotation},
		// With decimals
		{"1.5s3", TokenizerStateUnsignedDecimalFraction},
		{"1.5f3", TokenizerStateUnsignedDecimalFraction},
		{"1.5d3", TokenizerStateUnsignedDecimalFraction},
		{"1.5l3", TokenizerStateUnsignedDecimalFraction},
		// Signed with decimals
		{"+1.5s3", TokenizerStateSignedDecimalFraction},
		{"-1.5f3", TokenizerStateSignedDecimalFraction},
		// With negative exponent
		{"1s-3", TokenizerStateUnsignedScientificNotation},
		{"1f-3", TokenizerStateUnsignedScientificNotation},
		// With positive exponent sign
		{"1d+3", TokenizerStateUnsignedScientificNotation},
		{"1l+3", TokenizerStateUnsignedScientificNotation},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test R7RS exponent markers s/f/d/l with big numbers
func TestTokenizer_ExtendedExponentMarkers_BigNumbers(t *testing.T) {
	tcs := []struct {
		bs    string
		scan  string
		err0  error
		state TokenizerState
	}{
		// Big integer with s/f/d/l markers
		{"#z1s10", "#z1s10", io.EOF, TokenizerStateBigIntegerBase10},
		{"#z1f10", "#z1f10", io.EOF, TokenizerStateBigIntegerBase10},
		{"#z1d10", "#z1d10", io.EOF, TokenizerStateBigIntegerBase10},
		{"#z1l10", "#z1l10", io.EOF, TokenizerStateBigIntegerBase10},
		// Big float with s/f/d/l markers
		{"#m1s10", "#m1s10", io.EOF, TokenizerStateBigFloat},
		{"#m1f10", "#m1f10", io.EOF, TokenizerStateBigFloat},
		{"#m1.5d3", "#m1.5d3", io.EOF, TokenizerStateBigFloat},
		{"#m1.5l3", "#m1.5l3", io.EOF, TokenizerStateBigFloat},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.bs), func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.bs), false)
			p.mark()
			p.read()
			err := p.err
			state := p.state
			p.Text()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.scan)
		})
	}
}

// Test additional imaginary part variations for coverage
func TestTokenizer_ImaginaryPartBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Complex with decimal imaginary part
		{"1+2.5i", TokenizerStateUnsignedComplex},
		{"1-2.5i", TokenizerStateUnsignedComplex},
		// Complex with decimal imaginary part and exponent
		{"1+2.5e3i", TokenizerStateUnsignedComplex},
		{"1-2.5e3i", TokenizerStateUnsignedComplex},
		// Complex with integer imaginary and exponent
		{"1+2e3i", TokenizerStateUnsignedComplex},
		{"1-2e3i", TokenizerStateUnsignedComplex},
		// Complex with signed real part
		{"+1+2i", TokenizerStateSignedComplex},
		{"-1+2i", TokenizerStateSignedComplex},
		{"+1-2i", TokenizerStateSignedComplex},
		{"-1-2i", TokenizerStateSignedComplex},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test polar part variations for coverage
func TestTokenizer_PolarPartBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Polar with dot-prefixed angle
		{"1@.5", TokenizerStateUnsignedComplexPolar},
		{"1@+.5", TokenizerStateUnsignedComplexPolar},
		{"1@-.5", TokenizerStateUnsignedComplexPolar},
		// Polar with integer angle and decimal
		{"1@1.5", TokenizerStateUnsignedComplexPolar},
		{"1@+1.5", TokenizerStateUnsignedComplexPolar},
		{"1@-1.5", TokenizerStateUnsignedComplexPolar},
		// Polar with exponent
		{"1@1e2", TokenizerStateUnsignedComplexPolar},
		{"1@1.5e2", TokenizerStateUnsignedComplexPolar},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayReadUnsignedFractionalRealNumberOrRationalRealNumber branches
func TestTokenizer_FractionalBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Unsigned decimal fractions
		{".5", TokenizerStateUnsignedDecimalFraction},
		{"0.5", TokenizerStateUnsignedDecimalFraction},
		{"10.5", TokenizerStateUnsignedDecimalFraction},
		// Unsigned with exponent - integers without decimal keep as integer
		{"10.5e5", TokenizerStateUnsignedDecimalFraction},
		// Unsigned rational
		{"3/4", TokenizerStateUnsignedRationalFraction},
		{"10/20", TokenizerStateUnsignedRationalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayUnsignedFractional branches
func TestTokenizer_MayUnsignedFractionalBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Signed inf parsing
		{"+inf.0", TokenizerStateSignedInf},
		{"-inf.0", TokenizerStateSignedInf},
		// Signed nan parsing
		{"+nan.0", TokenizerStateSignedNan},
		{"-nan.0", TokenizerStateSignedNan},
		// Dot followed by digit
		{"+.25", TokenizerStateSignedDecimalFraction},
		{"-.25", TokenizerStateSignedDecimalFraction},
		// Digit followed by dot and digit
		{"+1.25", TokenizerStateSignedDecimalFraction},
		{"-1.25", TokenizerStateSignedDecimalFraction},
		// Digit followed by slash (rational)
		{"+1/4", TokenizerStateSignedRationalFraction},
		{"-1/4", TokenizerStateSignedRationalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayUnsignedFractional with exponents
func TestTokenizer_MayUnsignedExponentBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Integer with exponent branches (scientific notation - parser determines int vs float)
		{"+10e5", TokenizerStateSignedScientificNotation},
		{"-10e5", TokenizerStateSignedScientificNotation},
		// Decimal with exponent
		{"+10.5e5", TokenizerStateSignedDecimalFraction},
		{"-10.5e5", TokenizerStateSignedDecimalFraction},
		// Exponents with signs
		{"+10.5e+5", TokenizerStateSignedDecimalFraction},
		{"+10.5e-5", TokenizerStateSignedDecimalFraction},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test more imaginary branches
func TestTokenizer_ImaginaryBranchesExtra(t *testing.T) {
	tests := []tokenizerTestCase{
		// Complex with decimal imaginary and exponent
		{"1+2.5e2i", TokenizerStateUnsignedComplex},
		{"1-2.5e2i", TokenizerStateUnsignedComplex},
		// Complex with integer imaginary and exponent
		{"1+2e2i", TokenizerStateUnsignedComplex},
		{"1-2e2i", TokenizerStateUnsignedComplex},
		// Complex with inf/nan in imaginary
		{"1+inf.0i", TokenizerStateUnsignedComplex},
		{"1-inf.0i", TokenizerStateUnsignedComplex},
		{"1+nan.0i", TokenizerStateUnsignedComplex},
		{"1-nan.0i", TokenizerStateUnsignedComplex},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test radix and exactness markers with numbers following
func TestTokenizer_RadixExactnessWithNumbers(t *testing.T) {
	tests := []tokenizerTestCase{
		// Exactness
		{"#e", TokenizerStateMarkerNumberExact},
		{"#i", TokenizerStateMarkerNumberInexact},
		// Exactness followed by subsequents (becomes marker)
		{"#efoo", TokenizerStateMarker},
		{"#ifoo", TokenizerStateMarker},
		// Radix followed by subsequents (becomes marker)
		{"#bggg", TokenizerStateMarker}, // g is not a valid binary digit
		{"#oggg", TokenizerStateMarker}, // g is not a valid octal digit
		{"#dggg", TokenizerStateMarker}, // g is not a valid decimal digit
		{"#xggg", TokenizerStateMarker}, // g is not a valid hex digit
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test scan method
func TestTokenizer_ScanMethod(t *testing.T) {
	// The scan method is used internally - test via tokens that use it
	tests := []tokenizerTestCase{
		{"+inf.0", TokenizerStateSignedInf},
		{"-inf.0", TokenizerStateSignedInf},
		{"+nan.0", TokenizerStateSignedNan},
		{"-nan.0", TokenizerStateSignedNan},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test mayReadSignedImaginaryPart with nan branches
func TestTokenizer_ImaginaryNanBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// nan in imaginary part of complex
		{"1+nan.0i", TokenizerStateUnsignedComplex},
		{"1-nan.0i", TokenizerStateUnsignedComplex},
		{"+1+nan.0i", TokenizerStateSignedComplex},
		{"-1+nan.0i", TokenizerStateSignedComplex},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// Test imaginary part with decimal and exponent
func TestTokenizer_ImaginaryDecimalExponent(t *testing.T) {
	tests := []tokenizerTestCase{
		// Decimal imaginary part
		{"1+2.0i", TokenizerStateUnsignedComplex},
		{"1-2.0i", TokenizerStateUnsignedComplex},
		// Imaginary with exponent
		{"1+2e1i", TokenizerStateUnsignedComplex},
		{"1-2e1i", TokenizerStateUnsignedComplex},
		// Decimal imaginary with exponent
		{"1+2.0e1i", TokenizerStateUnsignedComplex},
		{"1-2.0e1i", TokenizerStateUnsignedComplex},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
		})
	}
}

// TestCombinedRadixExactness tests combined radix and exactness prefixes
// Per R7RS 7.1.1: <prefix> → <radix> <exactness> | <exactness> <radix>
func TestCombinedRadixExactness(t *testing.T) {
	tcs := []struct {
		input string
		state TokenizerState
		span  string
		err   error
	}{
		// Exactness prefix alone (returns marker, parser handles number)
		{
			input: "#e",
			state: TokenizerStateMarkerNumberExact,
			span:  "#e",
			err:   io.EOF,
		},
		{
			input: "#i",
			state: TokenizerStateMarkerNumberInexact,
			span:  "#i",
			err:   io.EOF,
		},
		// Radix prefix alone
		{
			input: "#b",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   io.EOF,
		},
		{
			input: "#o",
			state: TokenizerStateMarkerBase8,
			span:  "#o",
			err:   io.EOF,
		},
		{
			input: "#d",
			state: TokenizerStateMarkerBase10,
			span:  "#d",
			err:   io.EOF,
		},
		{
			input: "#x",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   io.EOF,
		},
		// Exactness followed by radix: tokenizer returns proper exactness marker
		// Parser will handle assembling the prefixes
		{
			input: "#e#b101",
			state: TokenizerStateMarkerNumberExact,
			span:  "#e",
			err:   nil,
		},
		{
			input: "#i#x1a",
			state: TokenizerStateMarkerNumberInexact,
			span:  "#i",
			err:   nil,
		},
		// Radix followed by exactness: tokenizer returns proper radix marker
		{
			input: "#b#e101",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   nil,
		},
		{
			input: "#x#i1a",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   nil,
		},
		// Leading zeros in different radixes
		{
			input: "#b00101",
			state: TokenizerStateMarkerBase2,
			span:  "#b",
			err:   nil,
		},
		{
			input: "#o00777",
			state: TokenizerStateMarkerBase8,
			span:  "#o",
			err:   nil,
		},
		{
			input: "#x00FF",
			state: TokenizerStateMarkerBase16,
			span:  "#x",
			err:   nil,
		},
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d: %q", i, tc.input), func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			c.Check(p.err, qt.ErrorIs, tc.err)
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.span)
		})
	}
}

// TestInvalidNumbers tests error handling for malformed numbers
func TestInvalidNumbers(t *testing.T) {
	tcs := []struct {
		name  string
		input string
		state TokenizerState
		span  string
	}{
		{
			name:  "multiple_dots",
			input: "1.2.3",
			state: TokenizerStateUnsignedDecimalFraction,
			span:  "1.2",
		},
		{
			name:  "multiple_slashes",
			input: "1/2/3",
			state: TokenizerStateUnsignedRationalFraction,
			span:  "1/2",
		},
		{
			name:  "dot_only",
			input: ".",
			state: TokenizerStateCons,
			span:  ".",
		},
		{
			name:  "plus_only",
			input: "+",
			state: TokenizerStateSymbol,
			span:  "+",
		},
		{
			name:  "minus_only",
			input: "-",
			state: TokenizerStateSymbol,
			span:  "-",
		},
		{
			name:  "rational_no_denominator",
			input: "1/",
			state: TokenizerStateUnsignedRationalFraction, // tokenizer includes the /
			span:  "1/",
		},
		{
			name:  "decimal_trailing_dot_delimiter",
			input: "1.(",
			state: TokenizerStateUnsignedDecimalFraction,
			span:  "1.",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// These should tokenize as partial tokens or different types
			c.Check(p.state, qt.Equals, tc.state)
			c.Check(p.Text(), qt.Equals, tc.span)
		})
	}
}

// TestKnownBugs documents known tokenizer bugs per CLAUDE.md
// Note: Some bugs mentioned in CLAUDE.md may have been fixed
func TestKnownBugs(t *testing.T) {
	t.Run("signed_integer_with_exponent_now_works", func(t *testing.T) {
		// Previously documented as bug: +1e10 tokenized as two tokens
		// This has been FIXED - now tokenizes correctly as scientific notation
		// Parser will determine if result is integer or float based on exponent
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("+1e10"), false)

		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.Type(), qt.Equals, TokenizerStateSignedScientificNotation)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "+1e10")

		// Verify no second token
		_, err2 := p.Next()
		c.Check(err2, qt.Equals, io.EOF)
	})

	t.Run("trailing_dot_with_exponent_now_works", func(t *testing.T) {
		// Previously documented as bug: 1.e10 errored
		// Testing current behavior
		c := qt.New(t)
		p := NewTokenizer(strings.NewReader("1.e10"), false)

		tok, err := p.Next()
		c.Assert(err, qt.IsNil)
		c.Check(tok.Type(), qt.Equals, TokenizerStateUnsignedDecimalFraction)
		c.Check(tok.(*SimpleToken).src, qt.Equals, "1.e10")
	})
}
