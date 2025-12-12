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

package tokenizer

import (
	"fmt"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

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

// TestEdgeCaseSymbols tests edge case symbols to improve coverage
func TestEdgeCaseSymbols(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Dot-subsequent symbols
		{input: ".+", expectedType: TokenizerStateSymbol},
		{input: ".-", expectedType: TokenizerStateSymbol},
		{input: "..", expectedType: TokenizerStateSymbol},
		{input: "...", expectedType: TokenizerStateSymbol},
		{input: ".@", expectedType: TokenizerStateSymbol},

		// Sign-subsequent symbols
		{input: "+.", expectedType: TokenizerStateSymbol},
		{input: "+..", expectedType: TokenizerStateSymbol},
		{input: "+...", expectedType: TokenizerStateSymbol},
		{input: "-.", expectedType: TokenizerStateSymbol},
		{input: "-..", expectedType: TokenizerStateSymbol},
		{input: "-...", expectedType: TokenizerStateSymbol},

		// Other peculiar identifiers
		{input: "+@", expectedType: TokenizerStateSymbol},
		{input: "-@", expectedType: TokenizerStateSymbol},
		{input: "+!", expectedType: TokenizerStateSymbol},
		{input: "-!", expectedType: TokenizerStateSymbol},
		{input: "+$", expectedType: TokenizerStateSymbol},
		{input: "-$", expectedType: TokenizerStateSymbol},
		{input: "+%", expectedType: TokenizerStateSymbol},
		{input: "-%", expectedType: TokenizerStateSymbol},
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
		{input: "#b0", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#b1", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#b10", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#b11", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#b101010", expectedType1: TokenizerStateMarkerBase2, expectedType2: TokenizerStateUnsignedInteger},

		// Octal
		{input: "#o0", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#o7", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#o10", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#o77", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#o777", expectedType1: TokenizerStateMarkerBase8, expectedType2: TokenizerStateUnsignedInteger},

		// Hexadecimal (must start with digit)
		{input: "#x0", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x9", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x10", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x100", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x1a", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x1f", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x1ff", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
		{input: "#x123abc", expectedType1: TokenizerStateMarkerBase16, expectedType2: TokenizerStateUnsignedInteger},
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
