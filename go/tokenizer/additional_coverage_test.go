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
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestTokenValue tests the Value() method on SimpleToken
func TestTokenValue(t *testing.T) {
	tcs := []struct {
		input        string
		expectedVal  string
		expectedType TokenizerState
	}{
		{
			input:        `"hello"`,
			expectedVal:  "hello",
			expectedType: TokenizerStateStringEnd,
		},
		{
			input:        `""`,
			expectedVal:  "",
			expectedType: TokenizerStateStringEnd,
		},
		{
			input:        `"test\nstring"`,
			expectedVal:  "test\nstring",
			expectedType: TokenizerStateStringEnd,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.Value(), qt.Equals, tc.expectedVal)
		})
	}
}

// TestConsDot tests the cons dot tokenization
func TestConsDot(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        ". ",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
		},
		{
			input:        ".)",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
		},
		{
			input:        ".\n",
			expectedType: TokenizerStateCons,
			expectedStr:  ".",
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

// TestPeculiarIdentifiersExtended tests additional peculiar identifier cases
func TestPeculiarIdentifiersExtended(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "...more",
			expectedType: TokenizerStateSymbol,
			expectedStr:  "...more",
		},
		{
			input:        ".foo",
			expectedType: TokenizerStateSymbol,
			expectedStr:  ".foo",
		},
		{
			input:        "+soup+",
			expectedType: TokenizerStateSymbol,
			expectedStr:  "+soup+",
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

// TestDirectives tests directive tokenization
func TestDirectives(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "#!fold-case",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!fold-case",
		},
		{
			input:        "#!no-fold-case",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!no-fold-case",
		},
		{
			input:        "#!r7rs",
			expectedType: TokenizerStateDirective,
			expectedStr:  "#!r7rs",
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

// TestVectorMarker tests vector marker tokenization
func TestVectorMarker(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "#(",
			expectedType: TokenizerStateOpenVector,
			expectedStr:  "#(",
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

// TestStringEscapesExtended tests additional string escape sequences
func TestStringEscapesExtended(t *testing.T) {
	tcs := []struct {
		input       string
		expectedVal string
	}{
		{
			input:       `"back\bspace"`,
			expectedVal: "back\bspace",
		},
		{
			input:       `"alarm\a"`,
			expectedVal: "alarm\a",
		},
		{
			input:       `"return\rchar"`,
			expectedVal: "return\rchar",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Value(), qt.Equals, tc.expectedVal)
		})
	}
}

// TestCommentPhases tests multi-phase comment tokenization
func TestCommentPhases(t *testing.T) {
	tcs := []struct {
		input          string
		expectedPhases []struct {
			typ TokenizerState
			str string
		}
	}{
		{
			input: "; line comment\n123",
			expectedPhases: []struct {
				typ TokenizerState
				str string
			}{
				{TokenizerStateLineCommentBegin, ";"},
				{TokenizerStateLineCommentBody, " line comment"},
				{TokenizerStateLineCommentEnd, "\n"},
				{TokenizerStateUnsignedInteger, "123"},
			},
		},
		{
			input: "#| block |#456",
			expectedPhases: []struct {
				typ TokenizerState
				str string
			}{
				{TokenizerStateBlockCommentBegin, "#|"},
				{TokenizerStateBlockCommentBody, " block "},
				{TokenizerStateBlockCommentEnd, "|#"},
				{TokenizerStateUnsignedInteger, "456"},
			},
		},
	}

	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizerWithComments(strings.NewReader(tc.input), false, true)
			for j, expected := range tc.expectedPhases {
				token, err := tok.Next()
				c.Check(err, qt.IsNil, qt.Commentf("phase %d", j))
				c.Check(token.Type(), qt.Equals, expected.typ, qt.Commentf("phase %d", j))
				c.Check(token.String(), qt.Equals, expected.str, qt.Commentf("phase %d", j))
			}
		})
	}
}

// TestNestedBlockComments tests nested block comment support
func TestNestedBlockComments(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "#| outer #| inner |# outer |#",
			expectedType: TokenizerStateBlockComment,
			expectedStr:  "#| outer #| inner |# outer |#",
		},
		{
			input:        "#| #| #| deep |# |# |#",
			expectedType: TokenizerStateBlockComment,
			expectedStr:  "#| #| #| deep |# |# |#",
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
			expectedType2: TokenizerStateUnsignedInteger,
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

// TestEqualTo tests the EqualTo method on tokens
func TestEqualTo(t *testing.T) {
	tok1 := NewTokenizer(strings.NewReader("123"), false)
	token1, err1 := tok1.Next()
	qt.Check(t, err1, qt.IsNil)
	st1, ok1 := token1.(*SimpleToken)
	qt.Check(t, ok1, qt.IsTrue)

	tok2 := NewTokenizer(strings.NewReader("123"), false)
	token2, err2 := tok2.Next()
	qt.Check(t, err2, qt.IsNil)
	st2, ok2 := token2.(*SimpleToken)
	qt.Check(t, ok2, qt.IsTrue)

	tok3 := NewTokenizer(strings.NewReader("456"), false)
	token3, err3 := tok3.Next()
	qt.Check(t, err3, qt.IsNil)
	st3, ok3 := token3.(*SimpleToken)
	qt.Check(t, ok3, qt.IsTrue)

	// Test that equal tokens are equal
	qt.Check(t, st1.EqualTo(st2), qt.IsTrue)

	// Test that different tokens are not equal
	qt.Check(t, st1.EqualTo(st3), qt.IsFalse)

	// Test equality with different types
	tok4 := NewTokenizer(strings.NewReader("abc"), false)
	token4, err4 := tok4.Next()
	qt.Check(t, err4, qt.IsNil)
	st4, ok4 := token4.(*SimpleToken)
	qt.Check(t, ok4, qt.IsTrue)
	qt.Check(t, st1.EqualTo(st4), qt.IsFalse)
}

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

// TestNewTokenizerEdgeCases tests edge cases in NewTokenizer
func TestNewTokenizerEdgeCases(t *testing.T) {
	// Test case insensitive mode
	tcs := []struct {
		input        string
		caseInsens   bool
		expectedType TokenizerState
		expectedStr  string
	}{
		{
			input:        "ABC",
			caseInsens:   false,
			expectedType: TokenizerStateSymbol,
			expectedStr:  "ABC",
		},
		{
			input:        "abc",
			caseInsens:   true,
			expectedType: TokenizerStateSymbol,
			expectedStr:  "abc",
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q ci=%v", i, tc.input, tc.caseInsens), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), tc.caseInsens)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
			c.Check(token.String(), qt.Equals, tc.expectedStr)
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
			expectedType2: TokenizerStateUnsignedInteger,
		},
		{
			input:         "#o3771",
			expectedType1: TokenizerStateMarkerBase8,
			expectedType2: TokenizerStateUnsignedInteger,
		},
		{
			input:         "#d9991",
			expectedType1: TokenizerStateMarkerBase10,
			expectedType2: TokenizerStateUnsignedInteger,
		},
		{
			input:         "#x1fff",
			expectedType1: TokenizerStateMarkerBase16,
			expectedType2: TokenizerStateUnsignedInteger,
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

// TestClose tests the Close method
func TestClose(t *testing.T) {
	tok := NewTokenizer(strings.NewReader("123"), false)

	// Read token first
	_, err := tok.Next()
	qt.Check(t, err, qt.IsNil)

	// Now close
	err = tok.Close()
	qt.Check(t, err, qt.IsNil)
}

// TestContinueCommentToken tests comment continuation
func TestContinueCommentToken(t *testing.T) {
	tcs := []struct {
		input string
		count int // number of tokens expected
	}{
		{
			input: "; comment\n",
			count: 4, // begin, body, end, EOF
		},
		{
			input: "#| block |#",
			count: 4, // begin, body, end, EOF
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizerWithComments(strings.NewReader(tc.input), false, true)
			count := 0
			for {
				_, err := tok.Next()
				if err == io.EOF {
					break
				}
				c.Check(err, qt.IsNil)
				count++
			}
			c.Check(count, qt.Equals, tc.count-1) // -1 because we don't count EOF
		})
	}
}

// TestContinueLineComment tests line comment phases
func TestContinueLineComment(t *testing.T) {
	input := "; this is a line comment\n"
	tok := NewTokenizerWithComments(strings.NewReader(input), false, true)

	// Phase 1: Begin
	token1, err1 := tok.Next()
	qt.Check(t, err1, qt.IsNil)
	qt.Check(t, token1.Type(), qt.Equals, TokenizerStateLineCommentBegin)

	// Phase 2: Body
	token2, err2 := tok.Next()
	qt.Check(t, err2, qt.IsNil)
	qt.Check(t, token2.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// Phase 3: End
	token3, err3 := tok.Next()
	qt.Check(t, err3, qt.IsNil)
	qt.Check(t, token3.Type(), qt.Equals, TokenizerStateLineCommentEnd)
}

// TestContinueBlockComment tests block comment phases
func TestContinueBlockComment(t *testing.T) {
	input := "#| block comment |#"
	tok := NewTokenizerWithComments(strings.NewReader(input), false, true)

	// Phase 1: Begin
	token1, err1 := tok.Next()
	qt.Check(t, err1, qt.IsNil)
	qt.Check(t, token1.Type(), qt.Equals, TokenizerStateBlockCommentBegin)

	// Phase 2: Body
	token2, err2 := tok.Next()
	qt.Check(t, err2, qt.IsNil)
	qt.Check(t, token2.Type(), qt.Equals, TokenizerStateBlockCommentBody)

	// Phase 3: End
	token3, err3 := tok.Next()
	qt.Check(t, err3, qt.IsNil)
	qt.Check(t, token3.Type(), qt.Equals, TokenizerStateBlockCommentEnd)
}

// TestIsCommentToken tests the isCommentToken function
func TestIsCommentToken(t *testing.T) {
	tcs := []struct {
		input        string
		withComments bool
		expectedType TokenizerState
	}{
		{
			input:        "; comment\n123",
			withComments: false,
			expectedType: TokenizerStateLineCommentStart,
		},
		{
			input:        "#| comment |#456",
			withComments: false,
			expectedType: TokenizerStateBlockComment,
		},
		{
			input:        "#;(datum)",
			withComments: false,
			expectedType: TokenizerStateDatumComment,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizerWithComments(strings.NewReader(tc.input), false, tc.withComments)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
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

// TestMayUnsignedFractionalEdgeCases tests fractional number edge cases
func TestMayUnsignedFractionalEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
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

// TestScanLineEnding tests scanLineEnding coverage
func TestScanLineEnding(t *testing.T) {
	tcs := []struct {
		input string
	}{
		{input: "\n"},
		{input: "\r"},
		{input: "\r\n"},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			// Read a symbol followed by the line ending
			fullInput := "x" + tc.input + "y"
			tok := NewTokenizer(strings.NewReader(fullInput), false)

			// First token: x
			token1, err1 := tok.Next()
			c.Check(err1, qt.IsNil)
			c.Check(token1.String(), qt.Equals, "x")

			// Second token: y (line ending was consumed)
			token2, err2 := tok.Next()
			c.Check(err2, qt.IsNil)
			c.Check(token2.String(), qt.Equals, "y")
		})
	}
}

// TestReadBlockCommentEdgeCases tests block comment edge cases
func TestReadBlockCommentEdgeCases(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
		shouldError  bool
	}{
		{
			input:        "#||#",
			expectedType: TokenizerStateBlockComment,
			shouldError:  false,
		},
		{
			input:        "#| |#",
			expectedType: TokenizerStateBlockComment,
			shouldError:  false,
		},
		{
			input:        "#| nested #| deep |# |#",
			expectedType: TokenizerStateBlockComment,
			shouldError:  false,
		},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			if tc.shouldError {
				c.Check(err, qt.Not(qt.IsNil))
			} else {
				c.Check(err, qt.IsNil)
				c.Check(token.Type(), qt.Equals, tc.expectedType)
			}
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

