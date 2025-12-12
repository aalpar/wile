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

// TestCharacterConstants tests character constant edge cases
func TestCharacterConstants(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		// Mnemonics
		{input: `#\alarm`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\backspace`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\delete`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\escape`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\newline`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\null`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\return`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\space`, expectedType: TokenizerStateCharMnemonic},
		{input: `#\tab`, expectedType: TokenizerStateCharMnemonic},

		// Hex escapes
		{input: `#\x0`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x00`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x20`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\xFF`, expectedType: TokenizerStateCharHexEscape},
		{input: `#\x1234`, expectedType: TokenizerStateCharHexEscape},

		// Graphic characters
		{input: `#\a`, expectedType: TokenizerStateCharGraphic},
		{input: `#\A`, expectedType: TokenizerStateCharGraphic},
		{input: `#\0`, expectedType: TokenizerStateCharGraphic},
		{input: `#\!`, expectedType: TokenizerStateCharGraphic},
		{input: `#\(`, expectedType: TokenizerStateCharGraphic},
		{input: `#\)`, expectedType: TokenizerStateCharGraphic},
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

// TestStringEscapeVariations tests more string escape combinations
func TestStringEscapeVariations(t *testing.T) {
	tcs := []struct {
		input       string
		expectedVal string
	}{
		{input: `"\a"`, expectedVal: "\a"},
		{input: `"\b"`, expectedVal: "\b"},
		{input: `"\t"`, expectedVal: "\t"},
		{input: `"\n"`, expectedVal: "\n"},
		{input: `"\r"`, expectedVal: "\r"},
		{input: `"\""`, expectedVal: `"`},
		{input: `"\\"`, expectedVal: `\`},
		{input: `"\x00"`, expectedVal: "\x00"},
		{input: `"\x20"`, expectedVal: " "},
		{input: `"\x41"`, expectedVal: "A"},
		{input: `"\x00\x01\x02"`, expectedVal: "\x00\x01\x02"},
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

// TestTypedArrayVariations tests typed array marker variations
func TestTypedArrayVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#u8(", expectedType: TokenizerStateOpenVectorUnsignedByteMarker},
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

// TestBooleanVariations tests boolean constant variations
func TestBooleanVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#t", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#T", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#true", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#True", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#TRUE", expectedType: TokenizerStateMarkerBooleanTrue},
		{input: "#f", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#F", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#false", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#False", expectedType: TokenizerStateMarkerBooleanFalse},
		{input: "#FALSE", expectedType: TokenizerStateMarkerBooleanFalse},
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

// TestDatumCommentExtended tests datum comment tokenization
func TestDatumCommentExtended(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#;", expectedType: TokenizerStateDatumComment},
		{input: "#; ", expectedType: TokenizerStateDatumComment},
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

// TestQuotingAndQuasiquoting tests quote-like tokens
func TestQuotingAndQuasiquoting(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "'", expectedType: TokenizerStateQuote},
		{input: "`", expectedType: TokenizerStateQuasiquote},
		{input: ",", expectedType: TokenizerStateUnquote},
		{input: ",@", expectedType: TokenizerStateUnquoteSplicing},
		{input: "#'", expectedType: TokenizerStateSyntax},
		{input: "#`", expectedType: TokenizerStateQuasisyntax},
		{input: "#,", expectedType: TokenizerStateUnsyntax},
		{input: "#,@", expectedType: TokenizerStateUnsyntaxSplicing},
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

// TestSymbolVariations tests various symbol forms
func TestSymbolVariations(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "foo", expectedType: TokenizerStateSymbol},
		{input: "foo-bar", expectedType: TokenizerStateSymbol},
		{input: "foo_bar", expectedType: TokenizerStateSymbol},
		{input: "foo?", expectedType: TokenizerStateSymbol},
		{input: "foo!", expectedType: TokenizerStateSymbol},
		{input: "foo.", expectedType: TokenizerStateSymbol},
		{input: "->string", expectedType: TokenizerStateSymbol},
		{input: "string->list", expectedType: TokenizerStateSymbol},
		{input: "list->vector", expectedType: TokenizerStateSymbol},
		{input: "+", expectedType: TokenizerStateSymbol},
		{input: "-", expectedType: TokenizerStateSymbol},
		{input: "*", expectedType: TokenizerStateSymbol},
		{input: "/", expectedType: TokenizerStateSymbol},
		{input: "<", expectedType: TokenizerStateSymbol},
		{input: ">", expectedType: TokenizerStateSymbol},
		{input: "=", expectedType: TokenizerStateSymbol},
		{input: "<=", expectedType: TokenizerStateSymbol},
		{input: ">=", expectedType: TokenizerStateSymbol},
		{input: "...", expectedType: TokenizerStateSymbol},
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
