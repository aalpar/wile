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
		{input: "1.5+i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1.5-i", expectedType: TokenizerStateUnsignedComplex},

		// With inf/nan imaginary parts
		{input: "1+inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-inf.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1+nan.0i", expectedType: TokenizerStateUnsignedComplex},
		{input: "1-nan.0i", expectedType: TokenizerStateUnsignedComplex},

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

// TestNumberFractionalVariations tests to improve mayUnsignedFractionalRealNumberOrRationalRealNumber coverage
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
