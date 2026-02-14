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

// tokenizerTestCase is the common struct for table-driven tokenizer tests
// that check if an input string tokenizes to a specific state.
type tokenizerTestCase struct {
	in    string
	state TokenizerState
}

// Test Token.Value() branches
func TestToken_Value_StringEnd(t *testing.T) {
	// Test empty string - should return val even when empty
	p := NewTokenizer(strings.NewReader(`""`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
	qt.Assert(t, tok.Value(), qt.Equals, "")
}

func TestToken_Value_WithVal(t *testing.T) {
	// Test string with escape sequences - val should contain processed value
	p := NewTokenizer(strings.NewReader(`"hello\nworld"`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
	// val should have actual newline, src has raw escape
	qt.Assert(t, tok.Value(), qt.Contains, "\n")
}

func TestToken_Value_NoVal(t *testing.T) {
	// Test symbol - should return src when val is empty
	p := NewTokenizer(strings.NewReader(`hello`), false)
	tok, err := p.Next()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateSymbol)
	qt.Assert(t, tok.Value(), qt.Equals, "hello")
}

// Test cons dot (improper list notation)
func TestTokenizer_ConsDot(t *testing.T) {
	tests := []tokenizerTestCase{
		{"(a . b)", TokenizerStateCons},
		{"(1 . 2)", TokenizerStateCons},
		{"(+ . args)", TokenizerStateCons},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			// Skip opening paren
			_, _ = p.Next()
			// Skip first element
			_, _ = p.Next()
			// Get the dot
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, tc.state)
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

// Test dot followed by symbol subsequent (peculiar identifiers)
func TestTokenizer_DotSubsequent(t *testing.T) {
	tests := []tokenizerTestCase{
		{"+..", TokenizerStateSymbol},
		{"+.@", TokenizerStateSymbol},
		{"-..", TokenizerStateSymbol},
		{"+.+", TokenizerStateSymbol},
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
			p.span()
			c.Check(err, qt.ErrorIs, tc.err0)
			c.Check(state, qt.Equals, tc.state)
			c.Check(p.span(), qt.Equals, tc.scan)
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

// Test continueCommentToken for better coverage
func TestTokenizer_ContinueCommentToken(t *testing.T) {
	// Line comment with content
	p := NewTokenizerWithComments(strings.NewReader("; a comment\n"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// Block comment
	p = NewTokenizerWithComments(strings.NewReader("#| block |#"), false)
	tok1, _ = p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// Test typed arrays (u8 vectors) - R7RS §7.1.1: case-insensitive
func TestTokenizer_TypedArrays(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#u8(1 2 3)", TokenizerStateOpenVectorUnsignedByteMarker},
		{"#U8(1 2 3)", TokenizerStateOpenVectorUnsignedByteMarker}, // uppercase
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

// Test extended symbols starting with |
// Skipped: Extended symbol implementation is in progress - see TestExtendedSymbols in edge_cases_test.go
func TestTokenizer_ExtendedSymbolsBasic(t *testing.T) {
	tests := []tokenizerTestCase{
		{"|", TokenizerStateSymbol},
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

// Test more typed array and radix branches
func TestTokenizer_TypedArrayRadixBranches(t *testing.T) {
	tests := []tokenizerTestCase{
		// Case insensitive booleans
		{"#T", TokenizerStateMarkerBooleanTrue},
		{"#TRUE", TokenizerStateMarkerBooleanTrue},
		{"#F", TokenizerStateMarkerBooleanFalse},
		{"#FALSE", TokenizerStateMarkerBooleanFalse},
		// After partial match, continue as marker
		{"#tfoo", TokenizerStateMarker},
		{"#ffoo", TokenizerStateMarker},
		// # followed by unknown letter
		{"#foo", TokenizerStateMarker},
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

// Test string escape sequences
func TestTokenizer_StringEscapeSequences(t *testing.T) {
	tests := []struct {
		in  string
		val string
	}{
		{`"\a"`, "\a"},
		{`"\b"`, "\b"},
		{`"\t"`, "\t"},
		{`"\n"`, "\n"},
		{`"\r"`, "\r"},
		{`"\\"`, "\\"},
		{`"\""`, "\""},
		{`"\x41;"`, "A"},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
			qt.Assert(t, tok.Value(), qt.Equals, tc.val)
		})
	}
}

// Test block comment with multi-token mode
func TestTokenizer_BlockCommentMultiToken(t *testing.T) {
	p := NewTokenizerWithComments(strings.NewReader("#| content |#"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
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

// Test datum comments
func TestTokenizer_DatumComment(t *testing.T) {
	p := NewTokenizer(strings.NewReader("#;foo bar"), false)
	// In non-emit mode, datum comment is skipped
	tok, _ := p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)

	p = NewTokenizerWithComments(strings.NewReader("#;foo bar"), false)
	tok, _ = p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
}

// Test character literals
func TestTokenizer_CharacterLiterals(t *testing.T) {
	tests := []tokenizerTestCase{
		{`#\a`, TokenizerStateCharGraphic},
		{`#\Z`, TokenizerStateCharGraphic},
		{`#\space`, TokenizerStateCharMnemonic},
		{`#\newline`, TokenizerStateCharMnemonic},
		{`#\tab`, TokenizerStateCharMnemonic},
		{`#\return`, TokenizerStateCharMnemonic},
		{`#\x41`, TokenizerStateCharHexEscape},
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

// Test label references and assignments
func TestTokenizer_Labels(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#123=", TokenizerStateLabelAssignment},
		{"#123#", TokenizerStateLabelReference},
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

// Test directives
func TestTokenizer_Directives(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#!fold-case", TokenizerStateDirective},
		{"#!no-fold-case", TokenizerStateDirective},
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

// Test vectors
func TestTokenizer_Vectors(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#(", TokenizerStateOpenVector},
		{"#(1 2 3)", TokenizerStateOpenVector},
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

// Test additional comment scenarios
func TestTokenizer_CommentScenarios(t *testing.T) {
	// Line comment at EOF
	p := NewTokenizerWithComments(strings.NewReader("; comment"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	// No End token since there's no newline - should get EOF

	// Block comment without closing (incomplete)
	p = NewTokenizerWithComments(strings.NewReader("#| incomplete"), false)
	tok1, _ = p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// Test strings with various content
func TestTokenizer_StringVariations(t *testing.T) {
	tests := []struct {
		in string
	}{
		{`"simple"`},
		{`"with spaces"`},
		{`"with\ttab"`},
		{`"multi\nline"`},
		{`""`}, // empty string
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
		})
	}
}

// Test isCommentToken helper
func TestTokenizer_IsCommentTokenHelper(t *testing.T) {
	// Datum comment without emit mode
	p := NewTokenizer(strings.NewReader("#; comment\nfoo"), false)
	tok, _ := p.Next()
	// In non-emit mode, datum comment gets a single token
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
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

// Test string escape sequence branches (exercises readDelimited + readEscapeSequence)
func TestTokenizer_StringEscapeBranches(t *testing.T) {
	tests := []struct {
		in  string
		val string
	}{
		// Supported escape sequences
		{`"\a"`, "\a"}, // alert
		{`"\b"`, "\b"}, // backspace
		{`"\n"`, "\n"}, // newline
		{`"\r"`, "\r"}, // carriage return
		{`"\t"`, "\t"}, // tab
		{`"\\"`, "\\"}, // backslash
		{`"\""`, "\""}, // double quote
		{`"|"`, "|"},   // bar (no escape needed)
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			tok, err := p.Next()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateString)
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

// Test line ending variations
func TestTokenizer_LineEndingVariations(t *testing.T) {
	// CRLF line ending
	p := NewTokenizerWithComments(strings.NewReader("; comment\r\nfoo"), false)
	tok, _ := p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// CR only line ending
	p = NewTokenizerWithComments(strings.NewReader("; comment\rfoo"), false)
	tok, _ = p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateLineCommentBody)
}

// Test syntax quotation tokens
func TestTokenizer_SyntaxQuotation(t *testing.T) {
	tests := []tokenizerTestCase{
		{"#'x", TokenizerStateSyntax},
		{"#`x", TokenizerStateQuasisyntax},
		{"#,x", TokenizerStateUnsyntax},
		{"#,@x", TokenizerStateUnsyntaxSplicing},
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
