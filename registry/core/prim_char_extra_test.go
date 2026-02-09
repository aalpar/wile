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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestCharPredicate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char? with character",
			prog: values.List(values.NewSymbol("char?"), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char? with integer",
			prog: values.List(values.NewSymbol("char?"), values.NewInteger(42)),
			out:  values.FalseValue,
		},
		{
			name: "char? with string",
			prog: values.List(values.NewSymbol("char?"), values.NewString("a")),
			out:  values.FalseValue,
		},
		{
			name: "char? with symbol",
			prog: values.List(values.NewSymbol("char?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("a"))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharAlphabetic(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-alphabetic? with lowercase letter",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char-alphabetic? with uppercase letter",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('Z')),
			out:  values.TrueValue,
		},
		{
			name: "char-alphabetic? with digit",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('1')),
			out:  values.FalseValue,
		},
		{
			name: "char-alphabetic? with space",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter(' ')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharNumeric(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-numeric? with digit",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('5')),
			out:  values.TrueValue,
		},
		{
			name: "char-numeric? with zero",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('0')),
			out:  values.TrueValue,
		},
		{
			name: "char-numeric? with letter",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "char-numeric? with space",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter(' ')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharWhitespace(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-whitespace? with space",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter(' ')),
			out:  values.TrueValue,
		},
		{
			name: "char-whitespace? with tab",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\t')),
			out:  values.TrueValue,
		},
		{
			name: "char-whitespace? with newline",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\n')),
			out:  values.TrueValue,
		},
		{
			name: "char-whitespace? with letter",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "char-whitespace? with digit",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('1')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharUpperCase(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-upper-case? with uppercase letter",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('A')),
			out:  values.TrueValue,
		},
		{
			name: "char-upper-case? with uppercase Z",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('Z')),
			out:  values.TrueValue,
		},
		{
			name: "char-upper-case? with lowercase letter",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "char-upper-case? with digit",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('1')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharLowerCase(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-lower-case? with lowercase letter",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char-lower-case? with lowercase z",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('z')),
			out:  values.TrueValue,
		},
		{
			name: "char-lower-case? with uppercase letter",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('A')),
			out:  values.FalseValue,
		},
		{
			name: "char-lower-case? with digit",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('1')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharUpcase(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-upcase lowercase a",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('a')),
			out:  values.NewCharacter('A'),
		},
		{
			name: "char-upcase lowercase z",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('z')),
			out:  values.NewCharacter('Z'),
		},
		{
			name: "char-upcase uppercase A",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('A')),
			out:  values.NewCharacter('A'),
		},
		{
			name: "char-upcase digit",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('1')),
			out:  values.NewCharacter('1'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharDowncase(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-downcase uppercase A",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('A')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "char-downcase uppercase Z",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('Z')),
			out:  values.NewCharacter('z'),
		},
		{
			name: "char-downcase lowercase a",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('a')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "char-downcase digit",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('1')),
			out:  values.NewCharacter('1'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharFoldcase(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char-foldcase uppercase A",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('A')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "char-foldcase uppercase Z",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('Z')),
			out:  values.NewCharacter('z'),
		},
		{
			name: "char-foldcase lowercase a",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('a')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "char-foldcase digit",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('1')),
			out:  values.NewCharacter('1'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestDigitValue(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "digit-value with '0'",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('0')),
			out:  values.NewInteger(0),
		},
		{
			name: "digit-value with '5'",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('5')),
			out:  values.NewInteger(5),
		},
		{
			name: "digit-value with '9'",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('9')),
			out:  values.NewInteger(9),
		},
		{
			name: "digit-value with letter",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "digit-value with space",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter(' ')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharFoldcaseUnicode tests R7RS Unicode simple case folding for char-foldcase.
// Per R7RS §6.6, char-foldcase uses Unicode simple case-folding which maps each
// character to exactly one character.
func TestCharFoldcaseUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// Basic ASCII
		{
			name: "uppercase A folds to a",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('A')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "lowercase a stays a",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('a')),
			out:  values.NewCharacter('a'),
		},
		{
			name: "digit stays same",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('5')),
			out:  values.NewCharacter('5'),
		},
		// German sharp S - simple folding keeps it unchanged
		// (only string-foldcase expands to "ss")
		{
			name: "sharp s stays sharp s (simple fold)",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('ß')),
			out:  values.NewCharacter('ß'),
		},
		{
			name: "capital sharp S folds to sharp s",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('ẞ')),
			out:  values.NewCharacter('ß'),
		},
		// Greek letters
		{
			name: "Greek uppercase sigma folds to lowercase",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('Σ')),
			out:  values.NewCharacter('σ'),
		},
		{
			name: "Greek lowercase sigma stays same",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('σ')),
			out:  values.NewCharacter('σ'),
		},
		// Accented characters
		{
			name: "uppercase E-acute folds to lowercase",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('É')),
			out:  values.NewCharacter('é'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestDigitValueUnicode tests R7RS Unicode digit-value for non-ASCII decimal digits.
// Per R7RS §6.6, digit-value returns the numeric value (0-9) for any Unicode
// character in the Nd (Number, Decimal Digit) category.
func TestDigitValueUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// Arabic-Indic digits (U+0660-U+0669)
		{
			name: "Arabic-Indic 0",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('٠')), // U+0660
			out:  values.NewInteger(0),
		},
		{
			name: "Arabic-Indic 5",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('٥')), // U+0665
			out:  values.NewInteger(5),
		},
		{
			name: "Arabic-Indic 9",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('٩')), // U+0669
			out:  values.NewInteger(9),
		},
		// Extended Arabic-Indic digits (U+06F0-U+06F9)
		{
			name: "Extended Arabic-Indic 0",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('۰')), // U+06F0
			out:  values.NewInteger(0),
		},
		{
			name: "Extended Arabic-Indic 5",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('۵')), // U+06F5
			out:  values.NewInteger(5),
		},
		{
			name: "Extended Arabic-Indic 9",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('۹')), // U+06F9
			out:  values.NewInteger(9),
		},
		// Devanagari digits (U+0966-U+096F)
		{
			name: "Devanagari 0",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('०')), // U+0966
			out:  values.NewInteger(0),
		},
		{
			name: "Devanagari 5",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('५')), // U+096B
			out:  values.NewInteger(5),
		},
		{
			name: "Devanagari 9",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('९')), // U+096F
			out:  values.NewInteger(9),
		},
		// Bengali digits (U+09E6-U+09EF)
		{
			name: "Bengali 0",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('০')), // U+09E6
			out:  values.NewInteger(0),
		},
		{
			name: "Bengali 7",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('৭')), // U+09ED
			out:  values.NewInteger(7),
		},
		// Thai digits (U+0E50-U+0E59)
		{
			name: "Thai 0",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('๐')), // U+0E50
			out:  values.NewInteger(0),
		},
		{
			name: "Thai 3",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('๓')), // U+0E53
			out:  values.NewInteger(3),
		},
		// Non-digits should return #f
		{
			name: "superscript 2 is not a decimal digit",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('²')), // U+00B2
			out:  values.FalseValue,
		},
		{
			name: "roman numeral is not a decimal digit",
			prog: values.List(values.NewSymbol("digit-value"), values.NewCharacter('Ⅴ')), // U+2164
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharAlphabeticUnicode tests char-alphabetic? with Unicode characters
// from multiple scripts. Per R7RS §6.6, char-alphabetic? returns #t for
// characters with the Unicode "Alphabetic" property.
func TestCharAlphabeticUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// Latin
		{
			name: "Latin uppercase A",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('A')),
			out:  values.TrueValue,
		},
		{
			name: "Latin lowercase z",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('z')),
			out:  values.TrueValue,
		},
		// Greek
		{
			name: "Greek lowercase alpha",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('α')), // U+03B1
			out:  values.TrueValue,
		},
		{
			name: "Greek uppercase omega",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('Ω')), // U+03A9
			out:  values.TrueValue,
		},
		// Cyrillic
		{
			name: "Cyrillic uppercase Ya",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('Я')), // U+042F
			out:  values.TrueValue,
		},
		{
			name: "Cyrillic lowercase de",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('д')), // U+0434
			out:  values.TrueValue,
		},
		// Hebrew
		{
			name: "Hebrew alef",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('א')), // U+05D0
			out:  values.TrueValue,
		},
		// Arabic
		{
			name: "Arabic ain",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ع')), // U+0639
			out:  values.TrueValue,
		},
		// CJK
		{
			name: "CJK ideograph",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('中')), // U+4E2D
			out:  values.TrueValue,
		},
		// Turkish special letters
		{
			name: "Turkish dotted capital I",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('İ')), // U+0130
			out:  values.TrueValue,
		},
		{
			name: "Turkish dotless lowercase i",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ı')), // U+0131
			out:  values.TrueValue,
		},
		// German sharp S
		{
			name: "German lowercase sharp s",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ß')), // U+00DF
			out:  values.TrueValue,
		},
		{
			name: "German capital sharp S",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.TrueValue,
		},
		// Non-alphabetic characters
		{
			name: "ASCII digit 0 is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('0')),
			out:  values.FalseValue,
		},
		{
			name: "Arabic-Indic digit 3 is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('٣')), // U+0663
			out:  values.FalseValue,
		},
		{
			name: "space is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter(' ')),
			out:  values.FalseValue,
		},
		{
			name: "newline is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('\n')),
			out:  values.FalseValue,
		},
		{
			name: "plus sign is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('+')),
			out:  values.FalseValue,
		},
		{
			name: "at sign is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('@')),
			out:  values.FalseValue,
		},
		{
			name: "emoji is not alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('😀')), // U+1F600
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharNumericUnicode tests char-numeric? with Unicode characters from
// multiple numeral systems. Per R7RS §6.6, char-numeric? returns #t for
// characters with the Unicode "Numeric_Type=Decimal" property.
func TestCharNumericUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// ASCII digits
		{
			name: "ASCII digit 0",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('0')),
			out:  values.TrueValue,
		},
		{
			name: "ASCII digit 9",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('9')),
			out:  values.TrueValue,
		},
		// Arabic-Indic
		{
			name: "Arabic-Indic 0",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('٠')), // U+0660
			out:  values.TrueValue,
		},
		// Devanagari
		{
			name: "Devanagari 5",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('५')), // U+096B
			out:  values.TrueValue,
		},
		// Thai
		{
			name: "Thai 7",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('๗')), // U+0E57
			out:  values.TrueValue,
		},
		// Non-numeric characters
		{
			name: "letter a is not numeric",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "Greek alpha is not numeric",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('α')),
			out:  values.FalseValue,
		},
		{
			name: "Roman numeral V is not numeric (Nl category, not Nd)",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('Ⅴ')), // U+2164
			out:  values.FalseValue,
		},
		{
			name: "superscript 2 is not numeric (No category, not Nd)",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('²')), // U+00B2
			out:  values.FalseValue,
		},
		{
			name: "Turkish dotted capital I is not numeric",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('İ')), // U+0130
			out:  values.FalseValue,
		},
		{
			name: "Turkish dotless i is not numeric",
			prog: values.List(values.NewSymbol("char-numeric?"), values.NewCharacter('ı')), // U+0131
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharWhitespaceUnicode tests char-whitespace? with Unicode whitespace
// characters. Per R7RS §6.6, char-whitespace? returns #t for characters
// with the Unicode "White_Space" property.
func TestCharWhitespaceUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// ASCII whitespace
		{
			name: "space",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter(' ')),
			out:  values.TrueValue,
		},
		{
			name: "tab",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\t')),
			out:  values.TrueValue,
		},
		{
			name: "newline",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\n')),
			out:  values.TrueValue,
		},
		{
			name: "carriage return",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\r')),
			out:  values.TrueValue,
		},
		// Unicode whitespace
		{
			name: "no-break space",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u00A0')), // U+00A0
			out:  values.TrueValue,
		},
		{
			name: "em space",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u2003')), // U+2003
			out:  values.TrueValue,
		},
		{
			name: "ideographic space (CJK)",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u3000')), // U+3000
			out:  values.TrueValue,
		},
		{
			name: "line separator",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u2028')), // U+2028
			out:  values.TrueValue,
		},
		{
			name: "paragraph separator",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u2029')), // U+2029
			out:  values.TrueValue,
		},
		// Non-whitespace characters
		{
			name: "letter a is not whitespace",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "Greek alpha is not whitespace",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('α')),
			out:  values.FalseValue,
		},
		{
			name: "zero-width space is not whitespace (U+200B)",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('\u200B')), // U+200B
			out:  values.FalseValue,
		},
		{
			name: "digit 0 is not whitespace",
			prog: values.List(values.NewSymbol("char-whitespace?"), values.NewCharacter('0')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharUpperCaseUnicode tests char-upper-case? with Unicode uppercase
// characters from multiple scripts. Per R7RS §6.6, char-upper-case? returns
// #t for characters with the Unicode "Uppercase" property.
func TestCharUpperCaseUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// Latin
		{
			name: "Latin uppercase A",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('A')),
			out:  values.TrueValue,
		},
		{
			name: "Latin uppercase Z",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('Z')),
			out:  values.TrueValue,
		},
		// Greek
		{
			name: "Greek uppercase sigma",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('Σ')), // U+03A3
			out:  values.TrueValue,
		},
		{
			name: "Greek uppercase omega",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('Ω')), // U+03A9
			out:  values.TrueValue,
		},
		// Cyrillic
		{
			name: "Cyrillic uppercase Ya",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('Я')), // U+042F
			out:  values.TrueValue,
		},
		// Turkish
		{
			name: "Turkish dotted capital I",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('İ')), // U+0130
			out:  values.TrueValue,
		},
		// German capital sharp S
		{
			name: "German capital sharp S",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.TrueValue,
		},
		// Accented
		{
			name: "uppercase E-acute",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('É')), // U+00C9
			out:  values.TrueValue,
		},
		// Non-uppercase characters
		{
			name: "lowercase a is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		{
			name: "Greek lowercase alpha is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('α')),
			out:  values.FalseValue,
		},
		{
			name: "Cyrillic lowercase ya is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('я')), // U+044F
			out:  values.FalseValue,
		},
		{
			name: "Turkish dotless i is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('ı')), // U+0131
			out:  values.FalseValue,
		},
		{
			name: "German lowercase sharp s is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('ß')), // U+00DF
			out:  values.FalseValue,
		},
		{
			name: "CJK ideograph has no case",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('中')), // U+4E2D
			out:  values.FalseValue,
		},
		{
			name: "digit 0 is not uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('0')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharLowerCaseUnicode tests char-lower-case? with Unicode lowercase
// characters from multiple scripts. Per R7RS §6.6, char-lower-case? returns
// #t for characters with the Unicode "Lowercase" property.
func TestCharLowerCaseUnicode(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// Latin
		{
			name: "Latin lowercase a",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "Latin lowercase z",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('z')),
			out:  values.TrueValue,
		},
		// Greek
		{
			name: "Greek lowercase sigma",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('σ')), // U+03C3
			out:  values.TrueValue,
		},
		{
			name: "Greek lowercase omega",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ω')), // U+03C9
			out:  values.TrueValue,
		},
		// Cyrillic
		{
			name: "Cyrillic lowercase ya",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('я')), // U+044F
			out:  values.TrueValue,
		},
		// Turkish
		{
			name: "Turkish dotless lowercase i",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ı')), // U+0131
			out:  values.TrueValue,
		},
		// German
		{
			name: "German lowercase sharp s",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ß')), // U+00DF
			out:  values.TrueValue,
		},
		// Accented
		{
			name: "lowercase e-acute",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('é')), // U+00E9
			out:  values.TrueValue,
		},
		// Non-lowercase characters
		{
			name: "uppercase A is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('A')),
			out:  values.FalseValue,
		},
		{
			name: "Greek uppercase sigma is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('Σ')), // U+03A3
			out:  values.FalseValue,
		},
		{
			name: "Cyrillic uppercase Ya is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('Я')), // U+042F
			out:  values.FalseValue,
		},
		{
			name: "Turkish dotted capital I is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('İ')), // U+0130
			out:  values.FalseValue,
		},
		{
			name: "German capital sharp S is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.FalseValue,
		},
		{
			name: "CJK ideograph has no case",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('中')), // U+4E2D
			out:  values.FalseValue,
		},
		{
			name: "digit 0 is not lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('0')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharCaseConversionTurkish tests Turkish special casing behavior.
// R7RS specifies locale-independent Unicode operations, so these use
// Unicode's simple case mappings (not Turkish locale rules).
func TestCharCaseConversionTurkish(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// char-upcase: U+0131 (dotless i) -> U+0049 (ASCII I)
		// Go's unicode.ToUpper maps U+0131 to U+0049 per Unicode simple
		// case mapping, not the Turkish-locale mapping to U+0130.
		{
			name: "upcase dotless i to ASCII I",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('ı')), // U+0131
			out:  values.NewCharacter('I'),                                               // U+0049
		},
		// char-downcase: U+0130 (dotted capital I) -> U+0069 (ASCII i)
		// Go's unicode.ToLower maps U+0130 to U+0069, which is correct per
		// Unicode simple case mapping (not Turkish locale).
		{
			name: "downcase dotted capital I to ASCII i",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('İ')), // U+0130
			out:  values.NewCharacter('i'),                                                 // U+0069
		},
		// char-foldcase: U+0130 -> U+0069 (simple case fold)
		{
			name: "foldcase dotted capital I to ASCII i",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('İ')), // U+0130
			out:  values.NewCharacter('i'),                                                 // U+0069
		},
		// Predicate checks
		{
			name: "dotted capital I is uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('İ')), // U+0130
			out:  values.TrueValue,
		},
		{
			name: "dotless i is lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ı')), // U+0131
			out:  values.TrueValue,
		},
		{
			name: "dotted capital I is alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('İ')), // U+0130
			out:  values.TrueValue,
		},
		{
			name: "dotless i is alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ı')), // U+0131
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestCharCaseConversionGermanSS tests German sharp S case conversion.
// char-foldcase uses simple (1:1) folding per R7RS §6.6, while
// string-foldcase uses full folding (ß -> "ss") per R7RS §6.7.
func TestCharCaseConversionGermanSS(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// char-downcase: U+1E9E (capital sharp S) -> U+00DF (lowercase sharp s)
		{
			name: "downcase capital sharp S to lowercase",
			prog: values.List(values.NewSymbol("char-downcase"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.NewCharacter('ß'),                                                 // U+00DF
		},
		// char-upcase: U+00DF (lowercase sharp s) -> unchanged (no 1:1 uppercase)
		// Go's unicode.ToUpper('ß') returns 'ß' unchanged.
		{
			name: "upcase lowercase sharp s stays unchanged",
			prog: values.List(values.NewSymbol("char-upcase"), values.NewCharacter('ß')), // U+00DF
			out:  values.NewCharacter('ß'),                                               // U+00DF
		},
		// char-foldcase: U+1E9E -> U+00DF (simple fold via simpleCaseFold)
		{
			name: "foldcase capital sharp S to lowercase",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.NewCharacter('ß'),                                                 // U+00DF
		},
		// char-foldcase: U+00DF stays unchanged (simple fold)
		{
			name: "foldcase lowercase sharp s stays unchanged",
			prog: values.List(values.NewSymbol("char-foldcase"), values.NewCharacter('ß')), // U+00DF
			out:  values.NewCharacter('ß'),                                                 // U+00DF
		},
		// Predicate checks
		{
			name: "lowercase sharp s is lowercase",
			prog: values.List(values.NewSymbol("char-lower-case?"), values.NewCharacter('ß')), // U+00DF
			out:  values.TrueValue,
		},
		{
			name: "capital sharp S is uppercase",
			prog: values.List(values.NewSymbol("char-upper-case?"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.TrueValue,
		},
		{
			name: "lowercase sharp s is alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ß')), // U+00DF
			out:  values.TrueValue,
		},
		{
			name: "capital sharp S is alphabetic",
			prog: values.List(values.NewSymbol("char-alphabetic?"), values.NewCharacter('ẞ')), // U+1E9E
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
