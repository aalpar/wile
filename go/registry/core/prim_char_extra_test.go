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

package core_test

import (
	"testing"

	"wile/values"

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
