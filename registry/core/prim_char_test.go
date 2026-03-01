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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestCharComparisons(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// char=?
		{
			name: "char=? true",
			prog: values.List(values.NewSymbol("char=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char=? false",
			prog: values.List(values.NewSymbol("char=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
		// char<?
		{
			name: "char<? true",
			prog: values.List(values.NewSymbol("char<?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.TrueValue,
		},
		{
			name: "char<? false",
			prog: values.List(values.NewSymbol("char<?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		// char>?
		{
			name: "char>? true",
			prog: values.List(values.NewSymbol("char>?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>? false",
			prog: values.List(values.NewSymbol("char>?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
		// char<=?
		{
			name: "char<=? true equal",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char<=? true less",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.TrueValue,
		},
		{
			name: "char<=? false",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		// char>=?
		{
			name: "char>=? true equal",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>=? true greater",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>=? false",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestCharToInteger(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char->integer lowercase a",
			prog: values.List(values.NewSymbol("char->integer"), values.NewCharacter('a')),
			out:  values.NewInteger(97),
		},
		{
			name: "char->integer space",
			prog: values.List(values.NewSymbol("char->integer"), values.NewCharacter(' ')),
			out:  values.NewInteger(32),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestIntegerToChar(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "integer->char 97",
			prog: values.List(values.NewSymbol("integer->char"), values.NewInteger(97)),
			out:  values.NewCharacter('a'),
		},
		{
			name: "integer->char 32",
			prog: values.List(values.NewSymbol("integer->char"), values.NewInteger(32)),
			out:  values.NewCharacter(' '),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// Extended character conversion tests

func TestCharToIntegerExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char->integer uppercase A",
			Code:     `(char->integer #\A)`,
			Expected: values.NewInteger(65),
		},
		{
			Name:     "char->integer uppercase Z",
			Code:     `(char->integer #\Z)`,
			Expected: values.NewInteger(90),
		},
		{
			Name:     "char->integer lowercase z",
			Code:     `(char->integer #\z)`,
			Expected: values.NewInteger(122),
		},
		{
			Name:     "char->integer digit 0",
			Code:     `(char->integer #\0)`,
			Expected: values.NewInteger(48),
		},
		{
			Name:     "char->integer newline",
			Code:     `(char->integer #\newline)`,
			Expected: values.NewInteger(10),
		},
		{
			Name:     "char->integer tab",
			Code:     `(char->integer #\tab)`,
			Expected: values.NewInteger(9),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestIntegerToCharExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "integer->char 65 (A)",
			Code:     `(integer->char 65)`,
			Expected: values.NewCharacter('A'),
		},
		{
			Name:     "integer->char 122 (z)",
			Code:     `(integer->char 122)`,
			Expected: values.NewCharacter('z'),
		},
		{
			Name:     "integer->char 48 (0)",
			Code:     `(integer->char 48)`,
			Expected: values.NewCharacter('0'),
		},
		{
			Name:     "integer->char 10 (newline)",
			Code:     `(integer->char 10)`,
			Expected: values.NewCharacter('\n'),
		},
		{
			Name:     "integer->char 9 (tab)",
			Code:     `(integer->char 9)`,
			Expected: values.NewCharacter('\t'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// Case-insensitive character comparison tests

func TestCharCIEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char-ci=? same case",
			Code:     `(char-ci=? #\a #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci=? different case lowercase first",
			Code:     `(char-ci=? #\a #\A)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci=? different case uppercase first",
			Code:     `(char-ci=? #\A #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci=? different chars",
			Code:     `(char-ci=? #\a #\b)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char-ci=? digits",
			Code:     `(char-ci=? #\5 #\5)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci=? space",
			Code:     `(char-ci=? #\space #\space)`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCharCILessThan(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char-ci<? less",
			Code:     `(char-ci<? #\a #\b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<? less case insensitive",
			Code:     `(char-ci<? #\A #\b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<? equal different case",
			Code:     `(char-ci<? #\A #\a)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char-ci<? greater",
			Code:     `(char-ci<? #\z #\A)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCharCIGreaterThan(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char-ci>? greater",
			Code:     `(char-ci>? #\b #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>? greater case insensitive",
			Code:     `(char-ci>? #\B #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>? equal different case",
			Code:     `(char-ci>? #\A #\a)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char-ci>? less",
			Code:     `(char-ci>? #\a #\Z)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCharCILessThanOrEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char-ci<=? less",
			Code:     `(char-ci<=? #\a #\b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<=? equal same case",
			Code:     `(char-ci<=? #\a #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<=? equal different case",
			Code:     `(char-ci<=? #\A #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<=? greater",
			Code:     `(char-ci<=? #\z #\a)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCharCIGreaterThanOrEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char-ci>=? greater",
			Code:     `(char-ci>=? #\b #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>=? equal same case",
			Code:     `(char-ci>=? #\a #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>=? equal different case",
			Code:     `(char-ci>=? #\A #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>=? less",
			Code:     `(char-ci>=? #\a #\z)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// Variadic case-insensitive character comparison tests (R7RS requires 2+ args)
// NOTE: These tests will fail until char-ci comparisons are made variadic per R7RS

func TestCharCICompareVariadic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// char-ci=? variadic
		{
			Name:     "char-ci=? three equal mixed case",
			Code:     `(char-ci=? #\a #\A #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci=? three one different",
			Code:     `(char-ci=? #\a #\A #\b)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char-ci=? four equal",
			Code:     `(char-ci=? #\Z #\z #\Z #\z)`,
			Expected: values.TrueValue,
		},
		// char-ci<? variadic
		{
			Name:     "char-ci<? three ascending mixed case",
			Code:     `(char-ci<? #\A #\b #\C)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<? three not ascending",
			Code:     `(char-ci<? #\a #\C #\b)`,
			Expected: values.FalseValue,
		},
		// char-ci>? variadic
		{
			Name:     "char-ci>? three descending mixed case",
			Code:     `(char-ci>? #\C #\b #\A)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>? three not descending",
			Code:     `(char-ci>? #\c #\a #\B)`,
			Expected: values.FalseValue,
		},
		// char-ci<=? variadic
		{
			Name:     "char-ci<=? three non-decreasing with equal",
			Code:     `(char-ci<=? #\a #\A #\b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci<=? three decreasing",
			Code:     `(char-ci<=? #\c #\B #\a)`,
			Expected: values.FalseValue,
		},
		// char-ci>=? variadic
		{
			Name:     "char-ci>=? three non-increasing with equal",
			Code:     `(char-ci>=? #\b #\A #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-ci>=? three increasing",
			Code:     `(char-ci>=? #\a #\B #\c)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// Variadic character comparison tests

func TestCharCompareVariadic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "char=? three equal",
			Code:     `(char=? #\a #\a #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char=? three one different",
			Code:     `(char=? #\a #\a #\b)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char<? three ascending",
			Code:     `(char<? #\a #\b #\c)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char<? three not ascending",
			Code:     `(char<? #\a #\c #\b)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "char>? three descending",
			Code:     `(char>? #\c #\b #\a)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char<=? three non-decreasing equal",
			Code:     `(char<=? #\a #\a #\b)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char>=? three non-increasing equal",
			Code:     `(char>=? #\b #\a #\a)`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// Error condition tests for character primitives per R7RS

func TestCharCompareErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// char=? errors
		{Name: "char=? with integer", Code: `(char=? #\a 42)`},
		{Name: "char=? with string", Code: `(char=? "a" #\a)`},
		{Name: "char=? with symbol", Code: `(char=? 'a #\a)`},
		// char<? errors
		{Name: "char<? with integer", Code: `(char<? #\a 42)`},
		{Name: "char<? with string", Code: `(char<? #\a "b")`},
		// char>? errors
		{Name: "char>? with integer", Code: `(char>? 42 #\a)`},
		{Name: "char>? with list", Code: `(char>? #\a '())`},
		// char<=? errors
		{Name: "char<=? with integer", Code: `(char<=? #\a 42)`},
		{Name: "char<=? with boolean", Code: `(char<=? #t #\a)`},
		// char>=? errors
		{Name: "char>=? with integer", Code: `(char>=? #\a 42)`},
		{Name: "char>=? with string", Code: `(char>=? #\a "a")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestCharCICompareErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// char-ci=? errors
		{Name: "char-ci=? with integer", Code: `(char-ci=? #\a 42)`},
		{Name: "char-ci=? with string", Code: `(char-ci=? "A" #\a)`},
		// char-ci<? errors
		{Name: "char-ci<? with integer", Code: `(char-ci<? #\a 42)`},
		{Name: "char-ci<? with symbol", Code: `(char-ci<? #\a 'b)`},
		// char-ci>? errors
		{Name: "char-ci>? with integer", Code: `(char-ci>? 42 #\a)`},
		{Name: "char-ci>? with list", Code: `(char-ci>? #\a '(a))`},
		// char-ci<=? errors
		{Name: "char-ci<=? with integer", Code: `(char-ci<=? #\a 42)`},
		{Name: "char-ci<=? with string", Code: `(char-ci<=? #\a "A")`},
		// char-ci>=? errors
		{Name: "char-ci>=? with integer", Code: `(char-ci>=? #\a 42)`},
		{Name: "char-ci>=? with boolean", Code: `(char-ci>=? #\a #f)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestCharPredicateErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// char-alphabetic? errors
		{Name: "char-alphabetic? with integer", Code: `(char-alphabetic? 65)`},
		{Name: "char-alphabetic? with string", Code: `(char-alphabetic? "a")`},
		// char-numeric? errors
		{Name: "char-numeric? with integer", Code: `(char-numeric? 5)`},
		{Name: "char-numeric? with string", Code: `(char-numeric? "5")`},
		// char-whitespace? errors
		{Name: "char-whitespace? with integer", Code: `(char-whitespace? 32)`},
		{Name: "char-whitespace? with string", Code: `(char-whitespace? " ")`},
		// char-upper-case? errors
		{Name: "char-upper-case? with integer", Code: `(char-upper-case? 65)`},
		{Name: "char-upper-case? with string", Code: `(char-upper-case? "A")`},
		// char-lower-case? errors
		{Name: "char-lower-case? with integer", Code: `(char-lower-case? 97)`},
		{Name: "char-lower-case? with string", Code: `(char-lower-case? "a")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestCharConversionErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// char->integer errors
		{Name: "char->integer with integer", Code: `(char->integer 65)`},
		{Name: "char->integer with string", Code: `(char->integer "a")`},
		{Name: "char->integer with symbol", Code: `(char->integer 'a)`},
		// integer->char type errors
		{Name: "integer->char with character", Code: `(integer->char #\a)`},
		{Name: "integer->char with string", Code: `(integer->char "65")`},
		// integer->char range errors (R7RS §6.6)
		{Name: "integer->char negative", Code: `(integer->char -1)`},
		{Name: "integer->char surrogate low", Code: `(integer->char #xD800)`},
		{Name: "integer->char surrogate mid", Code: `(integer->char #xDB00)`},
		{Name: "integer->char surrogate high", Code: `(integer->char #xDFFF)`},
		{Name: "integer->char above max", Code: `(integer->char #x110000)`},
		{Name: "integer->char large value", Code: `(integer->char 2000000)`},
		// char-upcase errors
		{Name: "char-upcase with integer", Code: `(char-upcase 97)`},
		{Name: "char-upcase with string", Code: `(char-upcase "a")`},
		// char-downcase errors
		{Name: "char-downcase with integer", Code: `(char-downcase 65)`},
		{Name: "char-downcase with string", Code: `(char-downcase "A")`},
		// char-foldcase errors
		{Name: "char-foldcase with integer", Code: `(char-foldcase 65)`},
		{Name: "char-foldcase with string", Code: `(char-foldcase "A")`},
		// digit-value errors
		{Name: "digit-value with integer", Code: `(digit-value 5)`},
		{Name: "digit-value with string", Code: `(digit-value "5")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// Edge case tests for integer->char (R7RS §6.6 Unicode scalar value boundaries)

func TestIntegerToChar_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "null char (0)",
			Code:     `(char->integer (integer->char 0))`,
			Expected: values.NewInteger(0),
		},
		{
			Name:     "just below surrogate (#xD7FF)",
			Code:     `(char->integer (integer->char #xD7FF))`,
			Expected: values.NewInteger(55295),
		},
		{
			Name:     "just above surrogate (#xE000)",
			Code:     `(char->integer (integer->char #xE000))`,
			Expected: values.NewInteger(57344),
		},
		{
			Name:     "max code point (#x10FFFF)",
			Code:     `(char->integer (integer->char #x10FFFF))`,
			Expected: values.NewInteger(1114111),
		},
		{
			Name:     "lambda (955)",
			Code:     `(char->integer (integer->char 955))`,
			Expected: values.NewInteger(955),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// Edge case tests for char->integer (high Unicode code points)

func TestCharToInteger_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "null char",
			Code:     `(char->integer #\null)`,
			Expected: values.NewInteger(0),
		},
		{
			Name:     "lambda round-trip",
			Code:     `(integer->char (char->integer #\λ))`,
			Expected: values.NewCharacter('λ'),
		},
		{
			Name:     "high code point round-trip",
			Code:     `(char->integer (integer->char #x10FFFF))`,
			Expected: values.NewInteger(1114111),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
