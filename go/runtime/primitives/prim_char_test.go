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

package primitives_test

import (
	"testing"

	"wile/values"

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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// Extended character conversion tests

func TestCharToIntegerExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char->integer uppercase A",
			code:     `(char->integer #\A)`,
			expected: values.NewInteger(65),
		},
		{
			name:     "char->integer uppercase Z",
			code:     `(char->integer #\Z)`,
			expected: values.NewInteger(90),
		},
		{
			name:     "char->integer lowercase z",
			code:     `(char->integer #\z)`,
			expected: values.NewInteger(122),
		},
		{
			name:     "char->integer digit 0",
			code:     `(char->integer #\0)`,
			expected: values.NewInteger(48),
		},
		{
			name:     "char->integer newline",
			code:     `(char->integer #\newline)`,
			expected: values.NewInteger(10),
		},
		{
			name:     "char->integer tab",
			code:     `(char->integer #\tab)`,
			expected: values.NewInteger(9),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestIntegerToCharExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "integer->char 65 (A)",
			code:     `(integer->char 65)`,
			expected: values.NewCharacter('A'),
		},
		{
			name:     "integer->char 122 (z)",
			code:     `(integer->char 122)`,
			expected: values.NewCharacter('z'),
		},
		{
			name:     "integer->char 48 (0)",
			code:     `(integer->char 48)`,
			expected: values.NewCharacter('0'),
		},
		{
			name:     "integer->char 10 (newline)",
			code:     `(integer->char 10)`,
			expected: values.NewCharacter('\n'),
		},
		{
			name:     "integer->char 9 (tab)",
			code:     `(integer->char 9)`,
			expected: values.NewCharacter('\t'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// Case-insensitive character comparison tests

func TestCharCIEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char-ci=? same case",
			code:     `(char-ci=? #\a #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci=? different case lowercase first",
			code:     `(char-ci=? #\a #\A)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci=? different case uppercase first",
			code:     `(char-ci=? #\A #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci=? different chars",
			code:     `(char-ci=? #\a #\b)`,
			expected: values.FalseValue,
		},
		{
			name:     "char-ci=? digits",
			code:     `(char-ci=? #\5 #\5)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci=? space",
			code:     `(char-ci=? #\space #\space)`,
			expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCharCILessThan(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char-ci<? less",
			code:     `(char-ci<? #\a #\b)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<? less case insensitive",
			code:     `(char-ci<? #\A #\b)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<? equal different case",
			code:     `(char-ci<? #\A #\a)`,
			expected: values.FalseValue,
		},
		{
			name:     "char-ci<? greater",
			code:     `(char-ci<? #\z #\A)`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCharCIGreaterThan(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char-ci>? greater",
			code:     `(char-ci>? #\b #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>? greater case insensitive",
			code:     `(char-ci>? #\B #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>? equal different case",
			code:     `(char-ci>? #\A #\a)`,
			expected: values.FalseValue,
		},
		{
			name:     "char-ci>? less",
			code:     `(char-ci>? #\a #\Z)`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCharCILessThanOrEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char-ci<=? less",
			code:     `(char-ci<=? #\a #\b)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<=? equal same case",
			code:     `(char-ci<=? #\a #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<=? equal different case",
			code:     `(char-ci<=? #\A #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<=? greater",
			code:     `(char-ci<=? #\z #\a)`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCharCIGreaterThanOrEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char-ci>=? greater",
			code:     `(char-ci>=? #\b #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>=? equal same case",
			code:     `(char-ci>=? #\a #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>=? equal different case",
			code:     `(char-ci>=? #\A #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>=? less",
			code:     `(char-ci>=? #\a #\z)`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// Variadic case-insensitive character comparison tests (R7RS requires 2+ args)
// NOTE: These tests will fail until char-ci comparisons are made variadic per R7RS

func TestCharCICompareVariadic(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// char-ci=? variadic
		{
			name:     "char-ci=? three equal mixed case",
			code:     `(char-ci=? #\a #\A #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci=? three one different",
			code:     `(char-ci=? #\a #\A #\b)`,
			expected: values.FalseValue,
		},
		{
			name:     "char-ci=? four equal",
			code:     `(char-ci=? #\Z #\z #\Z #\z)`,
			expected: values.TrueValue,
		},
		// char-ci<? variadic
		{
			name:     "char-ci<? three ascending mixed case",
			code:     `(char-ci<? #\A #\b #\C)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<? three not ascending",
			code:     `(char-ci<? #\a #\C #\b)`,
			expected: values.FalseValue,
		},
		// char-ci>? variadic
		{
			name:     "char-ci>? three descending mixed case",
			code:     `(char-ci>? #\C #\b #\A)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>? three not descending",
			code:     `(char-ci>? #\c #\a #\B)`,
			expected: values.FalseValue,
		},
		// char-ci<=? variadic
		{
			name:     "char-ci<=? three non-decreasing with equal",
			code:     `(char-ci<=? #\a #\A #\b)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci<=? three decreasing",
			code:     `(char-ci<=? #\c #\B #\a)`,
			expected: values.FalseValue,
		},
		// char-ci>=? variadic
		{
			name:     "char-ci>=? three non-increasing with equal",
			code:     `(char-ci>=? #\b #\A #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-ci>=? three increasing",
			code:     `(char-ci>=? #\a #\B #\c)`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// Variadic character comparison tests

func TestCharCompareVariadic(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "char=? three equal",
			code:     `(char=? #\a #\a #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char=? three one different",
			code:     `(char=? #\a #\a #\b)`,
			expected: values.FalseValue,
		},
		{
			name:     "char<? three ascending",
			code:     `(char<? #\a #\b #\c)`,
			expected: values.TrueValue,
		},
		{
			name:     "char<? three not ascending",
			code:     `(char<? #\a #\c #\b)`,
			expected: values.FalseValue,
		},
		{
			name:     "char>? three descending",
			code:     `(char>? #\c #\b #\a)`,
			expected: values.TrueValue,
		},
		{
			name:     "char<=? three non-decreasing equal",
			code:     `(char<=? #\a #\a #\b)`,
			expected: values.TrueValue,
		},
		{
			name:     "char>=? three non-increasing equal",
			code:     `(char>=? #\b #\a #\a)`,
			expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
