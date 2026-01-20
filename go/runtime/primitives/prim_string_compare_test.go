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

func TestStringEqual(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string=? equal strings",
			prog: values.List(values.NewSymbol("string=?"), values.NewString("hello"), values.NewString("hello")),
			out:  values.TrueValue,
		},
		{
			name: "string=? different strings",
			prog: values.List(values.NewSymbol("string=?"), values.NewString("hello"), values.NewString("world")),
			out:  values.FalseValue,
		},
		{
			name: "string=? empty strings",
			prog: values.List(values.NewSymbol("string=?"), values.NewString(""), values.NewString("")),
			out:  values.TrueValue,
		},
		{
			name: "string=? case sensitive",
			prog: values.List(values.NewSymbol("string=?"), values.NewString("Hello"), values.NewString("hello")),
			out:  values.FalseValue,
		},
		{
			name: "string=? different lengths",
			prog: values.List(values.NewSymbol("string=?"), values.NewString("ab"), values.NewString("abc")),
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

func TestStringLessThan(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string<? lexicographic less",
			prog: values.List(values.NewSymbol("string<?"), values.NewString("abc"), values.NewString("abd")),
			out:  values.TrueValue,
		},
		{
			name: "string<? prefix less",
			prog: values.List(values.NewSymbol("string<?"), values.NewString("a"), values.NewString("ab")),
			out:  values.TrueValue,
		},
		{
			name: "string<? equal strings",
			prog: values.List(values.NewSymbol("string<?"), values.NewString("hello"), values.NewString("hello")),
			out:  values.FalseValue,
		},
		{
			name: "string<? greater",
			prog: values.List(values.NewSymbol("string<?"), values.NewString("abd"), values.NewString("abc")),
			out:  values.FalseValue,
		},
		{
			name: "string<? empty and non-empty",
			prog: values.List(values.NewSymbol("string<?"), values.NewString(""), values.NewString("a")),
			out:  values.TrueValue,
		},
		{
			name: "string<? case sensitive",
			prog: values.List(values.NewSymbol("string<?"), values.NewString("A"), values.NewString("a")),
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

func TestStringGreaterThan(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string>? lexicographic greater",
			prog: values.List(values.NewSymbol("string>?"), values.NewString("abd"), values.NewString("abc")),
			out:  values.TrueValue,
		},
		{
			name: "string>? longer greater",
			prog: values.List(values.NewSymbol("string>?"), values.NewString("ab"), values.NewString("a")),
			out:  values.TrueValue,
		},
		{
			name: "string>? equal strings",
			prog: values.List(values.NewSymbol("string>?"), values.NewString("hello"), values.NewString("hello")),
			out:  values.FalseValue,
		},
		{
			name: "string>? less",
			prog: values.List(values.NewSymbol("string>?"), values.NewString("abc"), values.NewString("abd")),
			out:  values.FalseValue,
		},
		{
			name: "string>? non-empty and empty",
			prog: values.List(values.NewSymbol("string>?"), values.NewString("a"), values.NewString("")),
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

func TestStringLessThanOrEqual(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string<=? equal strings",
			prog: values.List(values.NewSymbol("string<=?"), values.NewString("hello"), values.NewString("hello")),
			out:  values.TrueValue,
		},
		{
			name: "string<=? less",
			prog: values.List(values.NewSymbol("string<=?"), values.NewString("abc"), values.NewString("abd")),
			out:  values.TrueValue,
		},
		{
			name: "string<=? prefix less",
			prog: values.List(values.NewSymbol("string<=?"), values.NewString("a"), values.NewString("ab")),
			out:  values.TrueValue,
		},
		{
			name: "string<=? greater",
			prog: values.List(values.NewSymbol("string<=?"), values.NewString("abd"), values.NewString("abc")),
			out:  values.FalseValue,
		},
		{
			name: "string<=? empty strings",
			prog: values.List(values.NewSymbol("string<=?"), values.NewString(""), values.NewString("")),
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

func TestStringGreaterThanOrEqual(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string>=? equal strings",
			prog: values.List(values.NewSymbol("string>=?"), values.NewString("hello"), values.NewString("hello")),
			out:  values.TrueValue,
		},
		{
			name: "string>=? greater",
			prog: values.List(values.NewSymbol("string>=?"), values.NewString("abd"), values.NewString("abc")),
			out:  values.TrueValue,
		},
		{
			name: "string>=? longer greater",
			prog: values.List(values.NewSymbol("string>=?"), values.NewString("ab"), values.NewString("a")),
			out:  values.TrueValue,
		},
		{
			name: "string>=? less",
			prog: values.List(values.NewSymbol("string>=?"), values.NewString("abc"), values.NewString("abd")),
			out:  values.FalseValue,
		},
		{
			name: "string>=? empty strings",
			prog: values.List(values.NewSymbol("string>=?"), values.NewString(""), values.NewString("")),
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

// Case-insensitive string comparison tests

func TestStringCIEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ci=? equal same case",
			code:     `(string-ci=? "hello" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci=? equal different case",
			code:     `(string-ci=? "Hello" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci=? equal all caps",
			code:     `(string-ci=? "HELLO" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci=? different strings",
			code:     `(string-ci=? "hello" "world")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci=? empty strings",
			code:     `(string-ci=? "" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci=? different lengths",
			code:     `(string-ci=? "AB" "ABC")`,
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

func TestStringCILessThan(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ci<? less",
			code:     `(string-ci<? "abc" "def")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<? less case insensitive",
			code:     `(string-ci<? "ABC" "def")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<? equal",
			code:     `(string-ci<? "Hello" "hello")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci<? greater",
			code:     `(string-ci<? "xyz" "ABC")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci<? prefix less",
			code:     `(string-ci<? "A" "AB")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<? empty less than non-empty",
			code:     `(string-ci<? "" "a")`,
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

func TestStringCIGreaterThan(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ci>? greater",
			code:     `(string-ci>? "def" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>? greater case insensitive",
			code:     `(string-ci>? "DEF" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>? equal",
			code:     `(string-ci>? "Hello" "hello")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci>? less",
			code:     `(string-ci>? "abc" "XYZ")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci>? non-empty greater than empty",
			code:     `(string-ci>? "a" "")`,
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

func TestStringCILessThanOrEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ci<=? equal same case",
			code:     `(string-ci<=? "hello" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<=? equal different case",
			code:     `(string-ci<=? "HELLO" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<=? less",
			code:     `(string-ci<=? "abc" "DEF")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<=? greater",
			code:     `(string-ci<=? "XYZ" "abc")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci<=? empty strings",
			code:     `(string-ci<=? "" "")`,
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

func TestStringCIGreaterThanOrEqual(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ci>=? equal same case",
			code:     `(string-ci>=? "hello" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>=? equal different case",
			code:     `(string-ci>=? "HELLO" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>=? greater",
			code:     `(string-ci>=? "xyz" "ABC")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>=? less",
			code:     `(string-ci>=? "abc" "XYZ")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci>=? empty strings",
			code:     `(string-ci>=? "" "")`,
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

// Variadic case-insensitive string comparison tests (R7RS requires 2+ args)
// NOTE: These tests will fail until string-ci comparisons are made variadic per R7RS

func TestStringCICompareVariadic(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// string-ci=? variadic
		{
			name:     "string-ci=? three equal mixed case",
			code:     `(string-ci=? "abc" "ABC" "Abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci=? three one different",
			code:     `(string-ci=? "abc" "ABC" "def")`,
			expected: values.FalseValue,
		},
		{
			name:     "string-ci=? four equal",
			code:     `(string-ci=? "Hello" "HELLO" "hello" "hElLo")`,
			expected: values.TrueValue,
		},
		// string-ci<? variadic
		{
			name:     "string-ci<? three ascending mixed case",
			code:     `(string-ci<? "ABC" "def" "GHI")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<? three not ascending",
			code:     `(string-ci<? "abc" "GHI" "def")`,
			expected: values.FalseValue,
		},
		// string-ci>? variadic
		{
			name:     "string-ci>? three descending mixed case",
			code:     `(string-ci>? "XYZ" "mno" "ABC")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>? three not descending",
			code:     `(string-ci>? "xyz" "abc" "MNO")`,
			expected: values.FalseValue,
		},
		// string-ci<=? variadic
		{
			name:     "string-ci<=? three non-decreasing with equal",
			code:     `(string-ci<=? "abc" "ABC" "def")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci<=? three decreasing",
			code:     `(string-ci<=? "xyz" "MNO" "abc")`,
			expected: values.FalseValue,
		},
		// string-ci>=? variadic
		{
			name:     "string-ci>=? three non-increasing with equal",
			code:     `(string-ci>=? "xyz" "MNO" "mno")`,
			expected: values.TrueValue,
		},
		{
			name:     "string-ci>=? three increasing",
			code:     `(string-ci>=? "abc" "MNO" "xyz")`,
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

// Variadic string comparison tests

func TestStringCompareVariadic(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string=? three equal strings",
			code:     `(string=? "a" "a" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "string=? three strings one different",
			code:     `(string=? "a" "a" "b")`,
			expected: values.FalseValue,
		},
		{
			name:     "string<? three ascending",
			code:     `(string<? "a" "b" "c")`,
			expected: values.TrueValue,
		},
		{
			name:     "string<? three not ascending",
			code:     `(string<? "a" "c" "b")`,
			expected: values.FalseValue,
		},
		{
			name:     "string>? three descending",
			code:     `(string>? "c" "b" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "string<=? three non-decreasing equal",
			code:     `(string<=? "a" "a" "b")`,
			expected: values.TrueValue,
		},
		{
			name:     "string>=? three non-increasing equal",
			code:     `(string>=? "b" "a" "a")`,
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
