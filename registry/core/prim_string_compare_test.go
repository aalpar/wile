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

// Binary string comparison tests using runSchemeCode
// These tests exercise the actual registered string comparison primitives

func TestStringEqualScheme(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic equality
		{
			name:     "equal strings",
			code:     `(string=? "hello" "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "different strings",
			code:     `(string=? "hello" "world")`,
			expected: values.FalseValue,
		},
		{
			name:     "empty strings equal",
			code:     `(string=? "" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "case sensitive different",
			code:     `(string=? "Hello" "hello")`,
			expected: values.FalseValue,
		},
		{
			name:     "different lengths",
			code:     `(string=? "ab" "abc")`,
			expected: values.FalseValue,
		},
		// Unicode
		{
			name:     "unicode equal",
			code:     `(string=? "café" "café")`,
			expected: values.TrueValue,
		},
		{
			name:     "unicode different",
			code:     `(string=? "café" "cafe")`,
			expected: values.FalseValue,
		},
		{
			name:     "Chinese strings equal",
			code:     `(string=? "你好" "你好")`,
			expected: values.TrueValue,
		},
		// Edge cases
		{
			name:     "single char equal",
			code:     `(string=? "a" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "single char different",
			code:     `(string=? "a" "b")`,
			expected: values.FalseValue,
		},
		{
			name:     "empty vs non-empty",
			code:     `(string=? "" "a")`,
			expected: values.FalseValue,
		},
		{
			name:     "whitespace matters",
			code:     `(string=? "a b" "a  b")`,
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

func TestStringLessThanScheme(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic ordering
		{
			name:     "lexicographic less",
			code:     `(string<? "abc" "abd")`,
			expected: values.TrueValue,
		},
		{
			name:     "lexicographic greater",
			code:     `(string<? "abd" "abc")`,
			expected: values.FalseValue,
		},
		{
			name:     "equal strings",
			code:     `(string<? "abc" "abc")`,
			expected: values.FalseValue,
		},
		{
			name:     "prefix is less",
			code:     `(string<? "ab" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "longer is not less than prefix",
			code:     `(string<? "abc" "ab")`,
			expected: values.FalseValue,
		},
		// Case sensitivity (ASCII order: A-Z < a-z)
		{
			name:     "uppercase less than lowercase",
			code:     `(string<? "A" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "Z less than a",
			code:     `(string<? "Z" "a")`,
			expected: values.TrueValue,
		},
		// Empty strings
		{
			name:     "empty less than non-empty",
			code:     `(string<? "" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "non-empty not less than empty",
			code:     `(string<? "a" "")`,
			expected: values.FalseValue,
		},
		{
			name:     "empty not less than empty",
			code:     `(string<? "" "")`,
			expected: values.FalseValue,
		},
		// Unicode
		{
			name:     "unicode ordering",
			code:     `(string<? "a" "α")`,
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

func TestStringGreaterThanScheme(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic ordering
		{
			name:     "lexicographic greater",
			code:     `(string>? "abd" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "lexicographic less",
			code:     `(string>? "abc" "abd")`,
			expected: values.FalseValue,
		},
		{
			name:     "equal strings",
			code:     `(string>? "abc" "abc")`,
			expected: values.FalseValue,
		},
		{
			name:     "longer is greater than prefix",
			code:     `(string>? "abc" "ab")`,
			expected: values.TrueValue,
		},
		{
			name:     "prefix is not greater",
			code:     `(string>? "ab" "abc")`,
			expected: values.FalseValue,
		},
		// Case sensitivity
		{
			name:     "lowercase greater than uppercase",
			code:     `(string>? "a" "A")`,
			expected: values.TrueValue,
		},
		{
			name:     "a greater than Z",
			code:     `(string>? "a" "Z")`,
			expected: values.TrueValue,
		},
		// Empty strings
		{
			name:     "non-empty greater than empty",
			code:     `(string>? "a" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "empty not greater than non-empty",
			code:     `(string>? "" "a")`,
			expected: values.FalseValue,
		},
		{
			name:     "empty not greater than empty",
			code:     `(string>? "" "")`,
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

func TestStringLessOrEqualScheme(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic ordering
		{
			name:     "less than",
			code:     `(string<=? "abc" "abd")`,
			expected: values.TrueValue,
		},
		{
			name:     "equal",
			code:     `(string<=? "abc" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "greater than",
			code:     `(string<=? "abd" "abc")`,
			expected: values.FalseValue,
		},
		{
			name:     "prefix less or equal",
			code:     `(string<=? "ab" "abc")`,
			expected: values.TrueValue,
		},
		// Empty strings
		{
			name:     "empty less or equal to empty",
			code:     `(string<=? "" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "empty less or equal to non-empty",
			code:     `(string<=? "" "a")`,
			expected: values.TrueValue,
		},
		// Case sensitivity
		{
			name:     "uppercase less or equal lowercase",
			code:     `(string<=? "A" "a")`,
			expected: values.TrueValue,
		},
		{
			name:     "same case equal",
			code:     `(string<=? "hello" "hello")`,
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

func TestStringGreaterOrEqualScheme(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic ordering
		{
			name:     "greater than",
			code:     `(string>=? "abd" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "equal",
			code:     `(string>=? "abc" "abc")`,
			expected: values.TrueValue,
		},
		{
			name:     "less than",
			code:     `(string>=? "abc" "abd")`,
			expected: values.FalseValue,
		},
		{
			name:     "longer greater or equal to prefix",
			code:     `(string>=? "abc" "ab")`,
			expected: values.TrueValue,
		},
		// Empty strings
		{
			name:     "empty greater or equal to empty",
			code:     `(string>=? "" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "non-empty greater or equal to empty",
			code:     `(string>=? "a" "")`,
			expected: values.TrueValue,
		},
		{
			name:     "empty not greater or equal to non-empty",
			code:     `(string>=? "" "a")`,
			expected: values.FalseValue,
		},
		// Case sensitivity
		{
			name:     "lowercase greater or equal uppercase",
			code:     `(string>=? "a" "A")`,
			expected: values.TrueValue,
		},
		{
			name:     "same case equal",
			code:     `(string>=? "hello" "hello")`,
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
