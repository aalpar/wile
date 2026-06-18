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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// Case-insensitive string comparison tests

func TestStringCIEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ci=? equal same case",
			Code:     `(string-ci=? "hello" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci=? equal different case",
			Code:     `(string-ci=? "Hello" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci=? equal all caps",
			Code:     `(string-ci=? "HELLO" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci=? different strings",
			Code:     `(string-ci=? "hello" "world")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci=? empty strings",
			Code:     `(string-ci=? "" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci=? different lengths",
			Code:     `(string-ci=? "AB" "ABC")`,
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

func TestStringCILessThan(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ci<? less",
			Code:     `(string-ci<? "abc" "def")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<? less case insensitive",
			Code:     `(string-ci<? "ABC" "def")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<? equal",
			Code:     `(string-ci<? "Hello" "hello")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci<? greater",
			Code:     `(string-ci<? "xyz" "ABC")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci<? prefix less",
			Code:     `(string-ci<? "A" "AB")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<? empty less than non-empty",
			Code:     `(string-ci<? "" "a")`,
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

func TestStringCIGreaterThan(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ci>? greater",
			Code:     `(string-ci>? "def" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>? greater case insensitive",
			Code:     `(string-ci>? "DEF" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>? equal",
			Code:     `(string-ci>? "Hello" "hello")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci>? less",
			Code:     `(string-ci>? "abc" "XYZ")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci>? non-empty greater than empty",
			Code:     `(string-ci>? "a" "")`,
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

func TestStringCILessThanOrEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ci<=? equal same case",
			Code:     `(string-ci<=? "hello" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<=? equal different case",
			Code:     `(string-ci<=? "HELLO" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<=? less",
			Code:     `(string-ci<=? "abc" "DEF")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<=? greater",
			Code:     `(string-ci<=? "XYZ" "abc")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci<=? empty strings",
			Code:     `(string-ci<=? "" "")`,
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

func TestStringCIGreaterThanOrEqual(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ci>=? equal same case",
			Code:     `(string-ci>=? "hello" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>=? equal different case",
			Code:     `(string-ci>=? "HELLO" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>=? greater",
			Code:     `(string-ci>=? "xyz" "ABC")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>=? less",
			Code:     `(string-ci>=? "abc" "XYZ")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci>=? empty strings",
			Code:     `(string-ci>=? "" "")`,
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

// Variadic case-insensitive string comparison tests (R7RS requires 2+ args)
// NOTE: These tests will fail until string-ci comparisons are made variadic per R7RS

func TestStringCICompareVariadic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// string-ci=? variadic
		{
			Name:     "string-ci=? three equal mixed case",
			Code:     `(string-ci=? "abc" "ABC" "Abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci=? three one different",
			Code:     `(string-ci=? "abc" "ABC" "def")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string-ci=? four equal",
			Code:     `(string-ci=? "Hello" "HELLO" "hello" "hElLo")`,
			Expected: values.TrueValue,
		},
		// string-ci<? variadic
		{
			Name:     "string-ci<? three ascending mixed case",
			Code:     `(string-ci<? "ABC" "def" "GHI")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<? three not ascending",
			Code:     `(string-ci<? "abc" "GHI" "def")`,
			Expected: values.FalseValue,
		},
		// string-ci>? variadic
		{
			Name:     "string-ci>? three descending mixed case",
			Code:     `(string-ci>? "XYZ" "mno" "ABC")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>? three not descending",
			Code:     `(string-ci>? "xyz" "abc" "MNO")`,
			Expected: values.FalseValue,
		},
		// string-ci<=? variadic
		{
			Name:     "string-ci<=? three non-decreasing with equal",
			Code:     `(string-ci<=? "abc" "ABC" "def")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci<=? three decreasing",
			Code:     `(string-ci<=? "xyz" "MNO" "abc")`,
			Expected: values.FalseValue,
		},
		// string-ci>=? variadic
		{
			Name:     "string-ci>=? three non-increasing with equal",
			Code:     `(string-ci>=? "xyz" "MNO" "mno")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string-ci>=? three increasing",
			Code:     `(string-ci>=? "abc" "MNO" "xyz")`,
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

// Binary string comparison tests using runSchemeCode
// These tests exercise the actual registered string comparison primitives

func TestStringEqualScheme(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic equality
		{
			Name:     "equal strings",
			Code:     `(string=? "hello" "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "different strings",
			Code:     `(string=? "hello" "world")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "empty strings equal",
			Code:     `(string=? "" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "case sensitive different",
			Code:     `(string=? "Hello" "hello")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "different lengths",
			Code:     `(string=? "ab" "abc")`,
			Expected: values.FalseValue,
		},
		// Unicode
		{
			Name:     "unicode equal",
			Code:     `(string=? "café" "café")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "unicode different",
			Code:     `(string=? "café" "cafe")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "Chinese strings equal",
			Code:     `(string=? "你好" "你好")`,
			Expected: values.TrueValue,
		},
		// Edge cases
		{
			Name:     "single char equal",
			Code:     `(string=? "a" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "single char different",
			Code:     `(string=? "a" "b")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "empty vs non-empty",
			Code:     `(string=? "" "a")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "whitespace matters",
			Code:     `(string=? "a b" "a  b")`,
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

func TestStringLessThanScheme(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic ordering
		{
			Name:     "lexicographic less",
			Code:     `(string<? "abc" "abd")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "lexicographic greater",
			Code:     `(string<? "abd" "abc")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "equal strings",
			Code:     `(string<? "abc" "abc")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "prefix is less",
			Code:     `(string<? "ab" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "longer is not less than prefix",
			Code:     `(string<? "abc" "ab")`,
			Expected: values.FalseValue,
		},
		// Case sensitivity (ASCII order: A-Z < a-z)
		{
			Name:     "uppercase less than lowercase",
			Code:     `(string<? "A" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "Z less than a",
			Code:     `(string<? "Z" "a")`,
			Expected: values.TrueValue,
		},
		// Empty strings
		{
			Name:     "empty less than non-empty",
			Code:     `(string<? "" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "non-empty not less than empty",
			Code:     `(string<? "a" "")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "empty not less than empty",
			Code:     `(string<? "" "")`,
			Expected: values.FalseValue,
		},
		// Unicode
		{
			Name:     "unicode ordering",
			Code:     `(string<? "a" "α")`,
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

func TestStringGreaterThanScheme(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic ordering
		{
			Name:     "lexicographic greater",
			Code:     `(string>? "abd" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "lexicographic less",
			Code:     `(string>? "abc" "abd")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "equal strings",
			Code:     `(string>? "abc" "abc")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "longer is greater than prefix",
			Code:     `(string>? "abc" "ab")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "prefix is not greater",
			Code:     `(string>? "ab" "abc")`,
			Expected: values.FalseValue,
		},
		// Case sensitivity
		{
			Name:     "lowercase greater than uppercase",
			Code:     `(string>? "a" "A")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "a greater than Z",
			Code:     `(string>? "a" "Z")`,
			Expected: values.TrueValue,
		},
		// Empty strings
		{
			Name:     "non-empty greater than empty",
			Code:     `(string>? "a" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "empty not greater than non-empty",
			Code:     `(string>? "" "a")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "empty not greater than empty",
			Code:     `(string>? "" "")`,
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

func TestStringLessOrEqualScheme(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic ordering
		{
			Name:     "less than",
			Code:     `(string<=? "abc" "abd")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "equal",
			Code:     `(string<=? "abc" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "greater than",
			Code:     `(string<=? "abd" "abc")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "prefix less or equal",
			Code:     `(string<=? "ab" "abc")`,
			Expected: values.TrueValue,
		},
		// Empty strings
		{
			Name:     "empty less or equal to empty",
			Code:     `(string<=? "" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "empty less or equal to non-empty",
			Code:     `(string<=? "" "a")`,
			Expected: values.TrueValue,
		},
		// Case sensitivity
		{
			Name:     "uppercase less or equal lowercase",
			Code:     `(string<=? "A" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "same case equal",
			Code:     `(string<=? "hello" "hello")`,
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

func TestStringGreaterOrEqualScheme(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic ordering
		{
			Name:     "greater than",
			Code:     `(string>=? "abd" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "equal",
			Code:     `(string>=? "abc" "abc")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "less than",
			Code:     `(string>=? "abc" "abd")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "longer greater or equal to prefix",
			Code:     `(string>=? "abc" "ab")`,
			Expected: values.TrueValue,
		},
		// Empty strings
		{
			Name:     "empty greater or equal to empty",
			Code:     `(string>=? "" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "non-empty greater or equal to empty",
			Code:     `(string>=? "a" "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "empty not greater or equal to non-empty",
			Code:     `(string>=? "" "a")`,
			Expected: values.FalseValue,
		},
		// Case sensitivity
		{
			Name:     "lowercase greater or equal uppercase",
			Code:     `(string>=? "a" "A")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "same case equal",
			Code:     `(string>=? "hello" "hello")`,
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

// Variadic string comparison tests

func TestStringCompareVariadic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string=? three equal strings",
			Code:     `(string=? "a" "a" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string=? three strings one different",
			Code:     `(string=? "a" "a" "b")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string<? three ascending",
			Code:     `(string<? "a" "b" "c")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string<? three not ascending",
			Code:     `(string<? "a" "c" "b")`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string>? three descending",
			Code:     `(string>? "c" "b" "a")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string<=? three non-decreasing equal",
			Code:     `(string<=? "a" "a" "b")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string>=? three non-increasing equal",
			Code:     `(string>=? "b" "a" "a")`,
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
