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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
