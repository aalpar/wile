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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
