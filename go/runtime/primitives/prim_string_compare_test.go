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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
