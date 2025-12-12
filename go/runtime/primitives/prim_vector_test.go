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

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestVectorLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector-length",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.NewInteger(3),
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

func TestVectorRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector-ref first",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				values.NewInteger(0)),
			out: values.NewInteger(1),
		},
		{
			name: "vector-ref last",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				values.NewInteger(2)),
			out: values.NewInteger(3),
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

func TestMakeVector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "make-vector without fill",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(5))),
			out: values.NewInteger(5),
		},
		{
			name: "make-vector with fill",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(3), values.NewInteger(42)),
				values.NewInteger(0)),
			out: values.NewInteger(42),
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

func TestVectorToList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector->list",
			prog: values.List(values.NewSymbol("vector->list"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
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

func TestListToVector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list->vector length",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("list->vector"),
					values.List(values.NewSymbol("quote"),
						values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))),
			out: values.NewInteger(3),
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
