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


package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestVectorCreation(t *testing.T) {
	tcs := []struct {
		name   string
		values []Value
		length int
	}{
		{
			name:   "empty vector",
			values: []Value{},
			length: 0,
		},
		{
			name:   "single element",
			values: []Value{NewInteger(1)},
			length: 1,
		},
		{
			name:   "two elements",
			values: []Value{NewInteger(1), NewInteger(2)},
			length: 2,
		},
		{
			name:   "three elements",
			values: []Value{NewInteger(1), NewInteger(2), NewInteger(3)},
			length: 3,
		},
		{
			name:   "mixed types",
			values: []Value{NewInteger(1), NewString("hello"), TrueValue},
			length: 3,
		},
		{
			name:   "nil element",
			values: []Value{nil},
			length: 1,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := NewVector(tc.values...)
			qt.Assert(t, v, qt.Not(qt.IsNil))
			qt.Assert(t, len(*v), qt.Equals, tc.length)
		})
	}
}

func TestVectorDatum(t *testing.T) {
	tcs := []struct {
		name   string
		in     *Vector
		isNil  bool
		length int
		values []Value
	}{
		{
			name:  "nil vector returns nil",
			in:    nil,
			isNil: true,
		},
		{
			name:   "empty vector returns empty slice",
			in:     NewVector(),
			length: 0,
		},
		{
			name:   "single element",
			in:     NewVector(NewInteger(42)),
			length: 1,
			values: []Value{NewInteger(42)},
		},
		{
			name:   "multiple elements",
			in:     NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
			length: 3,
			values: []Value{NewInteger(1), NewInteger(2), NewInteger(3)},
		},
		{
			name:   "mixed types",
			in:     NewVector(NewInteger(1), NewString("hello"), TrueValue),
			length: 3,
			values: []Value{NewInteger(1), NewString("hello"), TrueValue},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			datum := tc.in.Datum()
			if tc.isNil {
				qt.Assert(t, datum, qt.IsNil)
			} else {
				qt.Assert(t, datum, qt.HasLen, tc.length)
				for i, v := range tc.values {
					qt.Assert(t, datum[i], SchemeEquals, v)
				}
			}
		})
	}
}

func TestVectorIsVoid(t *testing.T) {
	tcs := []struct {
		name string
		in   *Vector
		out  bool
	}{
		{
			name: "nil vector is void",
			in:   nil,
			out:  true,
		},
		{
			name: "empty vector is not void",
			in:   NewVector(),
			out:  false,
		},
		{
			name: "single element vector is not void",
			in:   NewVector(NewInteger(1)),
			out:  false,
		},
		{
			name: "multiple element vector is not void",
			in:   NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.IsVoid(), qt.Equals, tc.out)
		})
	}
}

func TestVectorEqualTo(t *testing.T) {
	tcs := []struct {
		name string
		a    *Vector
		b    Value
		out  bool
	}{
		{
			name: "equal vectors same content",
			a:    NewVector(NewInteger(1), NewInteger(2)),
			b:    NewVector(NewInteger(1), NewInteger(2)),
			out:  true,
		},
		{
			name: "different content same length",
			a:    NewVector(NewInteger(1), NewInteger(2)),
			b:    NewVector(NewInteger(2), NewInteger(1)),
			out:  false,
		},
		{
			name: "different lengths",
			a:    NewVector(NewInteger(1), NewInteger(2)),
			b:    NewVector(NewInteger(1)),
			out:  false,
		},
		{
			name: "empty vectors equal",
			a:    NewVector(),
			b:    NewVector(),
			out:  true,
		},
		{
			name: "comparison with non-vector",
			a:    NewVector(NewInteger(1)),
			b:    NewInteger(1),
			out:  false,
		},
		{
			name: "comparison with nil vector",
			a:    NewVector(NewInteger(1)),
			b:    (*Vector)(nil),
			out:  false,
		},
		{
			name: "nil vectors equal",
			a:    nil,
			b:    (*Vector)(nil),
			out:  true,
		},
		{
			name: "single element equal",
			a:    NewVector(NewInteger(42)),
			b:    NewVector(NewInteger(42)),
			out:  true,
		},
		{
			name: "nested vectors equal",
			a:    NewVector(NewVector(NewInteger(1), NewInteger(2))),
			b:    NewVector(NewVector(NewInteger(1), NewInteger(2))),
			out:  true,
		},
		{
			name: "nested vectors different",
			a:    NewVector(NewVector(NewInteger(1), NewInteger(2))),
			b:    NewVector(NewVector(NewInteger(1), NewInteger(3))),
			out:  false,
		},
		{
			name: "mixed types equal",
			a:    NewVector(NewInteger(1), NewString("hello"), TrueValue),
			b:    NewVector(NewInteger(1), NewString("hello"), TrueValue),
			out:  true,
		},
		{
			name: "mixed types different",
			a:    NewVector(NewInteger(1), NewString("hello")),
			b:    NewVector(NewInteger(1), NewString("world")),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.a.EqualTo(tc.b), qt.Equals, tc.out)
		})
	}
}

func TestVectorSchemeString(t *testing.T) {
	tcs := []struct {
		name string
		in   *Vector
		out  string
	}{
		{
			name: "empty vector",
			in:   NewVector(),
			out:  "#()",
		},
		{
			name: "single element",
			in:   NewVector(NewInteger(42)),
			out:  "#( 42 )",
		},
		{
			name: "two elements",
			in:   NewVector(NewInteger(1), NewInteger(2)),
			out:  "#( 1 2 )",
		},
		{
			name: "three elements",
			in:   NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
			out:  "#( 1 2 3 )",
		},
		{
			name: "mixed types",
			in:   NewVector(NewInteger(1), NewString("hello"), TrueValue),
			out:  "#( 1 \"hello\" #t )",
		},
		{
			name: "nested vector",
			in:   NewVector(NewVector(NewInteger(1), NewInteger(2)), NewInteger(3)),
			out:  "#( #( 1 2 ) 3 )",
		},
		{
			name: "nested list",
			in:   NewVector(List(NewInteger(1), NewInteger(2)), NewInteger(3)),
			out:  "#( (1 2) 3 )",
		},
		{
			name: "symbols",
			in:   NewVector(NewSymbol("a"), NewSymbol("b")),
			out:  "#( a b )",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestVectorAsList(t *testing.T) {
	tcs := []struct {
		name   string
		in     *Vector
		out    Tuple
		isNil  bool
	}{
		{
			name:  "nil vector returns nil",
			in:    nil,
			isNil: true,
		},
		{
			name: "empty vector returns empty list",
			in:   NewVector(),
			out:  EmptyList,
		},
		{
			name: "single element vector",
			in:   NewVector(NewInteger(42)),
			out:  List(NewInteger(42)),
		},
		{
			name: "two element vector",
			in:   NewVector(NewInteger(1), NewInteger(2)),
			out:  List(NewInteger(1), NewInteger(2)),
		},
		{
			name: "three element vector",
			in:   NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
			out:  List(NewInteger(1), NewInteger(2), NewInteger(3)),
		},
		{
			name: "mixed types",
			in:   NewVector(NewInteger(1), NewString("hello"), TrueValue),
			out:  List(NewInteger(1), NewString("hello"), TrueValue),
		},
		{
			name: "nested list as element",
			in:   NewVector(List(NewInteger(1), NewInteger(2)), NewInteger(3)),
			out:  List(List(NewInteger(1), NewInteger(2)), NewInteger(3)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.in.AsList()
			if tc.isNil {
				qt.Assert(t, got, qt.IsNil)
			} else {
				qt.Assert(t, got, SchemeEquals, tc.out)
			}
		})
	}
}
