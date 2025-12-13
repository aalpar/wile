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
	"context"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
)

func Test_List(t *testing.T) {
	tcs := []struct {
		in     *Pair
		out    *Pair
		expect bool
	}{
		{
			in:     List(),
			out:    EmptyList,
			expect: true,
		},
		{
			in:     List(nil),
			out:    NewCons(nil, EmptyList),
			expect: true,
		},
		{
			in:     List(NewSymbol("first")),
			out:    NewCons(NewSymbol("first"), EmptyList),
			expect: true,
		},
		{
			in:     List(NewSymbol("first"), NewSymbol("second")),
			out:    NewCons(NewSymbol("first"), NewCons(NewSymbol("second"), EmptyList)),
			expect: true,
		},
	}

	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%d", i), func(t *testing.T) {
			q := tc.in.EqualTo(tc.out)
			qt.Assert(t, q, qt.Equals, tc.expect)
		})
	}
}

func Test_FlipVectorToList(t *testing.T) {
	tcs := []struct {
		name string
		in   *Vector
		out  *Pair
	}{
		{
			name: "nil vector returns empty list",
			in:   nil,
			out:  EmptyList,
		},
		{
			name: "empty vector returns empty list",
			in:   NewVector(),
			out:  EmptyList,
		},
		{
			name: "single element vector",
			in:   NewVector(NewInteger(1)),
			out:  List(NewInteger(1)),
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
			got := VectorToList(tc.in)
			qt.Assert(t, got, SchemeEquals, tc.out)
		})
	}
}

func Test_ForEach(t *testing.T) {
	list := List(NewInteger(1), NewInteger(2), NewInteger(3))
	count := 0
	sum := int64(0)

	tail, err := ForEach(nil, list, func(_ context.Context, i int, hasNext bool, v Value) error {
		count++
		if intVal, ok := v.(*Integer); ok {
			sum += intVal.Value
		}
		return nil
	})

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, SchemeEquals, EmptyList)
	qt.Assert(t, count, qt.Equals, 3)
	qt.Assert(t, sum, qt.Equals, int64(6))
}

func Test_ForEach_NonTuple(t *testing.T) {
	i := NewInteger(42)
	tail, err := ForEach(nil, i, func(_ context.Context, _ int, _ bool, _ Value) error {
		return fmt.Errorf("should not be called")
	})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, tail, SchemeEquals, i)
}

func Test_NewTemporaryVariableName(t *testing.T) {
	sym1 := NewTemporaryVariableName()
	sym2 := NewTemporaryVariableName()

	qt.Assert(t, sym1.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym2.Key[:4], qt.Equals, "__T_")
	qt.Assert(t, sym1.Key, qt.Not(qt.Equals), sym2.Key)
}

func Test_IsList(t *testing.T) {
	tests := []struct {
		name string
		in   Value
		out  bool
	}{
		{"nil is not a list", nil, false},
		{"empty list is a list", EmptyList, true},
		{"proper list is a list", List(NewInteger(1), NewInteger(2)), true},
		{"improper list is not a list", NewCons(NewInteger(1), NewInteger(2)), false},
		{"arraylist proper list is a list", NewArrayList(NewInteger(1), NewInteger(2), EmptyList), true},
		{"arraylist improper list is not a list", NewArrayList(NewInteger(1), NewInteger(2)), false},
		{"integer is not a list", NewInteger(42), false},
		{"string is not a list", NewString("hello"), false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, IsList(tc.in), qt.Equals, tc.out)
		})
	}
}
