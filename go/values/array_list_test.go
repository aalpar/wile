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
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestArrayList_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out string
	}{
		{nil, "#<void>"},
		{NewArrayList(EmptyList), "()"},
		{NewArrayList(NewCons(nil, nil)), "()"},
		{NewArrayList(NewArrayList(nil, nil)), "()"},
		{NewArrayList(EmptyList), "()"},
		{NewArrayList(NewInteger(1), NewInteger(2), EmptyList), "(1 2)"},
		{NewArrayList(NewInteger(1), NewInteger(2), NewInteger(3), EmptyList), "(1 2 3)"},
		{NewArrayList(NewArrayList(NewInteger(1), NewInteger(2)), EmptyList), "((1 . 2))"},
		{NewArrayList(NewArrayList(NewInteger(1), nil), EmptyList), "((1 . #<void>))"},
	}

	for _, tc := range tcs {
		t.Run(tc.out, func(t *testing.T) {
			got := tc.in.SchemeString()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestArrayLis_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 *ArrayList
		in1 *ArrayList
		out bool
	}{
		{
			in0: (*ArrayList)(nil),
			in1: (*ArrayList)(nil),
			out: true,
		},
		{
			in0: NewArrayList(nil, nil),
			in1: NewArrayList(nil, nil),
			out: true,
		},
		{
			in0: NewArrayList(nil, nil),
			in1: NewArrayList(nil, nil),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), EmptyList),
			in1: NewArrayList(NewInteger(10), EmptyList),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), (*Pair)(nil)),
			in1: NewArrayList(NewInteger(10), Value(nil)),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), (*Pair)(nil)),
			in1: NewArrayList(NewInteger(10), Void),
			out: true,
		},
		{
			in0: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList),
			in1: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			in1: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			in1: NewArrayList(NewInteger(10), NewInteger(30), EmptyList),
			out: false,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(30), EmptyList),
			in1: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			out: false,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			in1: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			out: true,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			in1: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			out: false,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
			in1: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			out: false,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			in1: NewArrayList(NewInteger(10), NewInteger(20), Void),
			out: false,
		},
		{
			in0: NewArrayList(NewInteger(10), NewInteger(20), Void),
			in1: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in0.EqualTo(tc.in1)
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestArrayList_IsList(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out bool
	}{
		{in: nil, out: false},
		{in: NewArrayList(nil, nil), out: true},
		{in: NewArrayList(NewInteger(10), EmptyList), out: true},
		{in: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList), out: true},
		{in: NewArrayList(NewInteger(10), NewInteger(20), EmptyList), out: true},
		{in: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsList()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestArrayLis_Length(t *testing.T) {
	tcs := []struct {
		in           *ArrayList
		out          int
		panicMatches string
	}{
		{in: nil,
			panicMatches: "not a list",
			out:          -1},
		{in: NewArrayList(nil, nil), out: 0},
		{in: NewArrayList(NewInteger(10), EmptyList), out: 1},
		{in: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList), out: 1},
		{in: NewArrayList(NewInteger(10), NewInteger(20), EmptyList), out: 2},
		{in: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
			panicMatches: "not a list",
			out:          -1,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() { tc.in.Length() }, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.Length()
				qt.Assert(t, got, qt.Equals, tc.out)
			}
		})
	}
}

func TestArrayLis_IsVoid(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out bool
	}{
		{in: nil, out: true},
		{in: NewArrayList(nil, nil), out: false},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsVoid()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestArrayLis_IsEmptyList(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out bool
	}{
		{in: nil, out: false},
		{in: NewArrayList(nil, nil), out: true},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsEmptyList()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestArrayList_Append(t *testing.T) {
	tcs := []struct {
		in           *ArrayList
		vs           *ArrayList
		out          *ArrayList
		panicMatches string
	}{
		{
			in:           (*ArrayList)(nil),
			vs:           (*ArrayList)(nil),
			out:          (*ArrayList)(nil),
			panicMatches: "not a list",
		},
		{
			in:  NewArrayList(nil, nil),
			vs:  (*ArrayList)(nil),
			out: (*ArrayList)(nil),
		},
		{
			in:  NewArrayList(nil, nil),
			vs:  (*ArrayList)(nil),
			out: (*ArrayList)(nil),
		},
		{
			in:  NewArrayList(nil, nil),
			vs:  NewArrayList(NewInteger(10), EmptyList),
			out: NewArrayList(NewInteger(10), EmptyList),
		},
		{
			in:  NewArrayList(NewInteger(10), EmptyList),
			vs:  NewArrayList(nil, nil),
			out: NewArrayList(NewInteger(10), EmptyList),
		},
		{
			in:  NewArrayList(NewInteger(10), EmptyList),
			vs:  NewArrayList(NewInteger(20), EmptyList),
			out: NewArrayList(NewInteger(10), NewInteger(20), EmptyList),
		},
		{
			in:  NewArrayList(NewInteger(10), EmptyList),
			vs:  NewArrayList(NewInteger(20), NewInteger(30)),
			out: NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() {
					tc.in.AppendList(tc.vs)
				}, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.AppendList(tc.vs)
				qt.Assert(t, got, SchemeEquals, tc.out)
			}
		})
	}
}

func TestArrayList_AsList(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out Value
	}{
		{
			in:  NewArrayList(NewSymbol("first"), NewSymbol("second"), NewSymbol("third"), EmptyList),
			out: List(NewSymbol("first"), NewSymbol("second"), NewSymbol("third")),
		},
		{
			in:  NewArrayList(NewSymbol("first"), NewSymbol("second"), EmptyList),
			out: List(NewSymbol("first"), NewSymbol("second")),
		},
		{
			in:  NewArrayList(NewSymbol("first"), EmptyList),
			out: List(NewSymbol("first")),
		},
		{
			in:  NewArrayList(NewSymbol("first"), NewSymbol("cdr")),
			out: NewCons(NewSymbol("first"), NewSymbol("cdr")),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			q := tc.in.AsList()
			qt.Assert(t, q, SchemeEquals, tc.out)
		})
	}
}

func TestArrayList_Datum(t *testing.T) {
	a := NewArrayList(NewInteger(1), NewInteger(2))
	datum := a.Datum()
	qt.Assert(t, len(datum), qt.Equals, 2)
	qt.Assert(t, datum[0], SchemeEquals, NewInteger(1))
	qt.Assert(t, datum[1], SchemeEquals, NewInteger(2))
}

func TestArrayList_Car(t *testing.T) {
	a := NewArrayList(NewInteger(42), NewInteger(99))
	qt.Assert(t, a.Car(), SchemeEquals, NewInteger(42))
}

func TestArrayList_Cdr(t *testing.T) {
	a := NewArrayList(NewInteger(42), NewInteger(99))
	cdr := a.Cdr()
	cdrList, ok := cdr.(*ArrayList)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, len(*cdrList), qt.Equals, 1)
	qt.Assert(t, (*cdrList)[0], SchemeEquals, NewInteger(99))
}

func TestArrayList_ForEach(t *testing.T) {
	a := NewArrayList(NewInteger(1), NewInteger(2), NewInteger(3))
	count := 0
	sum := int64(0)
	a.ForEach(nil, func(i int, hasNext bool, v Value) error { //nolint:errcheck
		count++
		if intVal, ok := v.(*Integer); ok {
			sum += intVal.Value
		}
		return nil
	})
	qt.Assert(t, count, qt.Equals, 3)
	qt.Assert(t, sum, qt.Equals, int64(6))
}

func TestArrayList_AsVector(t *testing.T) {
	a := NewArrayList(NewInteger(1), NewInteger(2), NewInteger(3))
	v := a.AsVector()
	qt.Assert(t, len(*v), qt.Equals, 3)
	qt.Assert(t, (*v)[0], SchemeEquals, NewInteger(1))
	qt.Assert(t, (*v)[1], SchemeEquals, NewInteger(2))
	qt.Assert(t, (*v)[2], SchemeEquals, NewInteger(3))
}

func TestArrayList_Append_Single(t *testing.T) {
	a := NewArrayList(NewInteger(1), NewInteger(2))
	result := a.Append(NewInteger(3))
	resultList, ok := result.(*ArrayList)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, len(*resultList), qt.Equals, 3)
	qt.Assert(t, (*resultList)[2], SchemeEquals, NewInteger(3))
}
