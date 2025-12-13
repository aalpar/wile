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

func TestPair_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *Pair
		out string
	}{
		{nil, "#<void>"},
		{NewCons(nil, nil), "()"},
		{NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList)), "(1 2)"},
		{NewCons(NewInteger(1), NewCons(NewInteger(2), NewCons(NewInteger(3), EmptyList))), "(1 2 3)"},
		{NewCons(NewCons(NewInteger(1), NewInteger(2)), EmptyList), "((1 . 2))"},
		{NewCons(NewCons(NewInteger(1), (*Pair)(nil)), EmptyList), "((1 . #<void>))"},
	}

	for _, tc := range tcs {
		got := tc.in.SchemeString()
		qt.Assert(t, got, qt.Equals, tc.out)
	}
}

func TestPair_EqualTo(t *testing.T) {
	tcs := []struct {
		nm  string
		in0 *Pair
		in1 *Pair
		out bool
	}{
		{
			nm:  "1",
			in0: (*Pair)(nil),
			in1: (*Pair)(nil),
			out: true,
		},
		{
			nm:  "2",
			in0: EmptyList,
			in1: EmptyList,
			out: true,
		},
		{
			nm:  "3",
			in0: &Pair{nil, nil},
			in1: EmptyList,
			out: true,
		},
		{
			nm:  "4",
			in0: EmptyList,
			in1: &Pair{nil, nil},
			out: true,
		},
		{
			nm:  "5",
			in0: (*Pair)(nil),
			in1: (*Pair)(nil),
			out: true,
		},
		{
			nm:  "6",
			in0: NewCons(NewInteger(10), EmptyList),
			in1: NewCons(NewInteger(10), EmptyList),
			out: true,
		},
		{
			nm:  "7",
			in0: NewCons(NewInteger(10), (*Pair)(nil)),
			in1: NewCons(NewInteger(10), Value(nil)),
			out: true,
		},
		{
			nm:  "8",
			in0: NewCons(NewInteger(10), (*Pair)(nil)),
			in1: NewCons(NewInteger(10), Void),
			out: true,
		},
		{
			nm:  "9",
			in0: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList),
			in1: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList),
			out: true,
		},
		{
			nm:  "10",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			out: true,
		},
		{
			nm:  "11",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(30), EmptyList)),
			out: false,
		},
		{
			nm:  "12",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(30), EmptyList)),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			out: false,
		},
		{
			nm:  "13",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			out: true,
		},
		{
			nm:  "14",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			out: false,
		},
		{
			nm:  "15",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			out: false,
		},
		{
			nm:  "16",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), Void)),
			out: false,
		},
		{
			nm:  "17",
			in0: NewCons(NewInteger(10), NewCons(NewInteger(20), Void)),
			in1: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.nm, func(t *testing.T) {
			got := tc.in0.EqualTo(tc.in1)
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestPair_NewCons(t *testing.T) {
	pr := NewCons(nil, nil)
	qt.Assert(t, pr, qt.Not(qt.IsNil))
}

func TestPair_IsList(t *testing.T) {
	tcs := []struct {
		in  *Pair
		out bool
	}{
		{in: nil, out: false},
		{in: EmptyList, out: true},
		{in: NewCons(NewInteger(10), EmptyList), out: true},
		{in: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList), out: true},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)), out: true},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
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

func TestPair_Length(t *testing.T) {
	tcs := []struct {
		in           *Pair
		out          int
		panicMatches string
	}{
		{in: nil,
			panicMatches: "not a list",
			out:          -1},
		{in: EmptyList, out: 0},
		{in: NewCons(NewInteger(10), EmptyList), out: 1},
		{in: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList), out: 1},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)), out: 2},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
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

func TestPair_IsVoid(t *testing.T) {
	tcs := []struct {
		in  *Pair
		out bool
	}{
		{in: nil, out: true},
		{in: EmptyList, out: false},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsVoid()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestPair_IsEmptyList(t *testing.T) {
	tcs := []struct {
		in  *Pair
		out bool
	}{
		{in: nil, out: false},
		{in: EmptyList, out: true},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsEmptyList()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestPair_AsVector(t *testing.T) {
	tcs := []struct {
		name         string
		in           *Pair
		out          *Vector
		panicMatches string
	}{
		{
			name: "nil returns nil",
			in:   nil,
			out:  nil,
		},
		{
			name: "empty list returns empty vector",
			in:   EmptyList,
			out:  NewVector(),
		},
		{
			name: "single element list",
			in:   NewCons(NewInteger(10), EmptyList),
			out:  NewVector(NewInteger(10)),
		},
		{
			name: "two element list",
			in:   NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
			out:  NewVector(NewInteger(10), NewInteger(20)),
		},
		{
			name: "three element list",
			in:   NewCons(NewInteger(1), NewCons(NewInteger(2), NewCons(NewInteger(3), EmptyList))),
			out:  NewVector(NewInteger(1), NewInteger(2), NewInteger(3)),
		},
		{
			name: "nested list as element",
			in:   NewCons(NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList)), EmptyList),
			out:  NewVector(NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList))),
		},
		{
			name: "mixed types",
			in:   NewCons(NewInteger(1), NewCons(NewString("hello"), NewCons(TrueValue, EmptyList))),
			out:  NewVector(NewInteger(1), NewString("hello"), TrueValue),
		},
		{
			name:         "improper list panics",
			in:           NewCons(NewInteger(1), NewInteger(2)),
			panicMatches: "not a list",
		},
		{
			name:         "improper list with multiple elements panics",
			in:           NewCons(NewInteger(1), NewCons(NewInteger(2), NewInteger(3))),
			panicMatches: "not a list",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() { tc.in.AsVector() }, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.AsVector()
				if tc.out == nil {
					qt.Assert(t, got, qt.IsNil)
				} else {
					qt.Assert(t, got, SchemeEquals, tc.out)
				}
			}
		})
	}
}

func TestPair_Append(t *testing.T) {
	tcs := []struct {
		name         string
		in           *Pair
		vs           Value
		out          Value
		panicMatches string
	}{
		{
			name:         "nil input",
			in:           (*Pair)(nil),
			vs:           (*Pair)(nil),
			out:          (*Pair)(nil),
			panicMatches: "not a list",
		},
		{
			name: "empty list input",
			in:   EmptyList,
			vs:   (*Pair)(nil),
			out:  (*Pair)(nil),
		},
		{
			name: "nil vs",
			in:   NewCons(nil, nil),
			vs:   (*Pair)(nil),
			out:  (*Pair)(nil),
		},
		{
			name: "empty vs",
			in:   EmptyList,
			vs:   NewCons(NewInteger(10), EmptyList),
			out:  NewCons(NewInteger(10), EmptyList),
		},
		{
			name: "empty vs with nil",
			in:   NewCons(NewInteger(10), EmptyList),
			vs:   EmptyList,
			out:  NewCons(NewInteger(10), EmptyList),
		},
		{
			name: "append to empty list",
			in:   NewCons(NewInteger(10), EmptyList),
			vs:   NewCons(NewInteger(20), EmptyList),
			out:  NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)),
		},
		{
			name: "append to empty list with nil",
			in:   NewCons(NewInteger(10), EmptyList),
			vs:   NewCons(NewInteger(20), NewInteger(30)),
			out:  NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
		},
		{
			name:         "append to non-list pair",
			in:           NewCons(NewInteger(1), NewInteger(2)),
			vs:           NewCons(NewInteger(3), EmptyList),
			panicMatches: "not a list",
		},
		{
			name: "append non-list value",
			in:   NewCons(NewInteger(1), EmptyList),
			vs:   NewInteger(2),
			out:  NewCons(NewInteger(1), NewInteger(2)),
		},
		{
			name: "append Void to list",
			in:   NewCons(NewInteger(1), EmptyList),
			vs:   Void,
			out:  NewCons(NewInteger(1), Void),
		},
		{
			name:         "append Void to non-list",
			in:           NewCons(NewInteger(1), NewInteger(2)),
			vs:           Void,
			panicMatches: "not a list",
		},
		{
			name:         "append Void to list",
			in:           NewCons(NewInteger(1), NewInteger(2)),
			vs:           Void,
			panicMatches: "not a list",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() {
					tc.in.Append(tc.vs)
				}, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.Append(tc.vs)
				qt.Assert(t, got, SchemeEquals, tc.out)
			}
		})
	}
}

func TestPair_Car(t *testing.T) {
	p := NewCons(NewInteger(42), NewInteger(99))
	qt.Assert(t, p.Car(), SchemeEquals, NewInteger(42))

	p2 := NewCons(NewString("hello"), EmptyList)
	qt.Assert(t, p2.Car(), SchemeEquals, NewString("hello"))
}

func TestPair_SetCar(t *testing.T) {
	p := NewCons(NewInteger(1), NewInteger(2))
	p.SetCar(NewInteger(10))
	qt.Assert(t, p.Car(), SchemeEquals, NewInteger(10))
	qt.Assert(t, p.Cdr(), SchemeEquals, NewInteger(2))
}

func TestPair_SetCdr(t *testing.T) {
	p := NewCons(NewInteger(1), NewInteger(2))
	p.SetCdr(NewInteger(20))
	qt.Assert(t, p.Car(), SchemeEquals, NewInteger(1))
	qt.Assert(t, p.Cdr(), SchemeEquals, NewInteger(20))
}

func TestPair_Datum(t *testing.T) {
	p := NewCons(NewInteger(1), NewInteger(2))
	datum := p.Datum()
	qt.Assert(t, datum[0], SchemeEquals, NewInteger(1))
	qt.Assert(t, datum[1], SchemeEquals, NewInteger(2))
}

func TestPair_String(t *testing.T) {
	p := NewCons(NewInteger(1), NewCons(NewInteger(2), EmptyList))
	s := p.String()
	qt.Assert(t, s, qt.Equals, "(1 2)")

	p2 := NewCons(NewInteger(1), NewInteger(2))
	s2 := p2.String()
	qt.Assert(t, s2, qt.Equals, "(1 . 2)")
}
