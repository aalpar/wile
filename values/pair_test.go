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
		{NewCons(nil, nil), "(#<void> . #<void>)"},
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
		in1 Value
		out bool
	}{
		{
			nm:  "1",
			in0: (*Pair)(nil),
			in1: (*Pair)(nil),
			out: true,
		},
		{
			nm:  "3",
			in0: &Pair{nil, nil},
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

func TestEmptyList_EqualTo(t *testing.T) {
	qt.Assert(t, EmptyList.EqualTo(EmptyList), qt.IsTrue)
	qt.Assert(t, EqualTo(EmptyList, EmptyList), qt.IsTrue)
	qt.Assert(t, EmptyList.EqualTo(NewCons(NewInteger(1), EmptyList)), qt.IsFalse)
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
		{in: NewCons(NewInteger(10), EmptyList), out: true},
		{in: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList), out: true},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)), out: true},
		{
			in:  NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
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

func TestEmptyList_IsList(t *testing.T) {
	qt.Assert(t, EmptyList.IsList(), qt.IsTrue)
}

func TestPair_IsList_Circular(t *testing.T) {
	// Test that circular lists return false (R7RS §6.4)
	// This tests Floyd's cycle detection algorithm

	t.Run("self-referential", func(t *testing.T) {
		// (set-cdr! x x) - cdr points to self
		p := NewCons(NewSymbol("a"), EmptyList)
		p.SetCdr(p)
		qt.Assert(t, p.IsList(), qt.Equals, false)
	})

	t.Run("cycle after one element", func(t *testing.T) {
		// (a . #0=(b . #0#))
		p1 := NewCons(NewSymbol("a"), EmptyList)
		p2 := NewCons(NewSymbol("b"), EmptyList)
		p1.SetCdr(p2)
		p2.SetCdr(p2) // p2 points to itself
		qt.Assert(t, p1.IsList(), qt.Equals, false)
	})

	t.Run("cycle back to start", func(t *testing.T) {
		// #0=(a b . #0#)
		p1 := NewCons(NewSymbol("a"), EmptyList)
		p2 := NewCons(NewSymbol("b"), EmptyList)
		p1.SetCdr(p2)
		p2.SetCdr(p1) // cycle back to start
		qt.Assert(t, p1.IsList(), qt.Equals, false)
	})

	t.Run("longer cycle", func(t *testing.T) {
		// (a b c d . #0=(e f . #0#))
		cells := make([]*Pair, 6)
		for i := range cells {
			cells[i] = NewCons(NewInteger(int64(i)), EmptyList)
		}
		for i := 0; i < 5; i++ {
			cells[i].SetCdr(cells[i+1])
		}
		cells[5].SetCdr(cells[4]) // cycle between last two
		qt.Assert(t, cells[0].IsList(), qt.Equals, false)
	})
}

func TestPair_Length(t *testing.T) {
	tcs := []struct {
		in           *Pair
		out          int
		panicMatches string
	}{
		{
			in:           nil,
			panicMatches: "not a list",
			out:          -1,
		},
		{in: NewCons(NewInteger(10), EmptyList), out: 1},
		{in: NewCons(NewCons(NewInteger(10), EmptyList), EmptyList), out: 1},
		{in: NewCons(NewInteger(10), NewCons(NewInteger(20), EmptyList)), out: 2},
		{
			in:           NewCons(NewInteger(10), NewCons(NewInteger(20), NewInteger(30))),
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

func TestEmptyList_Length(t *testing.T) {
	qt.Assert(t, EmptyList.Length(), qt.Equals, 0)
}

func TestPair_IsVoid(t *testing.T) {
	qt.Assert(t, (*Pair)(nil).IsVoid(), qt.IsTrue)
	qt.Assert(t, NewCons(NewInteger(1), EmptyList).IsVoid(), qt.IsFalse)
}

func TestPair_IsEmptyList(t *testing.T) {
	// *Pair.IsEmptyList() always returns false now that EmptyList is a separate type
	qt.Assert(t, (*Pair)(nil).IsEmptyList(), qt.IsFalse)
	qt.Assert(t, NewCons(NewInteger(1), EmptyList).IsEmptyList(), qt.IsFalse)
}

func TestEmptyList_IsVoidAndIsEmptyList(t *testing.T) {
	qt.Assert(t, EmptyList.IsEmptyList(), qt.IsTrue)
	qt.Assert(t, EmptyList.IsVoid(), qt.IsFalse)
	qt.Assert(t, IsEmptyList(EmptyList), qt.IsTrue)
	qt.Assert(t, IsVoid(EmptyList), qt.IsFalse)
}

func TestEmptyList_AsVector(t *testing.T) {
	got := EmptyList.AsVector()
	qt.Assert(t, got, qt.Not(qt.IsNil))
	qt.Assert(t, len(got.Datum()), qt.Equals, 0)
}

func TestEmptyList_Append(t *testing.T) {
	// Appending to empty list returns the argument
	got := EmptyList.Append(NewCons(NewInteger(10), EmptyList))
	qt.Assert(t, got, SchemeEquals, NewCons(NewInteger(10), EmptyList))

	// Appending nil/void returns it
	got = EmptyList.Append((*Pair)(nil))
	qt.Assert(t, got, qt.Equals, Value((*Pair)(nil)))
}

func TestEmptyList_SchemeString(t *testing.T) {
	qt.Assert(t, EmptyList.SchemeString(), qt.Equals, "()")
}

func TestEmptyList_Car_Panics(t *testing.T) {
	qt.Assert(t, func() { EmptyList.Car() }, qt.PanicMatches, "not a pair")
}

func TestEmptyList_Cdr_Panics(t *testing.T) {
	qt.Assert(t, func() { EmptyList.Cdr() }, qt.PanicMatches, "not a pair")
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
			name:         "void pair panics",
			in:           NewCons(nil, nil),
			panicMatches: "not a list",
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
			name:         "void pair input",
			in:           NewCons(nil, nil),
			vs:           (*Pair)(nil),
			panicMatches: "not a list",
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
