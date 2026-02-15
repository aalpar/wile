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
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestArrayList_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *ArrayList
		out string
	}{
		{nil, "#<void>"},
		{NewArrayList(EmptyList), "()"},
		{NewArrayList(NewCons(nil, nil)), "(#<void> . #<void>)"},
		{NewArrayList(NewArrayList(EmptyList)), "()"},
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
			in0: NewArrayList(EmptyList),
			in1: NewArrayList(EmptyList),
			out: true,
		},
		{
			in0: NewArrayList(EmptyList),
			in1: NewArrayList(EmptyList),
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
		{in: NewArrayList(EmptyList), out: true},
		{in: NewArrayList(NewInteger(10), EmptyList), out: true},
		{in: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList), out: true},
		{in: NewArrayList(NewInteger(10), NewInteger(20), EmptyList), out: true},
		{
			in:  NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
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
		{
			in:           nil,
			panicMatches: "not a list",
			out:          -1,
		},
		{in: NewArrayList(EmptyList), out: 0},
		{in: NewArrayList(NewInteger(10), EmptyList), out: 1},
		{in: NewArrayList(NewArrayList(NewInteger(10), EmptyList), EmptyList), out: 1},
		{in: NewArrayList(NewInteger(10), NewInteger(20), EmptyList), out: 2},
		{
			in:           NewArrayList(NewInteger(10), NewInteger(20), NewInteger(30)),
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
		{in: NewArrayList(EmptyList), out: false},
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
		{in: NewArrayList(EmptyList), out: true},
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
			in:  NewArrayList(EmptyList),
			vs:  (*ArrayList)(nil),
			out: (*ArrayList)(nil),
		},
		{
			in:  NewArrayList(EmptyList),
			vs:  (*ArrayList)(nil),
			out: (*ArrayList)(nil),
		},
		{
			in:  NewArrayList(EmptyList),
			vs:  NewArrayList(NewInteger(10), EmptyList),
			out: NewArrayList(NewInteger(10), EmptyList),
		},
		{
			in:  NewArrayList(NewInteger(10), EmptyList),
			vs:  NewArrayList(EmptyList),
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
	c := qt.New(t)

	t.Run("proper list", func(t *testing.T) {
		// (1 2 3)
		a := NewArrayList(NewInteger(1), NewInteger(2), NewInteger(3), EmptyList)
		cdr := a.Cdr()
		cdrList, ok := cdr.(*ArrayList)
		c.Assert(ok, qt.IsTrue, qt.Commentf("Cdr of multi-element proper list should return ArrayList"))
		c.Assert(len(*cdrList), qt.Equals, 3)
		c.Assert((*cdrList)[0], SchemeEquals, NewInteger(2))
		c.Assert((*cdrList)[1], SchemeEquals, NewInteger(3))
		c.Assert((*cdrList)[2], SchemeEquals, EmptyList)
	})

	t.Run("improper list", func(t *testing.T) {
		// (42 . 99)
		a := NewArrayList(NewInteger(42), NewInteger(99))
		cdr := a.Cdr()
		// Should return the direct value, not wrapped in ArrayList
		c.Assert(cdr, SchemeEquals, NewInteger(99), qt.Commentf("Cdr of improper list should return terminator directly"))
	})

	t.Run("single element proper list", func(t *testing.T) {
		// (42)
		a := NewArrayList(NewInteger(42), EmptyList)
		cdr := a.Cdr()
		c.Assert(cdr, SchemeEquals, EmptyList, qt.Commentf("Cdr of (42) should return EmptyList"))
	})
}

func TestArrayList_ForEach(t *testing.T) {
	// ArrayList: [1, 2, EmptyList] - proper list (1 2)
	// ForEach should visit elements 1 and 2, NOT the EmptyList terminator
	a := NewArrayList(NewInteger(1), NewInteger(2), EmptyList)
	count := 0
	sum := int64(0)
	a.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error { //nolint:errcheck
		count++
		intVal, ok := v.(*Integer)
		if ok {
			sum += intVal.Value
		}
		return nil
	})
	qt.Assert(t, count, qt.Equals, 2)
	qt.Assert(t, sum, qt.Equals, int64(3))
}

// TestArrayList_ForEach_TupleContract verifies ArrayList.ForEach matches Pair.ForEach
// semantics per the Tuple interface contract (issue #172).
func TestArrayList_ForEach_TupleContract(t *testing.T) {
	c := qt.New(t)

	t.Run("proper list - terminator not visited", func(t *testing.T) {
		// ArrayList: [5, 10, EmptyList]
		list := NewArrayList(NewInteger(5), NewInteger(10), EmptyList)

		visited := []Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 2, qt.Commentf("should visit 2 elements, not 3"))
		c.Assert(visited[0], SchemeEquals, NewInteger(5))
		c.Assert(visited[1], SchemeEquals, NewInteger(10))
		c.Assert(tail, SchemeEquals, EmptyList, qt.Commentf("should return EmptyList as tail"))
	})

	t.Run("improper list - terminator not visited", func(t *testing.T) {
		// ArrayList: [5, 10, 999] (improper, 999 is the cdr)
		improperCdr := NewInteger(999)
		list := NewArrayList(NewInteger(5), NewInteger(10), improperCdr)

		visited := []Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 2, qt.Commentf("should visit 2 elements, not 3"))
		c.Assert(visited[0], SchemeEquals, NewInteger(5))
		c.Assert(visited[1], SchemeEquals, NewInteger(10))
		c.Assert(tail, SchemeEquals, improperCdr, qt.Commentf("should return improper cdr as tail"))
	})

	t.Run("single element list", func(t *testing.T) {
		// ArrayList: [42, EmptyList]
		list := NewArrayList(NewInteger(42), EmptyList)

		visited := []Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 1)
		c.Assert(visited[0], SchemeEquals, NewInteger(42))
		c.Assert(tail, SchemeEquals, EmptyList)
	})

	t.Run("empty list", func(t *testing.T) {
		// ArrayList: [EmptyList] (just the terminator)
		list := NewArrayList(EmptyList)

		visited := []Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 0, qt.Commentf("empty list should not visit any elements"))
		c.Assert(tail, SchemeEquals, EmptyList)
	})

	t.Run("matches Pair.ForEach semantics", func(t *testing.T) {
		// Create equivalent Pair and ArrayList for (5 10)
		pair := List(NewInteger(5), NewInteger(10))
		arraylist := NewArrayList(NewInteger(5), NewInteger(10), EmptyList)

		// Collect elements from Pair
		pairElements := []Value{}
		pairTail, pairErr := pair.(*Pair).ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			pairElements = append(pairElements, v)
			return nil
		})

		// Collect elements from ArrayList
		arraylistElements := []Value{}
		arraylistTail, arraylistErr := arraylist.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v Value) error {
			arraylistElements = append(arraylistElements, v)
			return nil
		})

		// Both should have identical results
		c.Assert(pairErr, qt.IsNil)
		c.Assert(arraylistErr, qt.IsNil)
		c.Assert(len(arraylistElements), qt.Equals, len(pairElements), qt.Commentf("should visit same number of elements"))
		c.Assert(arraylistElements[0], SchemeEquals, pairElements[0])
		c.Assert(arraylistElements[1], SchemeEquals, pairElements[1])
		c.Assert(arraylistTail, SchemeEquals, pairTail, qt.Commentf("should return same tail"))
	})
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

// TestArrayList_PairEquivalence verifies that Pair and ArrayList are indistinguishable
// when using Car(), Cdr(), SetCar(), and SetCdr() operations.
func TestArrayList_PairEquivalence(t *testing.T) {
	c := qt.New(t)

	t.Run("proper list (1 2 3)", func(t *testing.T) {
		pair := NewCons(NewInteger(1), NewCons(NewInteger(2), NewCons(NewInteger(3), EmptyList)))
		arraylist := NewArrayList(NewInteger(1), NewInteger(2), NewInteger(3), EmptyList)

		// Car should be identical
		c.Assert(pair.Car(), SchemeEquals, arraylist.Car())

		// Cdr should return equivalent structures
		pairCdr := pair.Cdr().(*Pair)
		arraylistCdr := arraylist.Cdr().(*ArrayList)
		c.Assert(pairCdr.Car(), SchemeEquals, arraylistCdr.Car())
		c.Assert(pairCdr.Cdr().(*Pair).Car(), SchemeEquals, arraylistCdr.Cdr().(*ArrayList).Car())
	})

	t.Run("improper list (1 . 2)", func(t *testing.T) {
		pair := NewCons(NewInteger(1), NewInteger(2))
		arraylist := NewArrayList(NewInteger(1), NewInteger(2))

		// Car should be identical
		c.Assert(pair.Car(), SchemeEquals, arraylist.Car())

		// Cdr should return the direct value, not a wrapped structure
		c.Assert(pair.Cdr(), SchemeEquals, NewInteger(2))
		c.Assert(arraylist.Cdr(), SchemeEquals, NewInteger(2))
		c.Assert(pair.Cdr(), SchemeEquals, arraylist.Cdr(), qt.Commentf("Pair and ArrayList Cdr must return identical values for improper lists"))
	})

	t.Run("single element proper list (42)", func(t *testing.T) {
		pair := NewCons(NewInteger(42), EmptyList)
		arraylist := NewArrayList(NewInteger(42), EmptyList)

		c.Assert(pair.Car(), SchemeEquals, arraylist.Car())
		c.Assert(pair.Cdr(), SchemeEquals, EmptyList)
		c.Assert(arraylist.Cdr(), SchemeEquals, EmptyList)
		c.Assert(pair.Cdr(), SchemeEquals, arraylist.Cdr())
	})
}
