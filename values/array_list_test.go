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

package values_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestArrayList_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *values.ArrayList
		out string
	}{
		{nil, "#<void>"},
		{values.NewArrayList(values.EmptyList), "()"},
		{values.NewArrayList(values.NewCons(nil, nil)), "(#<void> . #<void>)"},
		{values.NewArrayList(values.NewArrayList(values.EmptyList)), "()"},
		{values.NewArrayList(values.EmptyList), "()"},
		{values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.EmptyList), "(1 2)"},
		{values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.EmptyList), "(1 2 3)"},
		{values.NewArrayList(values.NewArrayList(values.NewInteger(1), values.NewInteger(2)), values.EmptyList), "((1 . 2))"},
		{values.NewArrayList(values.NewArrayList(values.NewInteger(1), nil), values.EmptyList), "((1 . #<void>))"},
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
		in0 *values.ArrayList
		in1 *values.ArrayList
		out bool
	}{
		{
			in0: (*values.ArrayList)(nil),
			in1: (*values.ArrayList)(nil),
			out: true,
		},
		{
			in0: values.NewArrayList(values.EmptyList),
			in1: values.NewArrayList(values.EmptyList),
			out: true,
		},
		{
			in0: values.NewArrayList(values.EmptyList),
			in1: values.NewArrayList(values.EmptyList),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.EmptyList),
			in1: values.NewArrayList(values.NewInteger(10), values.EmptyList),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), (*values.Pair)(nil)),
			in1: values.NewArrayList(values.NewInteger(10), values.Value(nil)),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), (*values.Pair)(nil)),
			in1: values.NewArrayList(values.NewInteger(10), values.Void),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewArrayList(values.NewInteger(10), values.EmptyList), values.EmptyList),
			in1: values.NewArrayList(values.NewArrayList(values.NewInteger(10), values.EmptyList), values.EmptyList),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(30), values.EmptyList),
			out: false,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(30), values.EmptyList),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			out: false,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			out: true,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			out: false,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			out: false,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.Void),
			out: false,
		},
		{
			in0: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.Void),
			in1: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
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
		in  *values.ArrayList
		out bool
	}{
		{in: nil, out: false},
		{in: values.NewArrayList(values.EmptyList), out: true},
		{in: values.NewArrayList(values.NewInteger(10), values.EmptyList), out: true},
		{in: values.NewArrayList(values.NewArrayList(values.NewInteger(10), values.EmptyList), values.EmptyList), out: true},
		{in: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList), out: true},
		{
			in:  values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
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
		in           *values.ArrayList
		out          int
		panicMatches string
	}{
		{
			in:           nil,
			panicMatches: "not a list",
			out:          -1,
		},
		{in: values.NewArrayList(values.EmptyList), out: 0},
		{in: values.NewArrayList(values.NewInteger(10), values.EmptyList), out: 1},
		{in: values.NewArrayList(values.NewArrayList(values.NewInteger(10), values.EmptyList), values.EmptyList), out: 1},
		{in: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList), out: 2},
		{
			in:           values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
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
		in  *values.ArrayList
		out bool
	}{
		{in: nil, out: true},
		{in: values.NewArrayList(values.EmptyList), out: false},
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
		in  *values.ArrayList
		out bool
	}{
		{in: nil, out: false},
		{in: values.NewArrayList(values.EmptyList), out: true},
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
		in           *values.ArrayList
		vs           *values.ArrayList
		out          *values.ArrayList
		panicMatches string
	}{
		{
			in:           (*values.ArrayList)(nil),
			vs:           (*values.ArrayList)(nil),
			out:          (*values.ArrayList)(nil),
			panicMatches: "not a list",
		},
		{
			in:  values.NewArrayList(values.EmptyList),
			vs:  (*values.ArrayList)(nil),
			out: (*values.ArrayList)(nil),
		},
		{
			in:  values.NewArrayList(values.EmptyList),
			vs:  (*values.ArrayList)(nil),
			out: (*values.ArrayList)(nil),
		},
		{
			in:  values.NewArrayList(values.EmptyList),
			vs:  values.NewArrayList(values.NewInteger(10), values.EmptyList),
			out: values.NewArrayList(values.NewInteger(10), values.EmptyList),
		},
		{
			in:  values.NewArrayList(values.NewInteger(10), values.EmptyList),
			vs:  values.NewArrayList(values.EmptyList),
			out: values.NewArrayList(values.NewInteger(10), values.EmptyList),
		},
		{
			in:  values.NewArrayList(values.NewInteger(10), values.EmptyList),
			vs:  values.NewArrayList(values.NewInteger(20), values.EmptyList),
			out: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.EmptyList),
		},
		{
			in:  values.NewArrayList(values.NewInteger(10), values.EmptyList),
			vs:  values.NewArrayList(values.NewInteger(20), values.NewInteger(30)),
			out: values.NewArrayList(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
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
				qt.Assert(t, got, valuestest.SchemeEquals, tc.out)
			}
		})
	}
}

func TestArrayList_AsList(t *testing.T) {
	tcs := []struct {
		in  *values.ArrayList
		out values.Value
	}{
		{
			in:  values.NewArrayList(values.NewSymbol("first"), values.NewSymbol("second"), values.NewSymbol("third"), values.EmptyList),
			out: values.List(values.NewSymbol("first"), values.NewSymbol("second"), values.NewSymbol("third")),
		},
		{
			in:  values.NewArrayList(values.NewSymbol("first"), values.NewSymbol("second"), values.EmptyList),
			out: values.List(values.NewSymbol("first"), values.NewSymbol("second")),
		},
		{
			in:  values.NewArrayList(values.NewSymbol("first"), values.EmptyList),
			out: values.List(values.NewSymbol("first")),
		},
		{
			in:  values.NewArrayList(values.NewSymbol("first"), values.NewSymbol("cdr")),
			out: values.NewCons(values.NewSymbol("first"), values.NewSymbol("cdr")),
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			q := tc.in.AsList()
			qt.Assert(t, q, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestArrayList_Datum(t *testing.T) {
	a := values.NewArrayList(values.NewInteger(1), values.NewInteger(2))
	datum := a.Datum()
	qt.Assert(t, len(datum), qt.Equals, 2)
	qt.Assert(t, datum[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, datum[1], valuestest.SchemeEquals, values.NewInteger(2))
}

func TestArrayList_Car(t *testing.T) {
	a := values.NewArrayList(values.NewInteger(42), values.NewInteger(99))
	qt.Assert(t, a.Car(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestArrayList_Cdr(t *testing.T) {
	c := qt.New(t)

	t.Run("proper list", func(t *testing.T) {
		// (1 2 3)
		a := values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.EmptyList)
		cdr := a.Cdr()
		cdrList, ok := cdr.(*values.ArrayList)
		c.Assert(ok, qt.IsTrue, qt.Commentf("Cdr of multi-element proper list should return ArrayList"))
		c.Assert(len(*cdrList), qt.Equals, 3)
		c.Assert((*cdrList)[0], valuestest.SchemeEquals, values.NewInteger(2))
		c.Assert((*cdrList)[1], valuestest.SchemeEquals, values.NewInteger(3))
		c.Assert((*cdrList)[2], valuestest.SchemeEquals, values.EmptyList)
	})

	t.Run("improper list", func(t *testing.T) {
		// (42 . 99)
		a := values.NewArrayList(values.NewInteger(42), values.NewInteger(99))
		cdr := a.Cdr()
		// Should return the direct value, not wrapped in ArrayList
		c.Assert(cdr, valuestest.SchemeEquals, values.NewInteger(99), qt.Commentf("Cdr of improper list should return terminator directly"))
	})

	t.Run("single element proper list", func(t *testing.T) {
		// (42)
		a := values.NewArrayList(values.NewInteger(42), values.EmptyList)
		cdr := a.Cdr()
		c.Assert(cdr, valuestest.SchemeEquals, values.EmptyList, qt.Commentf("Cdr of (42) should return EmptyList"))
	})
}

func TestArrayList_ForEach(t *testing.T) {
	// ArrayList: [1, 2, EmptyList] - proper list (1 2)
	// ForEach should visit elements 1 and 2, NOT the EmptyList terminator
	a := values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.EmptyList)
	count := 0
	sum := int64(0)
	a.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, v values.Value) error { //nolint:errcheck
		count++
		intVal, ok := v.(*values.Integer)
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
		list := values.NewArrayList(values.NewInteger(5), values.NewInteger(10), values.EmptyList)

		visited := []values.Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 2, qt.Commentf("should visit 2 elements, not 3"))
		c.Assert(visited[0], valuestest.SchemeEquals, values.NewInteger(5))
		c.Assert(visited[1], valuestest.SchemeEquals, values.NewInteger(10))
		c.Assert(tail, valuestest.SchemeEquals, values.EmptyList, qt.Commentf("should return EmptyList as tail"))
	})

	t.Run("improper list - terminator not visited", func(t *testing.T) {
		// ArrayList: [5, 10, 999] (improper, 999 is the cdr)
		improperCdr := values.NewInteger(999)
		list := values.NewArrayList(values.NewInteger(5), values.NewInteger(10), improperCdr)

		visited := []values.Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 2, qt.Commentf("should visit 2 elements, not 3"))
		c.Assert(visited[0], valuestest.SchemeEquals, values.NewInteger(5))
		c.Assert(visited[1], valuestest.SchemeEquals, values.NewInteger(10))
		c.Assert(tail, valuestest.SchemeEquals, improperCdr, qt.Commentf("should return improper cdr as tail"))
	})

	t.Run("single element list", func(t *testing.T) {
		// ArrayList: [42, EmptyList]
		list := values.NewArrayList(values.NewInteger(42), values.EmptyList)

		visited := []values.Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 1)
		c.Assert(visited[0], valuestest.SchemeEquals, values.NewInteger(42))
		c.Assert(tail, valuestest.SchemeEquals, values.EmptyList)
	})

	t.Run("empty list", func(t *testing.T) {
		// ArrayList: [EmptyList] (just the terminator)
		list := values.NewArrayList(values.EmptyList)

		visited := []values.Value{}
		tail, err := list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			visited = append(visited, v)
			return nil
		})

		c.Assert(err, qt.IsNil)
		c.Assert(len(visited), qt.Equals, 0, qt.Commentf("empty list should not visit any elements"))
		c.Assert(tail, valuestest.SchemeEquals, values.EmptyList)
	})

	t.Run("matches Pair.ForEach semantics", func(t *testing.T) {
		// Create equivalent Pair and ArrayList for (5 10)
		pair := values.List(values.NewInteger(5), values.NewInteger(10))
		arraylist := values.NewArrayList(values.NewInteger(5), values.NewInteger(10), values.EmptyList)

		// Collect elements from Pair
		pairElements := []values.Value{}
		pairTail, pairErr := pair.(*values.Pair).ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			pairElements = append(pairElements, v)
			return nil
		})

		// Collect elements from ArrayList
		arraylistElements := []values.Value{}
		arraylistTail, arraylistErr := arraylist.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			arraylistElements = append(arraylistElements, v)
			return nil
		})

		// Both should have identical results
		c.Assert(pairErr, qt.IsNil)
		c.Assert(arraylistErr, qt.IsNil)
		c.Assert(len(arraylistElements), qt.Equals, len(pairElements), qt.Commentf("should visit same number of elements"))
		c.Assert(arraylistElements[0], valuestest.SchemeEquals, pairElements[0])
		c.Assert(arraylistElements[1], valuestest.SchemeEquals, pairElements[1])
		c.Assert(arraylistTail, valuestest.SchemeEquals, pairTail, qt.Commentf("should return same tail"))
	})
}

func TestArrayList_AsVector(t *testing.T) {
	a := values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	v := a.AsVector()
	qt.Assert(t, len(*v), qt.Equals, 3)
	qt.Assert(t, (*v)[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, (*v)[1], valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, (*v)[2], valuestest.SchemeEquals, values.NewInteger(3))
}

func TestArrayList_Append_Single(t *testing.T) {
	a := values.NewArrayList(values.NewInteger(1), values.NewInteger(2))
	result := a.Append(values.NewInteger(3))
	resultList, ok := result.(*values.ArrayList)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, len(*resultList), qt.Equals, 3)
	qt.Assert(t, (*resultList)[2], valuestest.SchemeEquals, values.NewInteger(3))
}

// TestArrayList_PairEquivalence verifies that Pair and ArrayList are indistinguishable
// when using Car(), Cdr(), SetCar(), and SetCdr() operations.
func TestArrayList_PairEquivalence(t *testing.T) {
	c := qt.New(t)

	t.Run("proper list (1 2 3)", func(t *testing.T) {
		pair := values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewCons(values.NewInteger(3), values.EmptyList)))
		arraylist := values.NewArrayList(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.EmptyList)

		// Car should be identical
		c.Assert(pair.Car(), valuestest.SchemeEquals, arraylist.Car())

		// Cdr should return equivalent structures
		pairCdr := pair.Cdr().(*values.Pair)
		arraylistCdr := arraylist.Cdr().(*values.ArrayList)
		c.Assert(pairCdr.Car(), valuestest.SchemeEquals, arraylistCdr.Car())
		c.Assert(pairCdr.Cdr().(*values.Pair).Car(), valuestest.SchemeEquals, arraylistCdr.Cdr().(*values.ArrayList).Car())
	})

	t.Run("improper list (1 . 2)", func(t *testing.T) {
		pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
		arraylist := values.NewArrayList(values.NewInteger(1), values.NewInteger(2))

		// Car should be identical
		c.Assert(pair.Car(), valuestest.SchemeEquals, arraylist.Car())

		// Cdr should return the direct value, not a wrapped structure
		c.Assert(pair.Cdr(), valuestest.SchemeEquals, values.NewInteger(2))
		c.Assert(arraylist.Cdr(), valuestest.SchemeEquals, values.NewInteger(2))
		c.Assert(pair.Cdr(), valuestest.SchemeEquals, arraylist.Cdr(), qt.Commentf("Pair and ArrayList Cdr must return identical values for improper lists"))
	})

	t.Run("single element proper list (42)", func(t *testing.T) {
		pair := values.NewCons(values.NewInteger(42), values.EmptyList)
		arraylist := values.NewArrayList(values.NewInteger(42), values.EmptyList)

		c.Assert(pair.Car(), valuestest.SchemeEquals, arraylist.Car())
		c.Assert(pair.Cdr(), valuestest.SchemeEquals, values.EmptyList)
		c.Assert(arraylist.Cdr(), valuestest.SchemeEquals, values.EmptyList)
		c.Assert(pair.Cdr(), valuestest.SchemeEquals, arraylist.Cdr())
	})
}
