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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestPair_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *values.Pair
		out string
	}{
		{nil, "#<void>"},
		{values.NewCons(nil, nil), "(#<void> . #<void>)"},
		{values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList)), "(1 2)"},
		{values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewCons(values.NewInteger(3), values.EmptyList))), "(1 2 3)"},
		{values.NewCons(values.NewCons(values.NewInteger(1), values.NewInteger(2)), values.EmptyList), "((1 . 2))"},
		{values.NewCons(values.NewCons(values.NewInteger(1), (*values.Pair)(nil)), values.EmptyList), "((1 . #<void>))"},
	}

	for _, tc := range tcs {
		got := tc.in.SchemeString()
		qt.Assert(t, got, qt.Equals, tc.out)
	}
}

func TestPair_EqualTo(t *testing.T) {
	tcs := []struct {
		nm  string
		in0 *values.Pair
		in1 values.Value
		out bool
	}{
		{
			nm:  "1",
			in0: (*values.Pair)(nil),
			in1: (*values.Pair)(nil),
			out: true,
		},
		{
			nm:  "3",
			in0: &values.Pair{nil, nil},
			in1: &values.Pair{nil, nil},
			out: true,
		},
		{
			nm:  "5",
			in0: (*values.Pair)(nil),
			in1: (*values.Pair)(nil),
			out: true,
		},
		{
			nm:  "6",
			in0: values.NewCons(values.NewInteger(10), values.EmptyList),
			in1: values.NewCons(values.NewInteger(10), values.EmptyList),
			out: true,
		},
		{
			nm:  "7",
			in0: values.NewCons(values.NewInteger(10), (*values.Pair)(nil)),
			in1: values.NewCons(values.NewInteger(10), values.Value(nil)),
			out: true,
		},
		{
			nm:  "8",
			in0: values.NewCons(values.NewInteger(10), (*values.Pair)(nil)),
			in1: values.NewCons(values.NewInteger(10), values.Void),
			out: true,
		},
		{
			nm:  "9",
			in0: values.NewCons(values.NewCons(values.NewInteger(10), values.EmptyList), values.EmptyList),
			in1: values.NewCons(values.NewCons(values.NewInteger(10), values.EmptyList), values.EmptyList),
			out: true,
		},
		{
			nm:  "10",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			out: true,
		},
		{
			nm:  "11",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(30), values.EmptyList)),
			out: false,
		},
		{
			nm:  "12",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(30), values.EmptyList)),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			out: false,
		},
		{
			nm:  "13",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			out: true,
		},
		{
			nm:  "14",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			out: false,
		},
		{
			nm:  "15",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			out: false,
		},
		{
			nm:  "16",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.Void)),
			out: false,
		},
		{
			nm:  "17",
			in0: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.Void)),
			in1: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
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
	qt.Assert(t, values.EmptyList.EqualTo(values.EmptyList), qt.IsTrue)
	qt.Assert(t, values.EqualTo(values.EmptyList, values.EmptyList), qt.IsTrue)
	qt.Assert(t, values.EmptyList.EqualTo(values.NewCons(values.NewInteger(1), values.EmptyList)), qt.IsFalse)
}

func TestPair_NewCons(t *testing.T) {
	pr := values.NewCons(nil, nil)
	qt.Assert(t, pr, qt.Not(qt.IsNil))
}

func TestPair_IsList(t *testing.T) {
	tcs := []struct {
		in  *values.Pair
		out bool
	}{
		{in: nil, out: false},
		{in: values.NewCons(values.NewInteger(10), values.EmptyList), out: true},
		{in: values.NewCons(values.NewCons(values.NewInteger(10), values.EmptyList), values.EmptyList), out: true},
		{in: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)), out: true},
		{
			in:  values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
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
	qt.Assert(t, values.EmptyList.IsList(), qt.IsTrue)
}

func TestPair_IsList_Circular(t *testing.T) {
	// Test that circular lists return false (R7RS §6.4)
	// This tests Floyd's cycle detection algorithm

	t.Run("self-referential", func(t *testing.T) {
		// (set-cdr! x x) - cdr points to self
		p := values.NewCons(values.NewSymbol("a"), values.EmptyList)
		p.SetCdr(p)
		qt.Assert(t, p.IsList(), qt.Equals, false)
	})

	t.Run("cycle after one element", func(t *testing.T) {
		// (a . #0=(b . #0#))
		p1 := values.NewCons(values.NewSymbol("a"), values.EmptyList)
		p2 := values.NewCons(values.NewSymbol("b"), values.EmptyList)
		p1.SetCdr(p2)
		p2.SetCdr(p2) // p2 points to itself
		qt.Assert(t, p1.IsList(), qt.Equals, false)
	})

	t.Run("cycle back to start", func(t *testing.T) {
		// #0=(a b . #0#)
		p1 := values.NewCons(values.NewSymbol("a"), values.EmptyList)
		p2 := values.NewCons(values.NewSymbol("b"), values.EmptyList)
		p1.SetCdr(p2)
		p2.SetCdr(p1) // cycle back to start
		qt.Assert(t, p1.IsList(), qt.Equals, false)
	})

	t.Run("longer cycle", func(t *testing.T) {
		// (a b c d . #0=(e f . #0#))
		cells := make([]*values.Pair, 6)
		for i := range cells {
			cells[i] = values.NewCons(values.NewInteger(int64(i)), values.EmptyList)
		}
		for i := range 5 {
			cells[i].SetCdr(cells[i+1])
		}
		cells[5].SetCdr(cells[4]) // cycle between last two
		qt.Assert(t, cells[0].IsList(), qt.Equals, false)
	})
}

func TestPair_Length(t *testing.T) {
	tcs := []struct {
		in           *values.Pair
		out          int
		panicMatches string
	}{
		{in: nil, out: 0},
		{in: values.NewCons(values.NewInteger(10), values.EmptyList), out: 1},
		{in: values.NewCons(values.NewCons(values.NewInteger(10), values.EmptyList), values.EmptyList), out: 1},
		{in: values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)), out: 2},
		{
			in:           values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
			panicMatches: ".*not a list",
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
	qt.Assert(t, values.EmptyList.Length(), qt.Equals, 0)
}

func TestPair_IsVoid(t *testing.T) {
	qt.Assert(t, (*values.Pair)(nil).IsVoid(), qt.IsTrue)
	qt.Assert(t, values.NewCons(values.NewInteger(1), values.EmptyList).IsVoid(), qt.IsFalse)
}

func TestPair_IsEmptyList(t *testing.T) {
	// *Pair.IsEmptyList() always returns false now that EmptyList is a separate type
	qt.Assert(t, (*values.Pair)(nil).IsEmptyList(), qt.IsFalse)
	qt.Assert(t, values.NewCons(values.NewInteger(1), values.EmptyList).IsEmptyList(), qt.IsFalse)
}

func TestEmptyList_IsVoidAndIsEmptyList(t *testing.T) {
	qt.Assert(t, values.EmptyList.IsEmptyList(), qt.IsTrue)
	qt.Assert(t, values.EmptyList.IsVoid(), qt.IsFalse)
	qt.Assert(t, values.IsEmptyList(values.EmptyList), qt.IsTrue)
	qt.Assert(t, values.IsVoid(values.EmptyList), qt.IsFalse)
}

func TestEmptyList_AsVector(t *testing.T) {
	got := values.EmptyList.AsVector()
	qt.Assert(t, got, qt.Not(qt.IsNil))
	qt.Assert(t, got.Length(), qt.Equals, 0)
}

func TestEmptyList_Append(t *testing.T) {
	// Appending to empty list returns the argument
	got := values.EmptyList.Append(values.NewCons(values.NewInteger(10), values.EmptyList))
	qt.Assert(t, got, valuestest.SchemeEquals, values.NewCons(values.NewInteger(10), values.EmptyList))

	// Appending nil/void returns it
	got = values.EmptyList.Append((*values.Pair)(nil))
	qt.Assert(t, got, qt.Equals, values.Value((*values.Pair)(nil)))
}

func TestEmptyList_SchemeString(t *testing.T) {
	qt.Assert(t, values.EmptyList.SchemeString(), qt.Equals, "()")
}

func TestEmptyList_Car_Panics(t *testing.T) {
	qt.Assert(t, func() { values.EmptyList.Car() }, qt.PanicMatches, ".*not a pair")
}

func TestEmptyList_Cdr_Panics(t *testing.T) {
	qt.Assert(t, func() { values.EmptyList.Cdr() }, qt.PanicMatches, ".*not a pair")
}

func TestPair_AsVector(t *testing.T) {
	tcs := []struct {
		name         string
		in           *values.Pair
		out          *values.Vector
		panicMatches string
	}{
		{
			name: "nil returns nil",
			in:   nil,
			out:  nil,
		},
		{
			name:         "void pair panics",
			in:           values.NewCons(nil, nil),
			panicMatches: ".*not a list",
		},
		{
			name: "single element list",
			in:   values.NewCons(values.NewInteger(10), values.EmptyList),
			out:  values.NewVector(values.NewInteger(10)),
		},
		{
			name: "two element list",
			in:   values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
			out:  values.NewVector(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			name: "three element list",
			in:   values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewCons(values.NewInteger(3), values.EmptyList))),
			out:  values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "nested list as element",
			in:   values.NewCons(values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList)), values.EmptyList),
			out:  values.NewVector(values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList))),
		},
		{
			name: "mixed types",
			in:   values.NewCons(values.NewInteger(1), values.NewCons(values.NewString("hello"), values.NewCons(values.TrueValue, values.EmptyList))),
			out:  values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
		{
			name:         "improper list panics",
			in:           values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			panicMatches: ".*not a list",
		},
		{
			name:         "improper list with multiple elements panics",
			in:           values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3))),
			panicMatches: ".*not a list",
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
					qt.Assert(t, got, valuestest.SchemeEquals, tc.out)
				}
			}
		})
	}
}

func TestPair_Append(t *testing.T) {
	tcs := []struct {
		name         string
		in           *values.Pair
		vs           values.Value
		out          values.Value
		panicMatches string
	}{
		{
			name:         "nil input",
			in:           (*values.Pair)(nil),
			vs:           (*values.Pair)(nil),
			out:          (*values.Pair)(nil),
			panicMatches: ".*not a list",
		},
		{
			name:         "void pair input",
			in:           values.NewCons(nil, nil),
			vs:           (*values.Pair)(nil),
			panicMatches: ".*not a list",
		},
		{
			name: "empty vs with nil",
			in:   values.NewCons(values.NewInteger(10), values.EmptyList),
			vs:   values.EmptyList,
			out:  values.NewCons(values.NewInteger(10), values.EmptyList),
		},
		{
			name: "append to empty list",
			in:   values.NewCons(values.NewInteger(10), values.EmptyList),
			vs:   values.NewCons(values.NewInteger(20), values.EmptyList),
			out:  values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.EmptyList)),
		},
		{
			name: "append to empty list with nil",
			in:   values.NewCons(values.NewInteger(10), values.EmptyList),
			vs:   values.NewCons(values.NewInteger(20), values.NewInteger(30)),
			out:  values.NewCons(values.NewInteger(10), values.NewCons(values.NewInteger(20), values.NewInteger(30))),
		},
		{
			name:         "append to non-list pair",
			in:           values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			vs:           values.NewCons(values.NewInteger(3), values.EmptyList),
			panicMatches: ".*not a list",
		},
		{
			name: "append non-list value",
			in:   values.NewCons(values.NewInteger(1), values.EmptyList),
			vs:   values.NewInteger(2),
			out:  values.NewCons(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "append Void to list",
			in:   values.NewCons(values.NewInteger(1), values.EmptyList),
			vs:   values.Void,
			out:  values.NewCons(values.NewInteger(1), values.Void),
		},
		{
			name:         "append Void to non-list",
			in:           values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			vs:           values.Void,
			panicMatches: ".*not a list",
		},
		{
			name:         "append Void to list",
			in:           values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			vs:           values.Void,
			panicMatches: ".*not a list",
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
				qt.Assert(t, got, valuestest.SchemeEquals, tc.out)
			}
		})
	}
}

func TestPair_Append_DoesNotMutateOriginal(t *testing.T) {
	c := qt.New(t)

	// Create list (1 2 3)
	original := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	// Append (4 5)
	toAppend := values.List(values.NewInteger(4), values.NewInteger(5))
	result := original.(*values.Pair).Append(toAppend)

	// Verify result is correct
	expected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))
	c.Assert(result, valuestest.SchemeEquals, expected)

	// Verify original is unchanged
	originalExpected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	c.Assert(original, valuestest.SchemeEquals, originalExpected)

	// Verify they don't share spine by mutating result
	result.(*values.Pair).SetCar(values.NewInteger(99))
	c.Assert(original.(*values.Pair).Car(), valuestest.SchemeEquals, values.NewInteger(1),
		qt.Commentf("original should still have 1, not 99"))
	c.Assert(result.(*values.Pair).Car(), valuestest.SchemeEquals, values.NewInteger(99),
		qt.Commentf("result should have 99"))
}

func TestPair_Append_SharesStructureWithLastArgument(t *testing.T) {
	c := qt.New(t)

	// R7RS §6.4: the last argument shares structure
	list1 := values.List(values.NewInteger(1), values.NewInteger(2))
	list2 := values.List(values.NewInteger(3), values.NewInteger(4))

	result := list1.(*values.Pair).Append(list2)

	// The tail of result should be the same pointer as list2
	// result is (1 2 3 4), so cdr of cdr should point to list2
	resultTail := result.(*values.Pair).Cdr().(*values.Pair).Cdr()
	c.Assert(resultTail, qt.Equals, list2,
		qt.Commentf("last argument should share structure"))
}

func TestPair_Append_EmptyList(t *testing.T) {
	c := qt.New(t)

	// Appending empty list should return the original list
	// but R7RS allows returning p when vs is empty since no mutation occurs
	original := values.List(values.NewInteger(1), values.NewInteger(2))
	result := original.(*values.Pair).Append(values.EmptyList)

	// Verify result equals original
	c.Assert(result, valuestest.SchemeEquals, original)

	// When vs is empty, returning p is allowed (no copy needed)
	// because there's no mutation risk
	c.Assert(result, qt.Equals, original,
		qt.Commentf("when appending empty list, can return original"))
}

func TestPair_Append_MultipleElements(t *testing.T) {
	c := qt.New(t)

	// Test appending longer lists
	list1 := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	list2 := values.List(values.NewInteger(4), values.NewInteger(5), values.NewInteger(6), values.NewInteger(7))
	result := list1.(*values.Pair).Append(list2)

	expected := values.List(
		values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
		values.NewInteger(4), values.NewInteger(5), values.NewInteger(6), values.NewInteger(7),
	)
	c.Assert(result, valuestest.SchemeEquals, expected)

	// Verify original list1 is unchanged
	c.Assert(list1, valuestest.SchemeEquals, values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))

	// Verify structure sharing with list2
	resultTail := result.(*values.Pair).Cdr().(*values.Pair).Cdr().(*values.Pair).Cdr()
	c.Assert(resultTail, qt.Equals, list2)
}

func TestPair_Car(t *testing.T) {
	p := values.NewCons(values.NewInteger(42), values.NewInteger(99))
	qt.Assert(t, p.Car(), valuestest.SchemeEquals, values.NewInteger(42))

	p2 := values.NewCons(values.NewString("hello"), values.EmptyList)
	qt.Assert(t, p2.Car(), valuestest.SchemeEquals, values.NewString("hello"))
}

func TestPair_SetCar(t *testing.T) {
	p := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	p.SetCar(values.NewInteger(10))
	qt.Assert(t, p.Car(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, p.Cdr(), valuestest.SchemeEquals, values.NewInteger(2))
}

func TestPair_SetCdr(t *testing.T) {
	p := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	p.SetCdr(values.NewInteger(20))
	qt.Assert(t, p.Car(), valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, p.Cdr(), valuestest.SchemeEquals, values.NewInteger(20))
}

func TestPair_String(t *testing.T) {
	p := values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList))
	s := p.String()
	qt.Assert(t, s, qt.Equals, "(1 2)")

	p2 := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	s2 := p2.String()
	qt.Assert(t, s2, qt.Equals, "(1 . 2)")
}

func TestPair_SchemeString_CircularCdr(t *testing.T) {
	// (a . <self>) — simplest circular cdr
	p := values.NewCons(values.NewSymbol("a"), values.EmptyList)
	p.SetCdr(p)
	got := p.SchemeString()
	qt.Assert(t, got, qt.Equals, "(a . ...)")
}

func TestPair_SchemeString_CircularCar(t *testing.T) {
	// (<self> . b)
	p := values.NewCons(values.EmptyList, values.NewSymbol("b"))
	p.SetCar(p)
	got := p.SchemeString()
	qt.Assert(t, got, qt.Equals, "(... . b)")
}

func TestPair_SchemeString_CircularMultiElement(t *testing.T) {
	// (a b c . <head>) — circular list with multiple elements
	c := values.NewCons(values.NewSymbol("c"), values.EmptyList)
	b := values.NewCons(values.NewSymbol("b"), c)
	a := values.NewCons(values.NewSymbol("a"), b)
	c.SetCdr(a)
	got := a.SchemeString()
	qt.Assert(t, got, qt.Equals, "(a b c . ...)")
}

func TestPair_String_CircularCdr(t *testing.T) {
	// Same test for String() which uses fmt.Stringer dispatch
	p := values.NewCons(values.NewSymbol("a"), values.EmptyList)
	p.SetCdr(p)
	got := p.String()
	qt.Assert(t, got, qt.Equals, "(a . ...)")
}

func TestPair_String_CircularCar(t *testing.T) {
	p := values.NewCons(values.EmptyList, values.NewSymbol("b"))
	p.SetCar(p)
	got := p.String()
	qt.Assert(t, got, qt.Equals, "(... . b)")
}

func TestPair_SchemeString_NonCircularRegression(t *testing.T) {
	// Verify non-circular pairs produce identical output to before
	tcs := []struct {
		name string
		pair *values.Pair
		want string
	}{
		{"proper list", values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList)), "(1 2)"},
		{"improper pair", values.NewCons(values.NewInteger(1), values.NewInteger(2)), "(1 . 2)"},
		{"nested", values.NewCons(values.NewCons(values.NewInteger(1), values.EmptyList), values.EmptyList), "((1))"},
		{"single", values.NewCons(values.NewSymbol("x"), values.EmptyList), "(x)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.pair.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// nestPairs builds n nested single-element pairs: ((( … ()))) with n opening
// pairs. The pairs sit at nesting depths 1..n; the innermost () sits at depth
// n+1. Used to probe the SchemeString/String host-safety depth bound.
func nestPairs(n int) *values.Pair {
	var v values.Value = values.EmptyList
	for range n {
		v = values.NewCons(v, values.EmptyList)
	}
	return v.(*values.Pair)
}

// deepMarker mirrors the unexported values.deepMarker; asserted by literal
// because these are external (package values_test) tests.
const deepMarker = "#<deep>"

func TestPair_SchemeString_DepthBounded(t *testing.T) {
	// A structure nested far deeper than the bound must not overflow the host
	// Go stack; it degrades to the deep marker after DefaultMaxWriteDepth
	// levels of descent. 2,000,000 deep overflowed before the bound existed.
	got := nestPairs(2_000_000).SchemeString()
	prefix, _, found := strings.Cut(got, deepMarker)
	qt.Assert(t, found, qt.IsTrue)
	// Exactly DefaultMaxWriteDepth container levels are shown before the marker.
	qt.Assert(t, strings.Count(prefix, "("), qt.Equals, values.DefaultMaxWriteDepth)
	// The deep marker is distinct from the cycle marker.
	qt.Assert(t, deepMarker != "...", qt.IsTrue)
}

func TestPair_String_DepthBounded(t *testing.T) {
	// The fmt.Stringer twin shares the defect and the fix.
	got := nestPairs(2_000_000).String()
	prefix, _, found := strings.Cut(got, deepMarker)
	qt.Assert(t, found, qt.IsTrue)
	qt.Assert(t, strings.Count(prefix, "("), qt.Equals, values.DefaultMaxWriteDepth)
}

func TestPair_SchemeString_DepthBoundary(t *testing.T) {
	// The marker first appears once a child would sit past the bound: nesting
	// exactly DefaultMaxWriteDepth pairs pushes the innermost () to depth+1 >
	// bound; one fewer stays within and renders in full.
	atBound := nestPairs(values.DefaultMaxWriteDepth).SchemeString()
	qt.Assert(t, strings.Contains(atBound, deepMarker), qt.IsTrue)

	underBound := nestPairs(values.DefaultMaxWriteDepth - 1).SchemeString()
	qt.Assert(t, strings.Contains(underBound, deepMarker), qt.IsFalse)
}

func TestPair_SchemeString_FlatListNotBounded(t *testing.T) {
	// Length is not depth: a flat list of any size is nesting depth 1 and must
	// render in full — the iterative cdr-spine walk never touches the bound.
	elems := make([]values.Value, values.DefaultMaxWriteDepth*3)
	for i := range elems {
		elems[i] = values.NewInteger(int64(i))
	}
	got := values.List(elems...).(*values.Pair).SchemeString()
	qt.Assert(t, strings.Contains(got, deepMarker), qt.IsFalse)
}

func TestPair_SchemeString_DeepStructureThroughVector(t *testing.T) {
	// The bound is enforced at the shared schemeStringChild chokepoint, so a
	// chain alternating pair→vector→pair is bounded too, not just pure pairs.
	var v values.Value = values.EmptyList
	for range 1_000_000 {
		v = values.NewVector(v)
	}
	got := values.NewCons(v, values.EmptyList).SchemeString()
	qt.Assert(t, strings.Contains(got, deepMarker), qt.IsTrue)
}

// TestPair_SchemeString_SharedAcyclic pins path-scoped (not all-visited) cycle
// detection: a node reachable by two SIBLING paths (an acyclic DAG, i.e.
// structural sharing) must render in full at every occurrence. Only a node
// reachable from ITSELF (a true cycle, still on the current path) collapses to
// "...". This is the Phase-3 switch away from the old all-visited marking that
// mistook sharing for a cycle.
func TestPair_SchemeString_SharedAcyclic(t *testing.T) {
	// (let ((s (list 1 2))) (list s s)) — shared sublist, acyclic.
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	dag := values.List(shared, shared)
	qt.Assert(t, dag.SchemeString(), qt.Equals, "((1 2) (1 2))")
}

// TestPair_SchemeString_SharedSpineCdr pins that sharing reached through the
// cdr spine (not just the car) is rendered in full. The earlier bug marked
// every spine cdrPair forever, so a pair appearing in two sibling subtrees was
// wrongly collapsed to "...".
func TestPair_SchemeString_SharedSpineCdr(t *testing.T) {
	shared := values.List(values.NewInteger(7), values.NewInteger(8))
	// (shared . shared) where both halves are the SAME object, acyclic:
	// car renders (7 8); cdr is the same pair, spliced onto the spine → 7 8.
	dag := values.NewCons(shared, shared)
	qt.Assert(t, dag.SchemeString(), qt.Equals, "((7 8) 7 8)")
}

// TestPair_SchemeString_TrueCycleStillBounded confirms a real set-cdr! cycle
// is still bounded (renders "...", does not hang or overflow) after the switch
// to path-scoped marking.
func TestPair_SchemeString_TrueCycleStillBounded(t *testing.T) {
	a := values.NewCons(values.NewSymbol("a"), values.EmptyList)
	b := values.NewCons(values.NewSymbol("b"), values.EmptyList)
	a.SetCdr(b)
	b.SetCdr(a) // #0=(a b . #0#)
	got := a.SchemeString()
	qt.Assert(t, got, qt.Equals, "(a b . ...)")
}

func TestPair_ForEach(t *testing.T) {
	ctx := context.TODO()

	tcs := []struct {
		name         string
		input        *values.Pair
		wantTail     values.Value
		wantElements []values.Value
	}{
		{
			name:         "nil receiver",
			input:        (*values.Pair)(nil),
			wantTail:     values.EmptyList,
			wantElements: nil,
		},
		{
			name:         "proper list",
			input:        values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList)),
			wantTail:     values.EmptyList,
			wantElements: []values.Value{values.NewInteger(1), values.NewInteger(2)},
		},
		{
			name:         "improper list",
			input:        values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			wantTail:     values.NewInteger(2),
			wantElements: []values.Value{values.NewInteger(1)},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			var got []values.Value
			tail, err := tc.input.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
				got = append(got, v)
				return nil
			})
			c.Assert(err, qt.IsNil)
			c.Assert(tail, valuestest.SchemeEquals, tc.wantTail)
			c.Assert(len(got), qt.Equals, len(tc.wantElements))
			for i, want := range tc.wantElements {
				c.Assert(got[i], valuestest.SchemeEquals, want)
			}
		})
	}
}

// ── Spine ───────────────────────────────────────────────────────────────

func TestSpine(t *testing.T) {
	a := values.NewInteger(1)
	b := values.NewInteger(2)
	c := values.NewInteger(3)
	proper := values.NewCons(a, values.NewCons(b, values.NewCons(c, values.EmptyList)))
	improper := values.NewCons(a, values.NewCons(b, c)) // (1 2 . 3)
	single := values.NewCons(a, values.EmptyList)

	tcs := []struct {
		name     string
		input    *values.Pair
		wantCars []values.Value
		wantTail values.Value
	}{
		{"proper-3-elements", proper, []values.Value{a, b, c}, values.EmptyList},
		{"improper-2-plus-tail", improper, []values.Value{a, b}, c},
		{"single-element", single, []values.Value{a}, values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var cars []values.Value
			var tail values.Value
			for cell := range values.Spine(tc.input, &tail) {
				cars = append(cars, cell.Car())
			}
			qt.Assert(t, len(cars), qt.Equals, len(tc.wantCars))
			for i, want := range tc.wantCars {
				qt.Assert(t, cars[i], valuestest.SchemeEquals, want)
			}
			qt.Assert(t, tail, valuestest.SchemeEquals, tc.wantTail)
		})
	}
}

func TestSpineWithCycleCheck(t *testing.T) {
	// Proper list (1 2 3)
	a := values.NewInteger(1)
	b := values.NewInteger(2)
	c := values.NewInteger(3)
	proper := values.NewCons(a, values.NewCons(b, values.NewCons(c, values.EmptyList)))

	// Cycle: 1 -> 2 -> back to head
	cycleHead := values.NewCons(a, values.EmptyList)
	cycleSecond := values.NewCons(b, cycleHead)
	cycleHead.SetCdr(cycleSecond)

	tcs := []struct {
		name      string
		input     *values.Pair
		wantCells int // -1 = don't care
		wantCycle bool
	}{
		{"proper", proper, 3, false},
		{"cycle", cycleHead, -1, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var cycled bool
			cells := 0
			for range values.SpineWithCycleCheck(tc.input, &cycled) {
				cells++
				if cells > 100 {
					t.Fatal("infinite loop — cycle not detected")
				}
			}
			qt.Assert(t, cycled, qt.Equals, tc.wantCycle)
			if tc.wantCells >= 0 {
				qt.Assert(t, cells, qt.Equals, tc.wantCells)
			}
		})
	}
}
