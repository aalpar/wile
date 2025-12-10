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
