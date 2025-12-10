package values

import (
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
