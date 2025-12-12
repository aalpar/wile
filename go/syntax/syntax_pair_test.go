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


package syntax

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxPair_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *SyntaxPair
		out string
	}{
		{nil, "#<syntax-void>"},
		{NewSyntaxCons(nil, nil, nil), "#'()"},
		{NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(2), nil), NewSyntaxEmptyList(nil), nil), nil), "#'(#'1 #'2)"},
		{NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(2), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(3), nil), NewSyntaxEmptyList(nil), nil), nil), nil), "#'(#'1 #'2 #'3)"},
		{NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxObject(values.NewInteger(2), nil), nil), NewSyntaxEmptyList(nil), nil), "#'(#'(#'1 . #'2))"},
		{NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), (*SyntaxPair)(nil), nil), NewSyntaxEmptyList(nil), nil), "#'(#'(#'1 . #<syntax-void>))"},
	}

	for _, tc := range tcs {
		t.Run(tc.out, func(t *testing.T) {
			got := tc.in.SchemeString()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestSyntaxPair_EqualTo(t *testing.T) {
	// EqualTo uses pointer comparison - only same object is equal
	t.Run("nil equals nil", func(t *testing.T) {
		var a, b *SyntaxPair
		qt.Assert(t, a.EqualTo(b), qt.IsTrue)
	})

	t.Run("same object equals itself", func(t *testing.T) {
		p := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil)
		qt.Assert(t, p.EqualTo(p), qt.IsTrue)
	})

	t.Run("different objects with same content not equal", func(t *testing.T) {
		p1 := NewSyntaxEmptyList(nil)
		p2 := NewSyntaxEmptyList(nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("different objects with different content not equal", func(t *testing.T) {
		p1 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil)
		p2 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("nil not equal to non-nil", func(t *testing.T) {
		var p1 *SyntaxPair
		p2 := NewSyntaxEmptyList(nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("wrong type returns false", func(t *testing.T) {
		p := NewSyntaxEmptyList(nil)
		qt.Assert(t, p.EqualTo(values.NewInteger(10)), qt.IsFalse)
	})
}

func TestSyntaxPair_NewSyntaxCons(t *testing.T) {
	pr := NewSyntaxCons(nil, nil, nil)
	qt.Assert(t, pr, qt.Not(qt.IsNil))
}

func TestSyntaxPair_IsList(t *testing.T) {
	tcs := []struct {
		in  *SyntaxPair
		out bool
	}{
		{in: nil, out: false},
		{in: SyntaxEmptyList, out: true},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil), out: true},
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil), NewSyntaxEmptyList(nil), nil), out: true},
		// List with nested cons as first element: ((10) 20) - proper list
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil), nil), out: true},
		// Improper list: (10 20 . 30)
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil), nil),
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

func TestSyntaxPair_Length(t *testing.T) {
	tcs := []struct {
		in           *SyntaxPair
		out          int
		panicMatches string
	}{
		{in: nil,
			panicMatches: "not a list",
			out:          -1},
		{in: NewSyntaxEmptyList(nil), out: 0},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil), out: 1},
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil), NewSyntaxEmptyList(nil), nil), out: 1},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil), nil), out: 2},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil), nil),
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

func TestSyntaxPair_IsVoid(t *testing.T) {
	tcs := []struct {
		in  *values.Pair
		out bool
	}{
		{in: nil, out: true},
		{in: values.EmptyList, out: false},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsVoid()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestSyntaxPair_IsEmptyList(t *testing.T) {
	tcs := []struct {
		in  *values.Pair
		out bool
	}{
		{in: nil, out: false},
		{in: values.EmptyList, out: true},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			got := tc.in.IsEmptyList()
			qt.Assert(t, got, qt.Equals, tc.out)
		})
	}
}

func TestSyntaxPair_AsVector(t *testing.T) {
	tcs := []struct {
		name         string
		in           *SyntaxPair
		out          *values.Vector
		panicMatches string
	}{
		{
			name: "nil returns nil",
			in:   nil,
			out:  nil,
		},
		{
			name: "empty list returns empty vector",
			in:   NewSyntaxEmptyList(nil),
			out:  values.NewVector(),
		},
		{
			name: "single element list",
			in:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			out:  values.NewVector(values.NewInteger(10)),
		},
		{
			name: "two element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(20), nil),
					NewSyntaxEmptyList(nil), nil), nil),
			out: values.NewVector(values.NewInteger(10), values.NewInteger(20)),
		},
		{
			name: "three element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(2), nil),
					NewSyntaxCons(
						NewSyntaxObject(values.NewInteger(3), nil),
						NewSyntaxEmptyList(nil), nil), nil), nil),
			out: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "nested list unwraps to regular values",
			in: NewSyntaxCons(
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(1), nil),
					NewSyntaxCons(
						NewSyntaxObject(values.NewInteger(2), nil),
						NewSyntaxEmptyList(nil), nil), nil),
				NewSyntaxEmptyList(nil), nil),
			out: values.NewVector(values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.EmptyList))),
		},
		{
			name: "mixed types",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewString("hello"), nil),
					NewSyntaxCons(
						NewSyntaxObject(values.TrueValue, nil),
						NewSyntaxEmptyList(nil), nil), nil), nil),
			out: values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
		{
			name: "improper list panics",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxObject(values.NewInteger(2), nil), nil),
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
					qt.Assert(t, got.EqualTo(tc.out), qt.IsTrue)
				}
			}
		})
	}
}

func TestSyntaxPair_AsSyntaxVector(t *testing.T) {
	tcs := []struct {
		name         string
		in           *SyntaxPair
		outLen       int
		panicMatches string
	}{
		{
			name:   "nil returns nil",
			in:     nil,
			outLen: -1, // special marker for nil
		},
		{
			name:   "empty list returns empty syntax vector",
			in:     NewSyntaxEmptyList(nil),
			outLen: 0,
		},
		{
			name:   "single element list",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			outLen: 1,
		},
		{
			name: "two element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(20), nil),
					NewSyntaxEmptyList(nil), nil), nil),
			outLen: 2,
		},
		{
			name: "three element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(2), nil),
					NewSyntaxCons(
						NewSyntaxObject(values.NewInteger(3), nil),
						NewSyntaxEmptyList(nil), nil), nil), nil),
			outLen: 3,
		},
		{
			name: "nested syntax pair as element",
			in: NewSyntaxCons(
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(1), nil),
					NewSyntaxEmptyList(nil), nil),
				NewSyntaxEmptyList(nil), nil),
			outLen: 1,
		},
		{
			name: "improper list panics",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxObject(values.NewInteger(2), nil), nil),
			panicMatches: "not a syntax list",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() { tc.in.AsSyntaxVector() }, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.AsSyntaxVector()
				if tc.outLen == -1 {
					qt.Assert(t, got, qt.IsNil)
				} else {
					qt.Assert(t, got, qt.Not(qt.IsNil))
					qt.Assert(t, len(got.Values), qt.Equals, tc.outLen)
				}
			}
		})
	}
}

func TestSyntaxPair_AsSyntaxVector_PreservesSyntaxValues(t *testing.T) {
	// Create a list of syntax values with source context
	sc := NewSourceContext("foo", "test.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(3, 3, 1))
	elem1 := NewSyntaxSymbol("foo", sc)
	elem2 := NewSyntaxSymbol("bar", sc)
	list := NewSyntaxCons(elem1, NewSyntaxCons(elem2, NewSyntaxEmptyList(sc), sc), sc)

	vec := list.AsSyntaxVector()

	qt.Assert(t, vec, qt.Not(qt.IsNil))
	qt.Assert(t, len(vec.Values), qt.Equals, 2)

	// Check that the elements are the original syntax values
	qt.Assert(t, vec.Values[0], qt.Equals, elem1)
	qt.Assert(t, vec.Values[1], qt.Equals, elem2)
}

func TestSyntaxPair_Append(t *testing.T) {
	tcs := []struct {
		name         string
		in           *SyntaxPair
		vs           SyntaxValue
		out          SyntaxValue
		panicMatches string
		expect       bool
	}{
		{
			name:         "1",
			in:           (*SyntaxPair)(nil),
			vs:           (*SyntaxPair)(nil),
			out:          (*SyntaxPair)(nil),
			panicMatches: "not a list",
			expect:       false,
		},
		{
			name:   "4",
			in:     NewSyntaxEmptyList(nil),
			vs:     NewSyntaxObject(values.NewInteger(10), nil),
			out:    NewSyntaxObject(values.NewInteger(10), nil),
			expect: true,
		},
		{
			name: "5",
			in:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			vs:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil),
			out: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil),
					NewSyntaxEmptyList(nil), nil), nil),
			expect: true,
		},
		{
			name:   "6",
			in:     NewSyntaxEmptyList(nil),
			vs:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			expect: true,
		},
		{
			name:   "7",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			vs:     NewSyntaxEmptyList(nil),
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			expect: true,
		},
		{
			name:   "8",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			vs:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil),
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxEmptyList(nil), nil), nil),
			expect: true,
		},
		{
			name:   "9",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxEmptyList(nil), nil),
			vs:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil),
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil), nil),
			expect: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.panicMatches != "" {
				qt.Assert(t, func() {
					tc.in.SyntaxAppend(tc.vs)
				}, qt.PanicMatches, tc.panicMatches)
			} else {
				got := tc.in.SyntaxAppend(tc.vs)
				qt.Assert(t, got.UnwrapAll().EqualTo(tc.out.UnwrapAll()), qt.Equals, tc.expect)
			}
		})
	}
}
