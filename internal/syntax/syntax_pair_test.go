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

package syntax

import (
	"context"
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxPair_SchemeString(t *testing.T) {
	tcs := []struct {
		in  *SyntaxPair
		out string
	}{
		{nil, "#<syntax-void>"},
		// Note: the pre-migration test case
		//   {NewSyntaxCons(nil, nil, nil), "#'()"}
		// has been removed. *SyntaxPair.IsEmptyList() now returns false
		// unconditionally (matching *values.Pair), so the nil-nil pair
		// no longer renders as "#'()". The empty list at the syntax phase
		// is exclusively SyntaxEmptyList (= values.EmptyList), which
		// renders as "()" — its own SchemeString.
		{NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(2), nil), SyntaxEmptyList, nil), nil), "#'(#'1 #'2)"},
		{NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(2), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(3), nil), SyntaxEmptyList, nil), nil), nil), "#'(#'1 #'2 #'3)"},
		{NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxObject(values.NewInteger(2), nil), nil), SyntaxEmptyList, nil), "#'(#'(#'1 . #'2))"},
		{NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), (*SyntaxPair)(nil), nil), SyntaxEmptyList, nil), "#'(#'(#'1))"},
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
		p := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
		qt.Assert(t, p.EqualTo(p), qt.IsTrue)
	})

	t.Run("different objects with same content not equal", func(t *testing.T) {
		p1 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
		p2 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("different objects with different content not equal", func(t *testing.T) {
		p1 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
		p2 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("nil not equal to non-nil", func(t *testing.T) {
		var p1 *SyntaxPair
		p2 := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
		qt.Assert(t, p1.EqualTo(p2), qt.IsFalse)
	})

	t.Run("wrong type returns false", func(t *testing.T) {
		p := NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil)
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
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil), out: true},
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil), SyntaxEmptyList, nil), out: true},
		// List with nested cons as first element: ((10) 20) - proper list
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil), nil), out: true},
		// Improper list: (10 20 . 30)
		{
			in:  NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil), nil),
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
		{in: nil, out: 0},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil), out: 1},
		{in: NewSyntaxCons(NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil), SyntaxEmptyList, nil), out: 1},
		{in: NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil), nil), out: 2},
		{
			in:           NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), NewSyntaxObject(values.NewInteger(30), nil), nil), nil),
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

func TestSyntaxPair_IsVoid(t *testing.T) {
	// Test *SyntaxPair (the type this test file is for), not *values.Pair.
	// The earlier shape mistakenly tested *values.Pair — parallel
	// copy-paste defect to the one fixed in TestSyntaxPair_IsEmptyList.
	qt.Assert(t, (*SyntaxPair)(nil).IsVoid(), qt.IsTrue,
		qt.Commentf("nil receiver is void"))
	qt.Assert(t, NewSyntaxCons(
		NewSyntaxObject(values.NewInteger(1), nil),
		SyntaxEmptyList,
		nil,
	).IsVoid(), qt.IsFalse,
		qt.Commentf("a properly-constructed pair is not void"))
}

// TestSyntaxPair_UnwrapAll_NilNilPair pins the post-migration behavior on
// a nil-nil pair. Pre-migration, *SyntaxPair.IsEmptyList()'s short-circuit
// inside UnwrapAllShared returned values.EmptyList. Post-migration the
// short-circuit is gone, so UnwrapAll walks the pair and returns a
// *values.Pair with the singleton EmptyList substituted for the nil cdr.
//
// This is a documented behavior change with no production consumers (the
// nil-nil construction appears only in tests). The test fences the change
// so a future regression (e.g. someone resurrecting the
// IsEmptyList-on-nil-nil semantic in UnwrapAllShared) is loud.
func TestSyntaxPair_UnwrapAll_NilNilPair(t *testing.T) {
	pair := NewSyntaxCons(nil, nil, nil)
	result := pair.UnwrapAll()
	_, isPair := result.(*values.Pair)
	qt.Assert(t, isPair, qt.IsTrue,
		qt.Commentf("nil-nil SyntaxPair unwraps to *values.Pair, not EmptyList"))
	qt.Assert(t, values.IsEmptyList(result), qt.IsFalse,
		qt.Commentf("the unwrapped result is not the empty list"))
}

// TestSyntaxPair_Unwrap_NilNilPair pins the shallow-unwrap behavior.
// Unlike UnwrapAll which recurses, Unwrap just wraps Car and Cdr in a
// new *values.Pair — so a nil-nil SyntaxPair yields *values.Pair{nil, nil}.
// Not the empty list. Documents the cliff post-migration.
func TestSyntaxPair_Unwrap_NilNilPair(t *testing.T) {
	pair := NewSyntaxCons(nil, nil, nil)
	result := pair.Unwrap()
	_, isPair := result.(*values.Pair)
	qt.Assert(t, isPair, qt.IsTrue,
		qt.Commentf("Unwrap returns *values.Pair, not EmptyList"))
	qt.Assert(t, values.IsEmptyList(result), qt.IsFalse,
		qt.Commentf("the shallow-unwrapped result is not the empty list"))
}

// TestSyntaxPair_SchemeString_NilNilPair pins the post-migration panic
// behavior. Pre-migration the IsEmptyList short-circuit produced "#'()".
// Post-migration SyntaxForEach panics on the nil-cdr type assertion at
// pr.Cdr().(SyntaxValue). The audit confirms zero production callers
// construct nil-nil pairs; this test documents the failure shape so
// future contributors who accidentally construct one see the panic and
// trace it back to construction rather than wondering why a method
// quietly mis-rendered.
func TestSyntaxPair_SchemeString_NilNilPair(t *testing.T) {
	pair := NewSyntaxCons(nil, nil, nil)
	qt.Assert(t, func() { _ = pair.SchemeString() }, qt.PanicMatches,
		`interface conversion: interface is nil, not values\.SyntaxValue`)
}

// TestSyntaxPair_AsVector_NilNilPair pins the parallel panic behavior
// on AsVector. Same rationale as TestSyntaxPair_SchemeString_NilNilPair.
func TestSyntaxPair_AsVector_NilNilPair(t *testing.T) {
	pair := NewSyntaxCons(nil, nil, nil)
	qt.Assert(t, func() { _ = pair.AsVector() }, qt.PanicMatches,
		`interface conversion: interface is nil, not values\.SyntaxValue`)
}

func TestSyntaxPair_IsEmptyList(t *testing.T) {
	// *SyntaxPair.IsEmptyList() always returns false — the empty list
	// at the syntax phase is SyntaxEmptyList (an alias for
	// values.EmptyList), not a *SyntaxPair. Mirrors the *values.Pair
	// migration.
	qt.Assert(t, (*SyntaxPair)(nil).IsEmptyList(), qt.IsFalse)
	qt.Assert(t, NewSyntaxCons(nil, nil, nil).IsEmptyList(), qt.IsFalse,
		qt.Commentf("nil-nil pair is no longer treated as the empty list"))
	qt.Assert(t, NewSyntaxCons(
		NewSyntaxObject(values.NewInteger(1), nil),
		SyntaxEmptyList,
		nil,
	).IsEmptyList(), qt.IsFalse)
	// SyntaxEmptyList itself reports as empty (via the values.EmptyList
	// singleton's own IsEmptyList method).
	qt.Assert(t, SyntaxEmptyList.IsEmptyList(), qt.IsTrue)
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
			name: "single element list",
			in:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			out:  values.NewVector(values.NewInteger(10)),
		},
		{
			name: "two element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(20), nil),
					SyntaxEmptyList, nil), nil),
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
						SyntaxEmptyList, nil), nil), nil),
			out: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "nested list unwraps to regular values",
			in: NewSyntaxCons(
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(1), nil),
					NewSyntaxCons(
						NewSyntaxObject(values.NewInteger(2), nil),
						SyntaxEmptyList, nil), nil),
				SyntaxEmptyList, nil),
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
						SyntaxEmptyList, nil), nil), nil),
			out: values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
		{
			name: "improper list panics",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxObject(values.NewInteger(2), nil), nil),
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
			name:   "single element list",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			outLen: 1,
		},
		{
			name: "two element list",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(20), nil),
					SyntaxEmptyList, nil), nil),
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
						SyntaxEmptyList, nil), nil), nil),
			outLen: 3,
		},
		{
			name: "nested syntax pair as element",
			in: NewSyntaxCons(
				NewSyntaxCons(
					NewSyntaxObject(values.NewInteger(1), nil),
					SyntaxEmptyList, nil),
				SyntaxEmptyList, nil),
			outLen: 1,
		},
		{
			name: "improper list panics",
			in: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(1), nil),
				NewSyntaxObject(values.NewInteger(2), nil), nil),
			panicMatches: ".*not a syntax list",
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
	list := NewSyntaxCons(elem1, NewSyntaxCons(elem2, SyntaxEmptyList, sc), sc)

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
			panicMatches: ".*not a list",
			expect:       false,
		},
		{
			name: "5",
			in:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			vs:   NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil),
			out: NewSyntaxCons(
				NewSyntaxObject(values.NewInteger(10), nil),
				NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil),
					SyntaxEmptyList, nil), nil),
			expect: true,
		},
		{
			name:   "7",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			vs:     SyntaxEmptyList,
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			expect: true,
		},
		{
			name:   "8",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
			vs:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil),
			out:    NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), NewSyntaxCons(NewSyntaxObject(values.NewInteger(20), nil), SyntaxEmptyList, nil), nil),
			expect: true,
		},
		{
			name:   "9",
			in:     NewSyntaxCons(NewSyntaxObject(values.NewInteger(10), nil), SyntaxEmptyList, nil),
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

func TestSyntaxPair_ForEach_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var p *SyntaxPair
	tail, err := p.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, _ values.Value) error {
		t.Fatal("callback should not be called on nil receiver")
		return nil
	})
	c.Assert(err, qt.IsNil)
	c.Assert(tail, valuestest.SchemeEquals, values.EmptyList)
}

func TestSyntaxPair_SyntaxForEach_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var p *SyntaxPair
	tail, err := p.SyntaxForEach(context.TODO(), func(_ context.Context, _ int, _ bool, _ SyntaxValue) error {
		t.Fatal("callback should not be called on nil receiver")
		return nil
	})
	c.Assert(err, qt.IsNil)
	c.Assert(tail, qt.Equals, SyntaxEmptyList)
}
