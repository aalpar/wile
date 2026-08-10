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

package helpers

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

// ── ForEachList ─────────────────────────────────────────────────────────

func TestForEachList(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	tcs := []struct {
		name string
		list values.Tuple
		want []values.Value
	}{
		{
			"empty list",
			values.EmptyList,
			nil,
		},
		{
			"single element",
			values.List(values.NewInteger(1)),
			[]values.Value{values.NewInteger(1)},
		},
		{
			"three elements",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			[]values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var got []values.Value
			err := ForEachList(ctx, tc.list, "test", func(_ context.Context, _ int, _ bool, v values.Value) error {
				got = append(got, v)
				return nil
			})
			c.Assert(err, qt.IsNil)
			c.Assert(len(got), qt.Equals, len(tc.want))
			for i := range tc.want {
				c.Assert(got[i], valuestest.SchemeEquals, tc.want[i])
			}
		})
	}
}

func TestForEachList_Errors(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	noop := func(_ context.Context, _ int, _ bool, _ values.Value) error {
		return nil
	}

	tcs := []struct {
		name     string
		list     values.Tuple
		sentinel error
	}{
		{
			"improper list",
			values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			werr.ErrNotAList,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := ForEachList(ctx, tc.list, "test", noop)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// ── ListToVector ─────────────────────────────────────────────────────

func TestListToVector(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg  values.Value
		want *values.Vector
	}{
		{
			"empty list",
			values.EmptyList,
			values.NewVector(),
		},
		{
			"single element",
			values.List(values.NewInteger(1)),
			values.NewVector(values.NewInteger(1)),
		},
		{
			"three elements",
			values.List(values.NewInteger(1), values.NewString("two"), values.TrueValue),
			values.NewVector(values.NewInteger(1), values.NewString("two"), values.TrueValue),
		},
		{
			"nested list",
			values.List(values.List(values.NewInteger(1), values.NewInteger(2))),
			values.NewVector(values.List(values.NewInteger(1), values.NewInteger(2))),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := ListToVector(mc, "test")
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestListToVector_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg      values.Value
		sentinel error
	}{
		{
			"not a list",
			values.NewInteger(42),
			werr.ErrNotAList,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := ListToVector(mc, "test")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// ── CollectVectors ───────────────────────────────────────────────────

func TestCollectVectors(t *testing.T) {
	c := qt.New(t)

	v1 := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	v2 := values.NewVector(values.NewInteger(4), values.NewInteger(5))
	v3 := values.NewVector(values.NewInteger(6), values.NewInteger(7), values.NewInteger(8), values.NewInteger(9))

	tcs := []struct {
		name       string
		rest       values.Value
		wantCount  int
		wantMinLen int
	}{
		{
			"empty list",
			values.EmptyList,
			0, 0,
		},
		{
			"single vector",
			values.List(v1),
			1, 3,
		},
		{
			"two vectors min is shorter",
			values.List(v1, v2),
			2, 2,
		},
		{
			"three vectors min is middle",
			values.List(v1, v2, v3),
			3, 2,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			vecs, minLen, err := CollectVectors(tc.rest, "test")
			c.Assert(err, qt.IsNil)
			c.Assert(len(vecs), qt.Equals, tc.wantCount)
			c.Assert(minLen, qt.Equals, tc.wantMinLen)
		})
	}
}

func TestCollectVectors_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		rest     values.Value
		sentinel error
	}{
		{
			"non-vector element",
			values.List(values.NewInteger(1)),
			werr.ErrNotAVector,
		},
		{
			"mixed vector and non-vector",
			values.List(
				values.NewVector(values.NewInteger(1)),
				values.NewString("bad"),
			),
			werr.ErrNotAVector,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, _, err := CollectVectors(tc.rest, "test")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// ── MemberLookup ─────────────────────────────────────────────────────

func TestMemberLookup(t *testing.T) {
	c := qt.New(t)

	lst := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	tcs := []struct {
		name string
		key  values.Value
		list values.Value
		eq   func(a, b values.Value) bool
		want values.Value
	}{
		{
			"found first element",
			values.NewInteger(1),
			lst,
			valEq,
			lst, // returns tail from match point
		},
		{
			"found middle element",
			values.NewInteger(2),
			lst,
			valEq,
			lst.Cdr(), // (2 3)
		},
		{
			// The LAST element is the case a first-and-middle table cannot see.
			// MemberLookup walks with values.ForEachFunc, which hands the element
			// and never the cons cell, so the returned sublist comes from a cursor
			// advanced separately inside the callback. A cursor that falls out of
			// step with the iterator returns a WRONG SUBLIST, not an error — and
			// the drift is cumulative, so it first becomes visible at the tail.
			"found last element",
			values.NewInteger(3),
			lst,
			valEq,
			lst.Cdr().(values.Tuple).Cdr(), // (3)
		},
		{
			"not found returns false",
			values.NewInteger(99),
			lst,
			valEq,
			values.FalseValue,
		},
		{
			"empty list returns false",
			values.NewInteger(1),
			values.EmptyList,
			valEq,
			values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.key, tc.list)
			err := MemberLookup(mc, "test", tc.eq)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// TestMemberLookup_MatchAtEveryIndex is the cursor guard, and it is deliberately
// exhaustive rather than sampled.
//
// values.ForEachFunc hands the ELEMENT and never the cons cell
// (pkg/values/values.go), while MemberLookup must return the sublist starting at
// the match. Pair.ForEach cannot supply it either: on a callback error it returns
// (nil, err) rather than the stop position. So the sublist comes from a cursor
// advanced by hand inside the callback, in lockstep with an iterator that hides
// the cell — and the failure mode of losing that lockstep is a wrong sublist, not
// an error. Nothing else in the suite would go red.
//
// Off-by-one drift is cumulative, so it is invisible near the head and grows
// toward the tail. Matching at every index of a list long enough to accumulate it
// is what makes the guard sound; a first-and-middle table is not.
func TestMemberLookup_MatchAtEveryIndex(t *testing.T) {
	c := qt.New(t)

	const n = 12
	elems := make([]values.Value, n)
	for i := range elems {
		elems[i] = values.NewInteger(int64(i))
	}
	lst := values.List(elems...)

	for i := range n {
		t.Run(fmt.Sprintf("index-%d", i), func(t *testing.T) {
			// The expected answer, derived independently of MemberLookup: walk i
			// cdrs from the head.
			want := values.Value(lst)
			for range i {
				tup, ok := want.(values.Tuple)
				c.Assert(ok, qt.IsTrue)
				want = tup.Cdr()
			}

			mc := makeMC(values.NewInteger(int64(i)), lst)
			err := MemberLookup(mc, "test", valEq)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, want)
		})
	}
}

func TestMemberLookup_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		key      values.Value
		list     values.Value
		sentinel error
	}{
		{
			"list not a list",
			values.NewInteger(1),
			values.NewInteger(42),
			werr.ErrNotAList,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.key, tc.list)
			err := MemberLookup(mc, "test", valEq)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// TestMemberLookup_CircularListTerminates pins the half of the walk that a
// hand-rolled cdr loop cannot have: Brent cycle detection. Before MemberLookup
// routed through ForEachList it spun forever on a circular list whose elements
// do not contain the key — from the CLI that is SIGKILL, exit 137 — while assq
// on the same shape returned in microseconds. Bounded by a goroutine because
// the pre-fix failure mode is a hang, not a wrong value.
func TestMemberLookup_CircularListTerminates(t *testing.T) {
	c := qt.New(t)

	// (1 2 3) with the last cdr pointing back at the head.
	head := values.NewCons(values.NewInteger(1), values.EmptyList)
	mid := values.NewCons(values.NewInteger(2), values.EmptyList)
	tail := values.NewCons(values.NewInteger(3), values.EmptyList)
	head.SetCdr(mid)
	mid.SetCdr(tail)
	tail.SetCdr(head)

	mc := makeMC(values.NewInteger(99), head)

	type result struct {
		err error
	}
	done := make(chan result, 1)
	go func() {
		done <- result{err: MemberLookup(mc, "memq", valEq)}
	}()

	select {
	case got := <-done:
		c.Assert(got.err, qt.IsNotNil)
		c.Assert(errors.Is(got.err, werr.ErrNotAList), qt.IsTrue,
			qt.Commentf("got %v", got.err))
		// The cycle sentinel stays reachable, as it does for assq.
		c.Assert(errors.Is(got.err, werr.ErrCircularList), qt.IsTrue,
			qt.Commentf("got %v", got.err))
	case <-time.After(5 * time.Second):
		t.Fatal("MemberLookup did not terminate on a circular list")
	}
}

// TestMemberLookup_ObservesCancellation pins the other half: the amortized
// context poll. A flat Go loop is invisible to an embedder deadline and to a
// REPL interrupt (which cancels a child context), and WithMaxCallDepth is
// inapplicable to it because nothing recurses — the context was the only lever
// and MemberLookup never read it.
//
// The list must exceed Pair.ForEach's poll interval (every 1024 elements), and
// the context is cancelled up front so the assertion is on the mechanism rather
// than on wall-clock timing.
func TestMemberLookup_ObservesCancellation(t *testing.T) {
	c := qt.New(t)

	elems := make([]values.Value, 4096)
	for i := range elems {
		elems[i] = values.NewInteger(int64(i))
	}
	lst := values.List(elems...)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	mc := makeMCWithContext(ctx, values.NewInteger(-1), lst)

	err := MemberLookup(mc, "memq", valEq)
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("MemberLookup walked a 4096-element list under a cancelled context"))
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue, qt.Commentf("got %v", err))
}

// ── AssocLookup ──────────────────────────────────────────────────────

func valEq(a, b values.Value) bool {
	return Eqv(a, b)
}

func TestAssocLookup(t *testing.T) {
	c := qt.New(t)

	// Build an alist: ((1 . "one") (2 . "two") (3 . "three"))
	entry1 := values.NewCons(values.NewInteger(1), values.NewString("one"))
	entry2 := values.NewCons(values.NewInteger(2), values.NewString("two"))
	entry3 := values.NewCons(values.NewInteger(3), values.NewString("three"))
	alist := values.List(entry1, entry2, entry3)

	tcs := []struct {
		name string
		key  values.Value
		list values.Value
		eq   func(a, b values.Value) bool
		want values.Value
	}{
		{
			"found first entry",
			values.NewInteger(1),
			alist,
			valEq,
			entry1,
		},
		{
			"found middle entry",
			values.NewInteger(2),
			alist,
			valEq,
			entry2,
		},
		{
			"found last entry",
			values.NewInteger(3),
			alist,
			valEq,
			entry3,
		},
		{
			"not found returns false",
			values.NewInteger(99),
			alist,
			valEq,
			values.FalseValue,
		},
		{
			"empty alist returns false",
			values.NewInteger(1),
			values.EmptyList,
			valEq,
			values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.key, tc.list)
			err := AssocLookup(mc, "test", tc.eq)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestAssocLookup_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		key      values.Value
		list     values.Value
		sentinel error
	}{
		{
			"alist not a list",
			values.NewInteger(1),
			values.NewInteger(42),
			werr.ErrNotAList,
		},
		{
			"alist entry not a pair",
			values.NewInteger(1),
			values.List(values.NewInteger(99)),
			werr.ErrNotAPair,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.key, tc.list)
			err := AssocLookup(mc, "test", valEq)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// ── Uncons ──────────────────────────────────────────────────────────────

func TestUncons(t *testing.T) {
	sym := values.NewSymbol("x")
	n := values.NewInteger(1)
	proper := values.List(sym, n)      // (x 1)
	improper := values.NewCons(sym, n) // (x . 1) — cdr is not a Tuple
	tcs := []struct {
		name     string
		input    values.Value
		wantHead values.Value
		wantTail values.Value
		wantErr  error
	}{
		{"proper-head-symbol", proper, sym, values.NewCons(n, values.EmptyList), nil},
		{"empty-list", values.EmptyList, nil, nil, werr.ErrNotAList},
		{"nil-input", nil, nil, nil, werr.ErrNotAList},
		{"improper-cdr-ok", improper, sym, n, nil},
		{"non-list-value", values.NewInteger(42), nil, nil, werr.ErrNotAList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			head, tail, err := Uncons(tc.input, "test", "first arg")
			if tc.wantErr != nil {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
					qt.Commentf("got %v", err))
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, head, valuestest.SchemeEquals, tc.wantHead)
			qt.Assert(t, tail, valuestest.SchemeEquals, tc.wantTail)
		})
	}
}

// ── UnconsTyped ─────────────────────────────────────────────────────────

func TestUnconsTyped(t *testing.T) {
	sym := values.NewSymbol("x")
	n := values.NewInteger(1)
	symList := values.List(sym, n) // (x 1) — head is symbol
	intList := values.List(n, sym) // (1 x) — head is integer
	tcs := []struct {
		name    string
		input   values.Value
		wantOk  bool
		wantErr error
	}{
		{"head-is-symbol", symList, true, nil},
		{"head-is-integer", intList, false, werr.ErrNotASymbol},
		{"empty", values.EmptyList, false, werr.ErrNotAList},
		{"non-list", values.NewInteger(42), false, werr.ErrNotAList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			gotSym, _, err := UnconsTyped[*values.Symbol](tc.input, werr.ErrNotASymbol, "test", "head")
			if tc.wantErr != nil {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
					qt.Commentf("got %v", err))
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, gotSym.EqualTo(sym), qt.IsTrue)
		})
	}
}

// ── CarAs ───────────────────────────────────────────────────────────────

func TestCarAs(t *testing.T) {
	sym := values.NewSymbol("foo")
	n := values.NewInteger(1)
	tcs := []struct {
		name    string
		tuple   values.Tuple
		wantErr error
	}{
		{"symbol-head", values.List(sym, n), nil},
		{"int-head", values.List(n), werr.ErrNotASymbol},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := CarAs[*values.Symbol](tc.tuple, werr.ErrNotASymbol, "test", "head")
			if tc.wantErr != nil {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.EqualTo(sym), qt.IsTrue)
		})
	}
}

// ── NthCons ─────────────────────────────────────────────────────────────

func TestNthCons(t *testing.T) {
	list := values.List(
		values.NewInteger(10),
		values.NewInteger(20),
		values.NewInteger(30),
	) // (10 20 30)
	tcs := []struct {
		name    string
		input   values.Value
		n       int64
		wantStr string
		wantErr error
	}{
		{"index-0", list, 0, "(10 20 30)", nil},
		{"index-1", list, 1, "(20 30)", nil},
		{"index-2", list, 2, "(30)", nil},
		{"index-3-empty", list, 3, "()", nil},
		{"index-out-of-range", list, 4, "", werr.ErrIndexOutOfRange},
		{"index-on-empty", values.EmptyList, 1, "", werr.ErrIndexOutOfRange},
		{"index-zero-on-empty-ok", values.EmptyList, 0, "()", nil},
		{"negative-index", list, -1, "", werr.ErrIndexOutOfRange},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := NthCons(tc.input, tc.n, "test")
			if tc.wantErr != nil {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
					qt.Commentf("got %v", err))
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}
