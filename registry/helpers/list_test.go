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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
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

// ── CollectStrings ───────────────────────────────────────────────────

func TestCollectStrings(t *testing.T) {
	c := qt.New(t)

	s1 := values.NewString("abc")
	s2 := values.NewString("de")
	s3 := values.NewString("fghij")

	tcs := []struct {
		name       string
		rest       values.Value
		wantCount  int
		wantMinLen int
	}{
		{"empty list", values.EmptyList, 0, 0},
		{"single string", values.List(s1), 1, 3},
		{"two strings min is shorter", values.List(s1, s2), 2, 2},
		{"three strings min is middle", values.List(s1, s2, s3), 3, 2},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			strs, runeSlices, minLen, err := CollectStrings(tc.rest, "test")
			c.Assert(err, qt.IsNil)
			c.Assert(len(strs), qt.Equals, tc.wantCount)
			if tc.wantCount > 0 {
				c.Assert(len(runeSlices), qt.Equals, tc.wantCount)
				c.Assert(minLen, qt.Equals, tc.wantMinLen)
			}
		})
	}
}

func TestCollectStrings_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		rest     values.Value
		sentinel error
	}{
		{
			"non-string element",
			values.List(values.NewInteger(1)),
			werr.ErrNotAString,
		},
		{
			"mixed string and non-string",
			values.List(values.NewString("ok"), values.NewInteger(1)),
			werr.ErrNotAString,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, _, _, err := CollectStrings(tc.rest, "test")
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
	proper := values.List(sym, n)           // (x 1)
	improper := values.NewCons(sym, n)      // (x . 1) — cdr is not a Tuple
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
