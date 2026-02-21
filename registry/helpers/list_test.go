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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

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
			values.ErrNotAList,
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
			values.ErrNotAVector,
		},
		{
			"mixed vector and non-vector",
			values.List(
				values.NewVector(values.NewInteger(1)),
				values.NewString("bad"),
			),
			values.ErrNotAVector,
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
			values.ErrNotAList,
		},
		{
			"alist entry not a pair",
			values.NewInteger(1),
			values.List(values.NewInteger(99)),
			values.ErrNotAPair,
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
