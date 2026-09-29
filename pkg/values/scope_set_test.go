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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestScopeSet_AllScopes(t *testing.T) {
	q := values.AllScopes()
	qt.Assert(t, q.IsAll(), qt.IsTrue)
	qt.Assert(t, q.IsEmpty(), qt.IsFalse)
	// Scopes is meaningless for the wildcard and reports the empty set.
	qt.Assert(t, q.Scopes().IsEmpty(), qt.IsTrue)
}

func TestScopeSet_EmptyScopes(t *testing.T) {
	q := values.EmptyScopes()
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
	qt.Assert(t, q.Scopes().Len(), qt.Equals, 0)
}

// TestScopeSet_ScopesOfNilIsEmptyNotAll pins the anti-footgun: the absent scope
// set is the empty set, NEVER the wildcard. This is the exact ambiguity the type
// exists to remove — the old read surface read nil as "match any". Under Scopes
// the nil the name refers to is not even expressible: the argument is a value
// type whose zero value is the empty set.
func TestScopeSet_ScopesOfNilIsEmptyNotAll(t *testing.T) {
	q := values.ScopesOf(values.Scopes{})
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
}

func TestScopeSet_ScopesOfEmptySliceIsEmpty(t *testing.T) {
	q := values.ScopesOf(values.ScopesFromSlice([]*values.Scope{}))
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
}

func TestScopeSet_Specific(t *testing.T) {
	s1 := values.NewScope()
	s2 := values.NewScope()
	q := values.ScopesOf(values.ScopesFromSlice([]*values.Scope{s1, s2}))
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsFalse)
	got := q.Scopes()
	// The two positional rows that stood here read the set back in insertion
	// order. Order is not part of Scopes' contract; membership and cardinality are
	// what resolution reads, and together they pin the set exactly.
	qt.Assert(t, got.Len(), qt.Equals, 2)
	qt.Assert(t, got.Has(s1), qt.IsTrue)
	qt.Assert(t, got.Has(s2), qt.IsTrue)
}

// TestScopeSet_AllAndEmptyAreDistinct is the headline: "all" and the empty set
// are now different states. A nil slice conflated them in the old encoding; the
// `all` flag separates them, so a query can no longer silently widen from empty
// to wildcard.
func TestScopeSet_AllAndEmptyAreDistinct(t *testing.T) {
	all := values.AllScopes()
	empty := values.EmptyScopes()

	qt.Assert(t, all.IsAll(), qt.IsTrue)
	qt.Assert(t, empty.IsAll(), qt.IsFalse)

	qt.Assert(t, all.IsEmpty(), qt.IsFalse)
	qt.Assert(t, empty.IsEmpty(), qt.IsTrue)
}

// TestScopeSet_ZeroValueIsEmptyNotAll pins the safe default: an uninitialized
// ScopeSet is the empty set, not the wildcard, so a forgotten construction
// cannot widen a resolution.
func TestScopeSet_ZeroValueIsEmptyNotAll(t *testing.T) {
	var q values.ScopeSet
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
}

func TestScopeSet_String(t *testing.T) {
	qt.Assert(t, values.AllScopes().String(), qt.Equals, "all-scopes")
	qt.Assert(t, values.EmptyScopes().String(), qt.Equals, "scopes{}")

	s1 := values.NewScope()
	s2 := values.NewScope()
	scopes := values.ScopesFromSlice([]*values.Scope{s1, s2})
	// The specific form reuses ScopeFingerprint, so the format matches the
	// map-key form exactly regardless of the minted scope IDs.
	want := "scopes{" + values.ScopeFingerprint(scopes) + "}"
	qt.Assert(t, values.ScopesOf(scopes).String(), qt.Equals, want)
}

// TestAddScopeToSet_DoesNotAliasSpareCapacity is the pin for the aliasing
// defect: two adds onto ONE source set must not write into one backing array.
//
// The source set is built the way the expander builds one — through
// RemoveScopeFromSet, which allocates cap len(scopes) and fills len(scopes)-1,
// leaving exactly one spare slot. A bare append would put both added scopes in
// that slot, and the first result would silently read back the second's scope.
// That is the shape that broke hygiene for two nested expansions of one macro
// under the Scheme syntax layer, where a template identifier is a single shared
// object rather than a fresh copy per expansion.
func TestAddScopeToSet_DoesNotAliasSpareCapacity(t *testing.T) {
	a := values.NewScopeWithLabel("a")
	b := values.NewScopeWithLabel("b")
	base := values.ScopesFromSlice([]*values.Scope{a, b})
	dropped := values.RemoveScopeFromSet(base, b)
	qt.Assert(t, dropped.Len(), qt.Equals, 1)
	// The assertion that stood here was `cap(dropped) > len(dropped)`: the defect
	// needed a set carrying a spare backing-array slot, and RemoveScopeFromSet was
	// where one came from. A persistent chain has neither a backing array nor a
	// capacity, so that precondition is no longer expressible and the aliasing it
	// set up is structurally absent rather than guarded. The property that
	// replaces it — a new maximum shares the whole tail — is pinned by
	// TestScopesAddOfNewMaximumSharesTheTail.

	introOuter := values.NewScopeWithLabel("intro:outer")
	introInner := values.NewScopeWithLabel("intro:inner")
	outer := values.AddScopeToSet(dropped, introOuter)
	inner := values.AddScopeToSet(dropped, introInner)

	// The two positional rows that stood here are DELETED rather than re-indexed.
	// They read the added scope back at a fixed position, which asserts storage
	// order — and order is not part of Scopes' contract, only an implementation
	// invariant that pkg/values asserts once for itself. What the rows were
	// actually for is that the two results do not read back each other's scope,
	// and membership says that directly, without depending on where a member sits.
	qt.Assert(t, outer.Has(introOuter), qt.IsTrue)
	qt.Assert(t, outer.Has(introInner), qt.IsFalse)
	qt.Assert(t, inner.Has(introInner), qt.IsTrue)
	qt.Assert(t, inner.Has(introOuter), qt.IsFalse)
	qt.Assert(t, dropped.Len(), qt.Equals, 1, qt.Commentf("the source set must be unchanged"))
}
