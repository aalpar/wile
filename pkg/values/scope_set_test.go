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
	// Scopes is meaningless for the wildcard and reports nil.
	qt.Assert(t, q.Scopes(), qt.IsNil)
}

func TestScopeSet_EmptyScopes(t *testing.T) {
	q := values.EmptyScopes()
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
	qt.Assert(t, q.Scopes(), qt.HasLen, 0)
}

// TestScopeSet_ScopesOfNilIsEmptyNotAll pins the anti-footgun: a nil slice is
// the empty set, NEVER the wildcard. This is the exact ambiguity the type
// exists to remove — the old read surface read nil as "match any".
func TestScopeSet_ScopesOfNilIsEmptyNotAll(t *testing.T) {
	q := values.ScopesOf(nil)
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
}

func TestScopeSet_ScopesOfEmptySliceIsEmpty(t *testing.T) {
	q := values.ScopesOf([]*values.Scope{})
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsTrue)
}

func TestScopeSet_Specific(t *testing.T) {
	s1 := values.NewScope()
	s2 := values.NewScope()
	q := values.ScopesOf([]*values.Scope{s1, s2})
	qt.Assert(t, q.IsAll(), qt.IsFalse)
	qt.Assert(t, q.IsEmpty(), qt.IsFalse)
	got := q.Scopes()
	qt.Assert(t, got, qt.HasLen, 2)
	qt.Assert(t, got[0], qt.Equals, s1)
	qt.Assert(t, got[1], qt.Equals, s2)
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
	scopes := []*values.Scope{s1, s2}
	// The specific form reuses ScopeFingerprint, so the format matches the
	// map-key form exactly regardless of the minted scope IDs.
	want := "scopes{" + values.ScopeFingerprint(scopes) + "}"
	qt.Assert(t, values.ScopesOf(scopes).String(), qt.Equals, want)
}
