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

// An INTERNAL test, deliberately. Two things it must assert are not reachable
// from outside the package: structural sharing, which is a claim about
// *scopeLink identity rather than about any observable answer, and Fingerprint's
// ordering, which needs scopes with CHOSEN ids. NewScope draws from a global
// monotone counter, so a set spanning the 9/10 boundary — the one that separates
// a string sort from an id sort — cannot be built through the public API at all.
package values

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// scopeAt mints a Scope with an exact id. Production code must never do this
// (see the precondition in Scopes' doc comment); a test that needs to pin an
// ordering has no other way to choose one.
func scopeAt(id uint64) *Scope {
	return &Scope{id: id}
}

func TestScopesEmptyIdentities(t *testing.T) {
	c := qt.New(t)

	var empty Scopes
	c.Assert(empty.IsEmpty(), qt.IsTrue)
	c.Assert(empty.Len(), qt.Equals, 0)
	c.Assert(empty.Fingerprint(), qt.Equals, "")
	c.Assert(empty.Slice(), qt.IsNil)
	c.Assert(empty.Has(scopeAt(1)), qt.IsFalse)

	// ∅ ⊆ X for every X, including ∅ — the rule ScopesMatch's length guard
	// encodes today by being unable to fire.
	c.Assert(empty.SubsetOf(empty), qt.IsTrue)
	c.Assert(empty.SubsetOf(ScopesFromSlice([]*Scope{scopeAt(3)})), qt.IsTrue)
	// and nothing non-empty is a subset of ∅.
	c.Assert(ScopesFromSlice([]*Scope{scopeAt(3)}).SubsetOf(empty), qt.IsFalse)

	// Removing from ∅ is the receiver, not a new allocation.
	c.Assert(empty.Remove(scopeAt(1)).IsEmpty(), qt.IsTrue)
	// A nil scope is not a member and does not become one.
	c.Assert(empty.Add(nil).IsEmpty(), qt.IsTrue)
}

// TestScopesAddOfNewMaximumSharesTheTail is the structural claim the whole
// representation rests on: the common add is ONE cell and the entire previous
// set is shared, not copied.
func TestScopesAddOfNewMaximumSharesTheTail(t *testing.T) {
	c := qt.New(t)

	base := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2), scopeAt(3)})
	grown := base.Add(scopeAt(9))

	c.Assert(grown.Len(), qt.Equals, 4)
	c.Assert(grown.node.scope.ID(), qt.Equals, uint64(9), qt.Commentf("the new maximum must head the chain"))
	c.Assert(grown.node.tail, qt.Equals, base.node, qt.Commentf("the tail must BE the previous chain, not a copy of it"))
	c.Assert(base.Len(), qt.Equals, 3, qt.Commentf("the receiver is immutable"))
}

func TestScopesAddOfExistingMemberIsTheReceiver(t *testing.T) {
	c := qt.New(t)

	s := scopeAt(4)
	base := ScopesFromSlice([]*Scope{scopeAt(1), s, scopeAt(7)})
	again := base.Add(s)

	c.Assert(again.node, qt.Equals, base.node, qt.Commentf("re-adding a member must not rebuild the chain"))
	c.Assert(again.Len(), qt.Equals, 3)
}

// TestScopesAddOfNonMaximalIdSplices covers the case the fast path does not:
// ordering is maintained, the count is right, and everything BELOW the splice is
// still shared.
func TestScopesAddOfNonMaximalIdSplices(t *testing.T) {
	c := qt.New(t)

	low := ScopesFromSlice([]*Scope{scopeAt(2)})
	base := low.Add(scopeAt(8))
	spliced := base.Add(scopeAt(5))

	c.Assert(ids(spliced), qt.DeepEquals, []uint64{8, 5, 2})
	c.Assert(spliced.Len(), qt.Equals, 3)
	c.Assert(spliced.node.tail.tail, qt.Equals, low.node,
		qt.Commentf("cells below the splice point must be shared"))
}

func TestScopesRemove(t *testing.T) {
	c := qt.New(t)

	head, mid, tail := scopeAt(9), scopeAt(5), scopeAt(1)
	base := ScopesFromSlice([]*Scope{tail, mid, head})
	c.Assert(ids(base), qt.DeepEquals, []uint64{9, 5, 1})

	c.Run("head", func(c *qt.C) {
		got := base.Remove(head)
		c.Assert(ids(got), qt.DeepEquals, []uint64{5, 1})
		c.Assert(got.node, qt.Equals, base.node.tail, qt.Commentf("removing the head returns the shared tail"))
	})
	c.Run("middle", func(c *qt.C) {
		got := base.Remove(mid)
		c.Assert(ids(got), qt.DeepEquals, []uint64{9, 1})
		c.Assert(got.Len(), qt.Equals, 2)
	})
	c.Run("last", func(c *qt.C) {
		got := base.Remove(tail)
		c.Assert(ids(got), qt.DeepEquals, []uint64{9, 5})
		c.Assert(got.Len(), qt.Equals, 2)
	})
	c.Run("absent", func(c *qt.C) {
		got := base.Remove(scopeAt(7))
		c.Assert(got.node, qt.Equals, base.node, qt.Commentf("removing an absent member must be the receiver"))
	})
	c.Run("absent below the floor", func(c *qt.C) {
		// Exercises the descending-order early exit rather than a full walk.
		got := base.Remove(scopeAt(0))
		c.Assert(got.node, qt.Equals, base.node)
	})

	c.Assert(base.Len(), qt.Equals, 3, qt.Commentf("every removal above left the receiver alone"))
}

// TestScopesRemoveOfHeadDoesNotAllocate is a PIN, not one of the phase's
// ratchets, and it cannot be one: Scopes, scopeLink and Remove do not exist at
// a7a3461e, so it pins a property of new code and can never be RED-on-master in
// the sense plan-pin-tests-must-fail-on-master means. The defect ratchet for this
// arc is the growth ratio, in the flip commit.
//
// Zero allocations is the whole reason the chain beats hash-consing for the
// intro flip, so it is worth pinning even though it can only ever be green.
func TestScopesRemoveOfHeadDoesNotAllocate(t *testing.T) {
	c := qt.New(t)

	head := scopeAt(100)
	base := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2), scopeAt(3)}).Add(head)

	var sink Scopes
	allocs := testing.AllocsPerRun(100, func() {
		sink = base.Remove(head)
	})
	c.Assert(sink.Len(), qt.Equals, 3)
	c.Assert(allocs, qt.Equals, 0.0,
		qt.Commentf("removing the maximum must return the shared tail, allocating nothing"))
}

func TestScopesFlip(t *testing.T) {
	c := qt.New(t)

	s := scopeAt(6)
	base := ScopesFromSlice([]*Scope{scopeAt(2)})

	on := base.Flip(s)
	c.Assert(on.Has(s), qt.IsTrue)
	c.Assert(on.Len(), qt.Equals, 2)

	off := on.Flip(s)
	c.Assert(off.Has(s), qt.IsFalse)
	c.Assert(off.Len(), qt.Equals, 1)
	c.Assert(off.node, qt.Equals, base.node, qt.Commentf("flip-then-unflip of a maximum returns the original chain"))
}

func TestScopesHas(t *testing.T) {
	c := qt.New(t)

	mid := scopeAt(5)
	set := ScopesFromSlice([]*Scope{scopeAt(1), mid, scopeAt(9)})

	c.Assert(set.Has(mid), qt.IsTrue)
	c.Assert(set.Has(scopeAt(9)), qt.IsFalse, qt.Commentf("membership is pointer identity, not id equality"))
	c.Assert(set.Has(scopeAt(4)), qt.IsFalse)
	c.Assert(set.Has(scopeAt(99)), qt.IsFalse)
}

// TestScopesSubsetOfRejectsInOne exercises each O(1) reject on its own, because
// a merge that happens to be correct hides a reject that never fires.
func TestScopesSubsetOfRejectsInOne(t *testing.T) {
	c := qt.New(t)

	c.Run("larger cannot be a subset", func(c *qt.C) {
		big := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2), scopeAt(3)})
		small := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2)})
		c.Assert(big.SubsetOf(small), qt.IsFalse)
	})
	c.Run("a bigger maximum cannot be a subset", func(c *qt.C) {
		// Equal cardinality, so the size reject cannot fire; max(b) > max(u).
		b := ScopesFromSlice([]*Scope{scopeAt(50)})
		u := ScopesFromSlice([]*Scope{scopeAt(10)})
		c.Assert(b.SubsetOf(u), qt.IsFalse)
	})
	c.Run("a shared spine is its own subset", func(c *qt.C) {
		base := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2)})
		same := base.Add(base.node.scope) // re-add a member: same chain back
		c.Assert(same.node, qt.Equals, base.node)
		c.Assert(same.SubsetOf(base), qt.IsTrue)
	})
	c.Run("a spine shared MID-WALK short-circuits", func(c *qt.C) {
		// The reject the doc claims hits constantly in practice, and the one
		// none of the head-level rejects can reach: a reference set is its
		// binder's set plus a scope, so the two chains diverge at the head and
		// converge one cell down. Every O(1) reject misses — the chains differ,
		// p is smaller, and p's maximum is the lower — so the merge must notice
		// the shared cell itself rather than walking both to the end.
		binder := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(2)})
		reference := binder.Add(scopeAt(30))
		c.Assert(reference.node, qt.Not(qt.Equals), binder.node)
		c.Assert(reference.node.tail, qt.Equals, binder.node)
		c.Assert(binder.SubsetOf(reference), qt.IsTrue)
	})
}

func TestScopesSubsetOfMerge(t *testing.T) {
	c := qt.New(t)

	a, b, d := scopeAt(1), scopeAt(2), scopeAt(4)
	use := ScopesFromSlice([]*Scope{a, b, d})

	c.Assert(ScopesFromSlice([]*Scope{a, d}).SubsetOf(use), qt.IsTrue,
		qt.Commentf("a genuine subset with a gap must merge through"))
	c.Assert(ScopesFromSlice([]*Scope{b}).SubsetOf(use), qt.IsTrue)
	c.Assert(use.SubsetOf(use), qt.IsTrue)

	// Equal cardinality, incomparable: neither is a subset. This is the shape
	// that makes resolution AMBIGUOUS rather than unresolved.
	x := ScopesFromSlice([]*Scope{a, b})
	y := ScopesFromSlice([]*Scope{a, d})
	c.Assert(x.SubsetOf(y), qt.IsFalse)
	c.Assert(y.SubsetOf(x), qt.IsFalse)

	// A member below the use set's floor: the merge must run off the end of u
	// rather than report a spurious match.
	c.Assert(ScopesFromSlice([]*Scope{scopeAt(3), d}).SubsetOf(use), qt.IsFalse)

	// u EXHAUSTS before p does. Neither O(1) reject fires — equal cardinality,
	// and p's maximum is the smaller — so this is the only way to reach the
	// merge's b == nil arm, and without it a walk that fell off the end of u
	// would read as a match.
	c.Assert(ScopesFromSlice([]*Scope{scopeAt(1)}).SubsetOf(ScopesFromSlice([]*Scope{scopeAt(2)})), qt.IsFalse)
}

// TestScopesFingerprintIsIdOrdered is the one that records the deliberate
// divergence. ScopeFingerprint sorts the id STRINGS, so {2,10} yields "10,2";
// this walks an id-ordered chain and yields "10,2" as well — but for {2,9,10}
// the two disagree, and the invariant is "one total order applied by one
// function", not "keep the old output".
func TestScopesFingerprintIsIdOrdered(t *testing.T) {
	c := qt.New(t)

	set := ScopesFromSlice([]*Scope{scopeAt(2), scopeAt(10), scopeAt(9)})
	got := set.Fingerprint()

	c.Assert(got, qt.Equals, "10,9,2", qt.Commentf("descending by id"))

	// The string sort the old function performs would put 10 before 2 before 9.
	stringSorted := "10,2,9"
	c.Assert(got, qt.Not(qt.Equals), stringSorted,
		qt.Commentf("the 9/10 boundary is exactly where a string sort and an id sort disagree"))

	// The guarantee match.FreeIdName actually depends on is the character class.
	c.Assert(strings.Trim(got, "0123456789,"), qt.Equals, "",
		qt.Commentf("Fingerprint must contain only [0-9,]"))

	// Distinct sets fingerprint distinctly; equal sets built in any order agree.
	other := ScopesFromSlice([]*Scope{scopeAt(10), scopeAt(9), scopeAt(2)})
	c.Assert(other.Fingerprint(), qt.Equals, got)
	c.Assert(ScopesFromSlice([]*Scope{scopeAt(2)}).Fingerprint(), qt.Not(qt.Equals), got)
}

func TestScopesSliceRoundTrip(t *testing.T) {
	c := qt.New(t)

	in := []*Scope{scopeAt(4), scopeAt(1), scopeAt(7)}
	set := ScopesFromSlice(in)

	out := set.Slice()
	c.Assert(ids(set), qt.DeepEquals, []uint64{7, 4, 1}, qt.Commentf("Slice is descending by id, not insertion order"))
	c.Assert(len(out), qt.Equals, 3)

	back := ScopesFromSlice(out)
	c.Assert(back.Fingerprint(), qt.Equals, set.Fingerprint())
	c.Assert(back.SubsetOf(set), qt.IsTrue)
	c.Assert(set.SubsetOf(back), qt.IsTrue)

	// Duplicates in the input collapse.
	dup := scopeAt(3)
	c.Assert(ScopesFromSlice([]*Scope{dup, dup, dup}).Len(), qt.Equals, 1)
}

func TestScopesForEach(t *testing.T) {
	c := qt.New(t)

	set := ScopesFromSlice([]*Scope{scopeAt(1), scopeAt(5), scopeAt(3)})

	var seen []uint64
	set.ForEach(func(s *Scope) {
		seen = append(seen, s.ID())
	})
	c.Assert(seen, qt.DeepEquals, []uint64{5, 3, 1})

	var empty Scopes
	called := false
	empty.ForEach(func(_ *Scope) {
		called = true
	})
	c.Assert(called, qt.IsFalse)
}

// TestScopesFromRealConstructorsAreMonotone ties the type back to the API
// production actually uses: NewScope draws from a monotone counter, so a set
// built by successive mints puts the newest at the head — the property Add's
// fast path depends on.
func TestScopesFromRealConstructorsAreMonotone(t *testing.T) {
	c := qt.New(t)

	first := NewScope()
	second := NewScope()
	third := NewScope()

	set := ScopesFromSlice([]*Scope{first, second}).Add(third)
	c.Assert(set.node.scope, qt.Equals, third, qt.Commentf("the newest mint must head the chain"))
	c.Assert(set.Len(), qt.Equals, 3)

	var allocs float64
	sink := set
	allocs = testing.AllocsPerRun(50, func() {
		sink = set.Remove(third)
	})
	c.Assert(allocs, qt.Equals, 0.0)
	c.Assert(sink.Len(), qt.Equals, 2)
}

// ids materializes the chain's ids in order, so an assertion can be about the
// ordering rather than about pointer soup.
func ids(p Scopes) []uint64 {
	var q []uint64
	p.ForEach(func(s *Scope) {
		q = append(q, s.ID())
	})
	return q
}
