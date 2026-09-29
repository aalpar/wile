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

package values

import (
	"iter"
	"strconv"
	"strings"
)

// Scopes is an immutable set of *Scope held as a persistent chain sorted
// DESCENDING by Scope id. The zero value is the empty set.
//
// It exists to make the two hot hygiene operations cheap. Today a scope set is a
// []*Scope that every touch copies: SourceContext.WithScope allocates
// len(scopes)+1 and copies, so compiling n nested lexical forms performs O(n²)
// adds over sets averaging O(n) members and allocates O(n³) words. A persistent
// chain shares its tail instead, which collapses one factor.
//
// # Why descending by id
//
// Scope carries a monotone uint64 from an atomic counter with no reuse, and the
// intro, use-site, lambda and let scopes are all minted immediately before being
// added. A freshly minted scope is therefore the new maximum, so Add is one
// 24-byte cons at the head sharing the whole tail, and Remove of that same head
// returns the shared tail with ZERO allocation — the property hash-consing
// cannot give, and the reason a chain beats it here.
//
// Measured 2026-09-26 by instrumenting the existing builders and classifying
// every operation by chain position: over the whole pkg/wile Go suite, 20,469,332
// adds were ALL head-max and 2,512,036 removes were ALL of the head, with zero
// splices; same at 100% on the nested-let and macro-recursion probes, on
// NewEngine bootstrap, and on binder-introducing macros.
//
// That is an EMPIRICAL property, not an enforced one, and the distinction
// matters to anyone reasoning from it. A non-maximal id splices, O(position),
// and nothing in the tree prevents one: the current builders do not even agree
// on insertion order (SourceContext.WithScope prepends while AddScopeToSet
// appends, the latter pinned by TestAddScopeToSet_DoesNotAliasSpareCapacity), so
// 1.38% of observed multi-element sets are not stored in descending-id order
// today. Storage order is irrelevant here because this type canonicalizes by id
// — but do not restate the folklore that one prepending builder guarantees the
// fast path.
//
// # Order is an implementation technique, not part of the contract
//
// A scope set is a SET — Flatt's model has no ordering, and neither does this
// type's contract. The chain is canonically sorted because that is what buys the
// O(1) operations above and what lets Fingerprint be a valid map key, but NO
// CALLER OUTSIDE THIS PACKAGE MAY DEPEND ON THE ORDER. There is deliberately no
// Slice() and no indexed access: ask Has, SubsetOf, Len or IsEmpty for semantics,
// Fingerprint for a key, and All when you genuinely need to walk. A caller that
// needs a materialized slice writes slices.Collect(s.All()) and owns the
// consequences of caring.
//
// This is not pedantry. While the set was a []*Scope, tests across five packages
// asserted member POSITIONS, so changing the representation meant deciding, per
// site, what the "right" new position was — a question that has no principled
// answer, because the object has no order. Keeping order out of the contract
// makes the question unaskable, and makes a future change of representation
// (ascending, hash-consed, bitmap) cost nothing outside this file.
//
// # The precondition the type does not enforce
//
// Every member must come from a New*Scope constructor. Scope has an unexported
// id beside exported fields and is re-exported as a type ALIAS by pkg/syntax, so
// &syntax.Scope{Label: "x"} compiles from any package, including an embedder's,
// and yields ID() == 0. Two id-0 scopes in one set break Has's early exit and the
// SubsetOf merge, where the slice form's slices.Contains is correct regardless of
// id. No in-tree non-test site constructs a Scope literal, so this is latent
// rather than live — but it is a precondition, not a free consequence of
// monotonicity.
type Scopes struct {
	node *scopeLink
}

// scopeLink is one cell of the persistent chain: the maximum-id member of the
// set it heads, the rest of that set, and the count. size is int32 because a
// scope set is bounded far below 2^31 by the nesting depth that produces it, and
// it keeps the cell at 24 bytes.
type scopeLink struct {
	scope *Scope     // the maximum-id member of this set
	tail  *scopeLink // the rest, all strictly smaller ids
	size  int32
}

// ScopesFromSlice builds a set from a slice in any order, discarding duplicates.
// It is the boundary constructor: callers holding a []*Scope convert once here
// rather than threading both representations.
func ScopesFromSlice(scopes []*Scope) Scopes {
	var q Scopes
	for _, s := range scopes {
		q = q.Add(s)
	}
	return q
}

// Len reports the number of members. O(1), which is what the cardinality compare
// in scoped resolution needs.
func (p Scopes) Len() int {
	if p.node == nil {
		return 0
	}
	return int(p.node.size)
}

// IsEmpty reports whether the set has no members.
func (p Scopes) IsEmpty() bool {
	return p.node == nil
}

// Has reports whether target is a member. The chain is id-descending, so the
// walk stops as soon as it passes target's id rather than scanning to the end.
func (p Scopes) Has(target *Scope) bool {
	id := target.ID()
	for n := p.node; n != nil; n = n.tail {
		if n.scope == target {
			return true
		}
		if n.scope.ID() < id {
			return false
		}
	}
	return false
}

// Add returns the set with s included. Adding a member it already has returns
// the receiver unchanged, so the result shares the whole chain.
//
// Adding a new maximum — the case every measured production add takes — is one
// cell sharing the entire tail. A non-maximal id splices: the cells above it are
// rebuilt and everything below is shared.
func (p Scopes) Add(s *Scope) Scopes {
	if s == nil {
		return p
	}
	node, changed := insert(p.node, s)
	if !changed {
		return p
	}
	return Scopes{node: node}
}

// insert returns the chain with s added and whether anything changed. It is
// recursive rather than iterative because the rebuilt prefix has to be relinked
// from the splice point outward, and the recursion depth is bounded by the
// number of members above s — zero for every add the tree actually performs.
func insert(n *scopeLink, s *Scope) (*scopeLink, bool) {
	if n == nil {
		return &scopeLink{scope: s, size: 1}, true
	}
	if n.scope == s {
		return n, false
	}
	if n.scope.ID() < s.ID() {
		return &scopeLink{scope: s, tail: n, size: n.size + 1}, true
	}
	tail, changed := insert(n.tail, s)
	if !changed {
		return n, false
	}
	return &scopeLink{scope: n.scope, tail: tail, size: tail.size + 1}, true
}

// Remove returns the set without target, or the receiver when target is absent.
//
// Removing the head returns the shared tail and allocates NOTHING. That is the
// intro-scope flip, the single hottest scope-set write in the expander, and it is
// the operation this representation exists for.
func (p Scopes) Remove(target *Scope) Scopes {
	node, changed := remove(p.node, target)
	if !changed {
		return p
	}
	return Scopes{node: node}
}

// remove returns the chain without target and whether anything changed.
func remove(n *scopeLink, target *Scope) (*scopeLink, bool) {
	if n == nil {
		return nil, false
	}
	if n.scope == target {
		return n.tail, true
	}
	// Descending order: once past target's id it cannot be below.
	if n.scope.ID() < target.ID() {
		return n, false
	}
	tail, changed := remove(n.tail, target)
	if !changed {
		return n, false
	}
	size := int32(1)
	if tail != nil {
		size = tail.size + 1
	}
	return &scopeLink{scope: n.scope, tail: tail, size: size}, true
}

// Flip toggles target's presence: the core of syntax-local-introduce.
func (p Scopes) Flip(target *Scope) Scopes {
	if p.Has(target) {
		return p.Remove(target)
	}
	return p.Add(target)
}

// SubsetOf reports whether p is a subset of u — Flatt's binding-resolution
// predicate, bindingScopes ⊆ useScopes, with p the binder's set.
//
// The merge is O(|p|+|u|) because both chains are id-ordered, and it carries
// three O(1) rejects the slice form cannot have:
//
//   - a larger set cannot be a subset of a smaller one;
//   - max(p) > max(u) is sound because max(p) ∈ p, so p ⊆ u would force
//     max(p) ≤ max(u);
//   - a shared spine is trivially a subset of itself, which hits constantly
//     once sets share tails.
func (p Scopes) SubsetOf(u Scopes) bool {
	if p.node == nil {
		return true
	}
	if u.node == nil {
		return false
	}
	if p.node == u.node {
		return true
	}
	if p.node.size > u.node.size {
		return false
	}
	if p.node.scope.ID() > u.node.scope.ID() {
		return false
	}
	a, b := p.node, u.node
	for a != nil {
		if b == nil {
			return false
		}
		if a == b {
			return true
		}
		if a.scope == b.scope {
			a, b = a.tail, b.tail
			continue
		}
		// b's head outranks a's, so a's head may still appear further down b.
		if b.scope.ID() > a.scope.ID() {
			b = b.tail
			continue
		}
		// b has passed a's id without matching it, so a's head is absent.
		return false
	}
	return true
}

// All returns an iterator over the members in descending-id order. Prefer it to
// ForEach wherever the loop body needs `break`, `continue` or an early `return`
// from the enclosing function — a callback cannot express those. Both are
// allocation-free; see docs/dev/iteration-idioms.md for when each shape applies.
func (p Scopes) All() iter.Seq[*Scope] {
	return func(yield func(*Scope) bool) {
		for n := p.node; n != nil; n = n.tail {
			if !yield(n.scope) {
				return
			}
		}
	}
}

// ForEach visits every member in descending-id order, allocation-free.
func (p Scopes) ForEach(fn func(s *Scope)) {
	for n := p.node; n != nil; n = n.tail {
		fn(n.scope)
	}
}

// Fingerprint builds a deterministic string from the set, so the set can key a
// map: two sets with the same members produce the same string, and any differing
// set a different one. The empty set fingerprints to "", and the output contains
// only [0-9,].
//
// THE INVARIANT IS "ONE TOTAL ORDER, APPLIED BY ONE FUNCTION", NOT "KEEP THE
// OLD OUTPUT". ScopeFingerprint formats ids to strings and then slices.Sort's
// the STRINGS, so {2,10} fingerprints to "10,2"; walking an id-sorted chain
// yields "2,10". Both cannot hold, and nothing requires the old spelling —
// every producer routes through one function, and match.FreeIdName's guarantee
// is the character class, which survives either ordering.
//
// MEASURED after the flip: all four callers use the equivalence relation alone
// and NONE is order-observable, so the output change cost nothing. An earlier
// version of this comment predicted that a golden ordering in
// pkg/internal/validate/frame_reclaim_build_test.go would move with it. That
// prediction was WRONG — the test sorts both sides with the same comparator, so
// it is order-independent by construction — and it is corrected here rather than
// quietly deleted, because a plausible-but-false prediction in a doc comment is
// how a later reader acquires a wrong model.
func (p Scopes) Fingerprint() string {
	if p.node == nil {
		return ""
	}
	var b strings.Builder
	for n := p.node; n != nil; n = n.tail {
		if n != p.node {
			b.WriteByte(',')
		}
		b.WriteString(strconv.FormatUint(n.scope.ID(), 10))
	}
	return b.String()
}
