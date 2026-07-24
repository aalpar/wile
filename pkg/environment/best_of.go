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

package environment

import "github.com/aalpar/wile/pkg/syntax"

// bestOf accumulates the highest-weight candidate seen so far during a
// monotone walk. "Best" is the candidate with the largest weight; ties
// are kept on the *first* candidate seen.
//
// The first use case is Flatt-style maximal-resolution over scope-tagged
// bindings (EnvironmentFrame.GetBinding and EnvironmentFrame.GetLocalIndex —
// see memory/2026-05-09-environment-structural-reduction.md, Finding 6 +
// Opportunity 1). In that setting the weight is the size of a binding's
// scope set: the parent-chain walk is innermost-first, so first-seen-on-tie
// corresponds to "innermost wins" — which is exactly Flatt's tie-breaking
// rule. The helper itself stays domain-neutral; callers attach whatever
// "weight" semantics they want.
//
// API shape: callers split each step into (1) shouldRecord, a pure
// predicate, and (2) record, which actually stores the candidate. This
// split lets the caller allocate the candidate value lazily — important
// for callers whose item construction is non-trivial (e.g.
// GetLocalIndex's NewLocalIndex(slot, depth)). A combined
// "consider(item, weight, target)" API would force the caller to
// allocate the candidate on every visit, even when the candidate is
// not the new best.
type bestOf[T any] struct {
	item   T
	weight int
	has    bool
}

// shouldRecord reports whether weight beats (or matches) the current
// best, and whether the walk should stop because of a perfect match.
//
// A perfect match — strictly positive weight that equals target — wins
// immediately and returns done = true. Otherwise the new candidate
// becomes the best iff weight is strictly greater than the current
// best's; ties keep the existing first-seen candidate.
//
// target is the weight the caller is matching against (typically
// len(scopes) at the binding-resolution call sites). target == 0
// means the caller has no specific size to match against, and no
// perfect-match shortcut fires.
//
// shouldRecord does not allocate; the caller MUST call record(item,
// weight) to commit the candidate when shouldRecord returns
// record = true.
func (p *bestOf[T]) shouldRecord(weight, target int) (record, done bool) {
	if weight > 0 && weight == target {
		return true, true
	}
	if !p.has || weight > p.weight {
		return true, false
	}
	return false, false
}

// record commits the given candidate as the current best. Callers MUST
// call shouldRecord first and only invoke record when it returned
// record = true.
func (p *bestOf[T]) record(item T, weight int) {
	p.item = item
	p.weight = weight
	p.has = true
}

// Result returns the recorded candidate and whether any candidate was
// recorded. This is the safe accessor for callers that cannot rely on
// the zero value of T being a meaningful "absent" sentinel: if has is
// false, item is T's zero value, which is fine for pointers (nil) but
// could lie for value types or string-like Ts.
func (p *bestOf[T]) Result() (T, bool) {
	return p.item, p.has
}

// scopedBestOf augments bestOf with Flatt ambiguity detection. bestOf ranks by
// integer weight alone and cannot see incomparability: two candidates of equal
// maximal cardinality carry distinct — hence mutually incomparable — scope sets,
// so neither is THE maximal subset of the reference. Racket raises "ambiguous
// binding" for such a reference; scopedBestOf records that an ambiguous tie
// occurred so the caller can raise instead of silently returning the first-seen
// (innermost) candidate.
//
// It preserves bestOf's shouldRecord/record split so callers keep deferring
// candidate construction (e.g. GetLocalIndex's NewLocalIndex allocation) to the
// visits that actually become the new best. bestScopes records only the current
// best's scope set — one slice-header assignment per record, no allocation.
type scopedBestOf[T any] struct {
	best       bestOf[T]
	bestScopes []*syntax.Scope
	ambiguous  bool
}

// shouldRecord mirrors bestOf.shouldRecord and additionally flags an ambiguous
// tie. On a non-recording visit that has the same sub-target cardinality as the
// current best but a DIFFERENT scope set, neither set is a subset of the other
// (equal cardinality + distinct ⇒ incomparable), so the reference is ambiguous.
// ScopesMatch(scopes, bestScopes) reports bestScopes ⊆ scopes; at equal
// cardinality that holds iff the sets are equal, so its negation is exactly
// "different set". The caller MUST call record(item, scopes) iff rec is true.
func (p *scopedBestOf[T]) shouldRecord(scopes []*syntax.Scope, target int) (rec, done bool) {
	weight := len(scopes)
	rec, done = p.best.shouldRecord(weight, target)
	if rec {
		return rec, done
	}
	if p.best.has && weight < target && weight == p.best.weight &&
		!syntax.ScopesMatch(scopes, p.bestScopes) {
		p.ambiguous = true
	}
	return rec, done
}

// record commits item as the current best and clears any prior ambiguity: a
// strictly-greater-weight candidate is a new unique maximum that supersedes any
// tie recorded at a lower weight. Callers MUST call shouldRecord first and only
// invoke record when it returned rec = true.
func (p *scopedBestOf[T]) record(item T, scopes []*syntax.Scope) {
	p.best.record(item, len(scopes))
	p.bestScopes = scopes
	p.ambiguous = false
}

// Result returns the recorded candidate and whether any was recorded.
func (p *scopedBestOf[T]) Result() (T, bool) {
	return p.best.Result()
}

// Ambiguous reports whether the maximal-weight candidate was not unique — two
// incomparable scope sets tied for the largest subset of the reference.
func (p *scopedBestOf[T]) Ambiguous() bool {
	return p.ambiguous
}
