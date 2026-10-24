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
// scope set. bestOf's own first-seen-on-tie is a raw accumulator policy, NOT
// Flatt's rule: the binding-resolution call sites wrap it in scopedBestOf,
// which detects an incomparable equal-cardinality tie and raises
// ErrAmbiguousBinding instead of keeping the first candidate. The helper
// itself stays domain-neutral; callers attach whatever "weight" semantics
// they want.
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
	bestScopes syntax.Scopes
	ambiguous  bool
}

// shouldRecord mirrors bestOf.shouldRecord and additionally flags an ambiguous
// tie. On a non-recording visit that has the same sub-target cardinality as the
// current best but a DIFFERENT scope set, neither set is a subset of the other
// (equal cardinality + distinct ⇒ incomparable), so the reference is ambiguous.
// ScopesMatch(scopes, bestScopes) reports bestScopes ⊆ scopes; at equal
// cardinality that holds iff the sets are equal, so its negation is exactly
// "different set". The caller MUST call record(item, scopes) iff rec is true.
func (p *scopedBestOf[T]) shouldRecord(scopes syntax.Scopes, target int) (rec, done bool) {
	weight := scopes.Len()
	rec, done = p.best.shouldRecord(weight, target)
	if rec {
		return rec, done
	}
	if p.tiesBest(weight, target, scopes) {
		p.ambiguous = true
	}
	return rec, done
}

// tiesBest is the one definition of an ambiguous tie: a candidate of the same
// sub-target weight as the current best whose scope set differs from it. Equal
// cardinality plus distinct ⇒ incomparable, so neither is THE maximal subset of
// the reference. ScopesMatch(scopes, bestScopes) reports bestScopes ⊆ scopes; at
// equal cardinality that holds iff the sets are equal, so its negation is exactly
// "different set". It is only meaningful for a candidate that did not beat the
// best.
func (p *scopedBestOf[T]) tiesBest(weight, target int, scopes syntax.Scopes) bool {
	return p.best.has && weight < target && weight == p.best.weight &&
		!syntax.ScopesMatch(scopes, p.bestScopes)
}

// record commits item as the current best and clears any prior ambiguity: a
// strictly-greater-weight candidate is a new unique maximum that supersedes any
// tie recorded at a lower weight. Callers MUST call shouldRecord first and only
// invoke record when it returned rec = true.
func (p *scopedBestOf[T]) record(item T, scopes syntax.Scopes) {
	p.best.record(item, scopes.Len())
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

// tiedBestOf is scopedBestOf that can also KEEP the tied candidates, for a caller
// that must cover every binder an ambiguous reference might denote. It is a
// wrapper rather than fields on scopedBestOf because the global probe zeroes
// scopedBestOf values on a hot path and never wants the ties. The tie rule is
// scopedBestOf.tiesBest, so the two cannot disagree on what a tie is.
//
// Ties are kept in visit order after the best, one per distinct scope set. A new
// best supersedes them, as it supersedes the ambiguity flag. With collect false
// it is a scopedBestOf and allocates nothing extra.
type tiedBestOf[T any] struct {
	scopedBestOf[T]
	collect   bool
	ties      []T
	tieScopes []syntax.Scopes
}

// shouldRecordTied is scopedBestOf.shouldRecord that also reports tie: the
// candidate is incomparable with the best and not already kept. The caller MUST
// call recordTie(item, scopes) iff tie is true.
func (p *tiedBestOf[T]) shouldRecordTied(scopes syntax.Scopes, target int) (rec, done, tie bool) {
	rec, done = p.scopedBestOf.shouldRecord(scopes, target)
	if rec || !p.collect {
		return rec, done, false
	}
	if !p.tiesBest(scopes.Len(), target, scopes) {
		return false, done, false
	}
	for _, k := range p.tieScopes {
		if syntax.ScopesMatch(scopes, k) {
			return false, done, false
		}
	}
	return false, done, true
}

// record commits item as the new best and drops the ties of the old one.
func (p *tiedBestOf[T]) record(item T, scopes syntax.Scopes) {
	p.scopedBestOf.record(item, scopes)
	p.ties = p.ties[:0]
	p.tieScopes = p.tieScopes[:0]
}

// recordTie keeps item as a tied candidate of the current best.
func (p *tiedBestOf[T]) recordTie(item T, scopes syntax.Scopes) {
	p.ties = append(p.ties, item)
	p.tieScopes = append(p.tieScopes, scopes)
}
