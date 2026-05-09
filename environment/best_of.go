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

// bestOf accumulates the highest-weight candidate seen so far during a
// monotone walk. "Best" is the candidate with the largest weight; ties
// are kept on the *first* candidate seen.
//
// The first use case is Flatt-style maximal-resolution over scope-tagged
// bindings (EnvironmentFrame.GetBinding and EnvironmentFrame.GetLocalIndex —
// see plans/2026-05-09-environment-structural-reduction.md, Finding 6 +
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
