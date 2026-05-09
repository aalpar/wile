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

// bestOf accumulates the best candidate seen so far during a Flatt-style
// maximal-resolution walk over scope-tagged bindings. "Best" is the
// candidate with the largest scopeCount; ties are kept on the *first*
// candidate seen (because the parent walk is innermost-first, the
// first-seen candidate is the innermost match — which is what Flatt's
// model selects when scope counts tie).
//
// This generalises the candidate-tracking loop that appears verbatim in
// EnvironmentFrame.GetBinding and EnvironmentFrame.GetLocalIndex (see
// plans/2026-05-09-environment-structural-reduction.md, Finding 6 +
// Opportunity 1).
//
// API shape: callers split each step into (1) shouldRecord, which is a
// pure predicate, and (2) record, which actually stores the candidate.
// This split lets the caller allocate the candidate value lazily —
// important for callers whose item construction is non-trivial (e.g.
// GetLocalIndex's NewLocalIndex(slot, depth)). A combined
// "consider(item, scopeCount, target)" API would force the caller to
// allocate the candidate on every visit, even when the candidate is
// not the new best.
type bestOf[T any] struct {
	item       T
	scopeCount int
	has        bool
}

// shouldRecord reports whether scopeCount beats (or matches) the current
// best, and whether the walk should stop because of a perfect match.
//
// A perfect match — non-empty scope set whose size equals target — wins
// immediately and returns done = true. Otherwise the new candidate
// becomes the best iff scopeCount is strictly greater than the current
// best's; ties keep the existing first-seen candidate.
//
// target is the scope count of the reference identifier (typically
// len(scopes) at the call site). target == 0 means the caller has no
// specific size to match against, and no perfect-match shortcut fires.
//
// shouldRecord does not allocate; the caller MUST call record(item,
// scopeCount) to commit the candidate when shouldRecord returns
// record = true.
func (p *bestOf[T]) shouldRecord(scopeCount, target int) (record, done bool) {
	if scopeCount > 0 && scopeCount == target {
		return true, true
	}
	if !p.has || scopeCount > p.scopeCount {
		return true, false
	}
	return false, false
}

// record commits the given candidate as the current best. Callers MUST
// call shouldRecord first and only invoke record when it returned
// record = true.
func (p *bestOf[T]) record(item T, scopeCount int) {
	p.item = item
	p.scopeCount = scopeCount
	p.has = true
}
