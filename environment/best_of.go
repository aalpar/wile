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
// candidate with the largest scopeCount; ties are won by the most-recently
// seen candidate (the visit order during a parent-chain walk is innermost
// first, and Flatt's model says the most specific binding wins).
//
// This generalises the candidate-tracking loop that appears verbatim in
// EnvironmentFrame.GetBinding and EnvironmentFrame.GetLocalIndex (see
// plans/2026-05-09-environment-structural-reduction.md, Finding 6 +
// Opportunity 1). Callers consume the helper through the consider method,
// which returns true exactly when a perfect-match candidate is found
// (scopeCount == target > 0), permitting an early-exit from the walk.
type bestOf[T any] struct {
	item       T
	scopeCount int
	has        bool
}

// consider feeds one candidate into the accumulator and reports whether
// the walk should stop. A "perfect match" — non-empty scope set whose
// size equals the target — wins immediately; otherwise the candidate with
// the largest scope count to date is kept.
//
// target is the scope count of the reference identifier (typically
// len(scopes) at the call site). Passing target == 0 means the caller
// has no specific size to match against, and no perfect-match shortcut
// will fire.
func (b *bestOf[T]) consider(item T, scopeCount, target int) bool {
	if scopeCount > 0 && scopeCount == target {
		b.item = item
		b.scopeCount = scopeCount
		b.has = true
		return true
	}
	if !b.has || scopeCount > b.scopeCount {
		b.item = item
		b.scopeCount = scopeCount
		b.has = true
	}
	return false
}
