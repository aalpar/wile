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

// ScopeSet is a hygiene *query* constraint used when resolving a name to a
// binding. It is either the wildcard "all" — match any binding of the name,
// ignoring scopes and resolving by slot order — or a specific scope set, resolved
// hygienically per Flatt's model with the empty set included.
//
// It collapses a state that was previously smeared across three carriers: a
// []*Scope slice whose nil value meant "match any" at some read sites and "the
// empty set" at others, plus a matchAny bool parameter and a scopeKeyed bool
// field bolted on where the slice could not carry the distinction. The three
// states — all / empty / specific — are now one value, so on the reference/query
// side the same nil no longer answers two opposite questions. The
// binder-creation path in LocalEnvironmentFrame.MaybeCreateLocalBinding was the
// last holdout and no longer reads a nil []*Scope as "match any" either; it takes
// nil as the empty set, which is the only reading that means anything on the
// creation side, since a binder's scope set is its identity.
//
// A binder's OWN scope set is not a ScopeSet: it is always a concrete []*Scope,
// because "all" is meaningless for identity. ScopeSet models the reference/query
// side only.
//
// The zero value is the empty set — a specific, non-wildcard query — not the
// wildcard. "all" must be asked for explicitly via AllScopes, so a forgotten
// initialization can never silently widen a resolution, which is the whole point
// of the type.
type ScopeSet struct {
	all bool
	// scopes is meaningful only when !all. Its zero value IS the empty set, so
	// the nil-vs-empty distinction the environment layer once depended on is not
	// merely carried by `all` — it is no longer expressible here at all.
	scopes Scopes
}

// AllScopes returns the wildcard query: any binding of the name matches,
// resolved by slot order. This is the introspection / bare-symbol reflective
// read semantics — "any binding of this name" rather than "the empty scope set".
func AllScopes() ScopeSet {
	return ScopeSet{all: true}
}

// ScopesOf returns a query constrained to the given scope set. The zero Scopes
// is the EMPTY set here (equivalent to EmptyScopes), NOT the wildcard — use
// AllScopes for that. This is the inverse of the historical footgun where a nil
// slice silently meant "match any", and under a value type the footgun is not
// expressible: there is no third state to mistake for a wildcard.
func ScopesOf(scopes Scopes) ScopeSet {
	return ScopeSet{scopes: scopes}
}

// EmptyScopes returns the ambient (empty) scope-set query: the constraint a
// reference written outside any macro expansion carries. It is ScopesOf(nil)
// under a name that states the intent, replacing the AmbientScopes() empty-slice
// sentinel.
func EmptyScopes() ScopeSet {
	return ScopeSet{}
}

// IsAll reports whether this is the wildcard query.
func (q ScopeSet) IsAll() bool {
	return q.all
}

// IsEmpty reports whether this is the ambient (empty, non-wildcard) query.
func (q ScopeSet) IsEmpty() bool {
	return !q.all && q.scopes.IsEmpty()
}

// Scopes returns the underlying scope set for a specific or empty query. It is
// meaningless for AllScopes and returns the empty set there.
func (q ScopeSet) Scopes() Scopes {
	if q.all {
		return Scopes{}
	}
	return q.scopes
}

// String returns a debug representation: "all-scopes" for the wildcard, or
// "scopes{...}" with the sorted decimal scope IDs (empty for the empty set),
// reusing ScopeFingerprint so the format matches the map-key form.
func (q ScopeSet) String() string {
	if q.all {
		return "all-scopes"
	}
	return "scopes{" + q.scopes.Fingerprint() + "}"
}
