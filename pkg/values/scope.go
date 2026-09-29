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
	"fmt"
	"sync/atomic"
)

// Scope is an identity marker for macro hygiene.
// Each macro invocation creates a fresh Scope. Hygiene checking uses pointer
// equality to determine if a binding's scopes are a subset of a reference's scopes.
// This implements Flatt's "sets of scopes" model where scopes are just unique tags,
// not environment hierarchies.
type Scope struct {
	id uint64 // ensures unique pointer identity (empty structs can share addresses in Go)
	// IsRebinding marks a let-syntax/letrec-syntax scope. Nothing consults it:
	// pattern-literal matching decides shadowing by binding, not by this flag
	// (literalScopesMatchWithDef, pkg/internal/match/syntax_adapter.go).
	IsRebinding bool
	// Label is an optional human-readable description for debugging.
	// Examples: "lambda", "let-syntax", "intro:my-macro", "library:(wile kanren)".
	Label string
}

// nextScopeID is a counter for generating unique scope identities
var nextScopeID atomic.Uint64

// NewScope creates a new scope with unique identity for hygiene tracking.
// By default, scopes are not rebinding scopes.
func NewScope() *Scope {
	id := nextScopeID.Add(1)
	return &Scope{id: id}
}

// NewScopeWithLabel creates a new scope with a human-readable label for debugging.
// The label has no semantic effect — it is purely for diagnostics.
func NewScopeWithLabel(label string) *Scope {
	id := nextScopeID.Add(1)
	return &Scope{id: id, Label: label}
}

// NewRebindingScope creates a new scope that can potentially rebind auxiliary syntax.
// Used by let-syntax and letrec-syntax to mark scopes that could shadow literals.
func NewRebindingScope() *Scope {
	id := nextScopeID.Add(1)
	return &Scope{id: id, IsRebinding: true}
}

// NewRebindingScopeWithLabel creates a new rebinding scope with a label.
func NewRebindingScopeWithLabel(label string) *Scope {
	id := nextScopeID.Add(1)
	return &Scope{id: id, IsRebinding: true, Label: label}
}

// ID returns the unique identifier for this scope.
// This can be used as a macro application ID for tracing.
func (p *Scope) ID() uint64 {
	if p == nil {
		return 0
	}
	return p.id
}

// String returns a human-readable representation of the scope.
// If a label is set, returns "scope:ID(label)"; otherwise "scope:ID".
func (p *Scope) String() string {
	if p == nil {
		return "scope:nil"
	}
	if p.Label != "" {
		return fmt.Sprintf("scope:%d(%s)", p.id, p.Label)
	}
	return fmt.Sprintf("scope:%d", p.id)
}

// ScopeFingerprint builds a deterministic string from a scope set, so the set
// can key a map: two sets holding the same scopes (in any order) produce the
// same fingerprint, and any differing set produces a different one. Identity is
// by scope ID, matching ScopesMatch's pointer-identity model — scopes carry no
// structure to compare. The empty set fingerprints to "", and a non-empty set
// to sorted decimal IDs joined by ',', so the output contains only [0-9,].
// ORDER CHANGED with the Scopes flip, deliberately. This used to format the ids
// to strings and sort the STRINGS, so {2,10} fingerprinted to "10,2"; it now
// walks an id-ordered chain and yields "10,9,2" where the string sort gave
// "10,2,9". The invariant was never the exact output, it is "one total order,
// applied by one function" — every producer routes through here, and
// match.FreeIdName's guarantee is the [0-9,] character class, which both
// orderings satisfy. Measured 2026-09-27: all four callers use only the
// equivalence relation, none is order-observable. The one observable site is a
// golden test ordering in validate's frame_reclaim_build_test.go.
func ScopeFingerprint(scopes Scopes) string {
	return scopes.Fingerprint()
}

// ScopesMatch checks if two sets of scopes are compatible for binding resolution.
// This implements the core hygiene check using Flatt's "sets of scopes" model:
// A reference matches a binding if the binding's scope set is a SUBSET of the
// reference's scope set.
//
// Powerset lattice P(S) (Flatt 2016, §3.2). Binding resolution is a
// subset test on finite scope sets.
//
//	match(ref, bind) ⟺ bind.scopes ⊆ ref.scopes
//	resolve(ref) = argmax { |s| : s ⊆ ref.scopes } over all bindings
//
//	where ref = useScopes, bind = bindingScopes,
//	s = a candidate binding's scope set, |s| = scope count.
//
//	Operations on P(S):
//	  AddScopeToSet    = join (union)
//	  RemoveScopeFromSet = relative complement
//	  FlipScopeInSet   = symmetric difference (XOR in Z/2Z^S)
//
//	Invariant: {} ⊆ X for all X — top-level bindings (empty scope set)
//	  match every reference. The argmax selects the most specific binding.
//	Constrains: GetLocalIndex (implements resolve/argmax),
//	  GetBinding (maximal resolution for scoped lookups),
//	  CompileSymbol (dispatches scoped vs unscoped lookup),
//	  TemplateDenotesPatternVariable (pattern variable as binder, template
//	  occurrence as reference — the subset relation and nothing more, because
//	  a macro use's unflipped use-site scope keeps an outer macro's introduced
//	  identifier out of the relation; see that function).
//	Constrained by: NewScope (each macro invocation creates a fresh scope),
//	  FlipScopeInSet (syntax-local-introduce toggles scope membership).
//
// See BIBLIOGRAPHY.md "Binding as Sets of Scopes".
//
// This ensures:
// - Top-level bindings (empty scope set) match any reference: {} ⊆ X for all X
// - A macro-introduced binding only matches references with that macro's intro scope
// - User bindings don't capture macro-introduced identifiers (different scope sets)
//
// Implementation note: Linear scan with pointer equality is intentionally used here.
// Scope sets are typically 0-4 elements (one per lexical form: macro invocation, lambda,
// let-syntax, with-binding-scope). For sets this small, linear scan is faster than
// hash-based or bitmap approaches due to cache locality and zero allocation overhead.
// It delegates to Scopes.SubsetOf and adds no rule of its own. Keeping it is
// what lets the ~20 external call sites read as the domain operation rather than
// as a method call in argument-reversed order; [I135-freefn] retires it.
//
// Implementation note that used to live here: the linear scan with pointer
// equality was intentional while a set was a slice, on the grounds that sets are
// "typically 0-4 elements". That premise was false outside the default
// configuration (84 members under the Scheme syntax layer, ~250 in the
// nested-let probe), and the scan is what made this O(n^2). SubsetOf is an
// id-ordered merge with three O(1) rejects, the useful one being a shared spine
// — which is the common case, since a reference's set is its binder's set plus a
// scope.
func ScopesMatch(useScopes, bindingScopes Scopes) bool {
	return bindingScopes.SubsetOf(useScopes)
}

// ScopesCompatible checks whether a binding with bindingScopes can match a
// reference with useScopes. A binding with no scopes (top-level / pre-hygiene)
// matches any reference.
//
// It is the entry point MOST consumers matching a REFERENCE against a candidate
// BINDER share, so scope resolution cannot diverge across them. This comment used
// to name two; measured 2026-09-10 there are NINE call sites in three packages —
// environment's resolveLocal, probeTiersLocked, probeBulkLocked and
// resolveAtCoordsLocked; validate's letBoundClosureEscapes, refMatchesBinder and
// nameSet.shadowLookup; compilation's sameBinder and
// freeVarCollector.boundInside.
//
// It is NOT every such consumer, and the exception is worth knowing:
// validate.resolveNodeByScopes (frame_reclaim_build.go) does a reference-vs-binder
// match and calls ScopesMatch DIRECTLY, spelling the subset out in its own comment.
// It is the frame-reclaim authority — its false positive is corruption, not a lost
// optimization — so a change to the relation has to reach it explicitly; it will
// not arrive by editing this function. (The two internal/match sites are different:
// they compare a pattern's scopes to a template's, not a reference to a binder.)
//
// The empty-set arm below is a SHORT-CIRCUIT, not the rule. ScopesMatch already
// answers true for an empty binding set — its length guard cannot fire when
// len(bindingScopes) is 0, and its loop is then vacuous — so deleting the arm
// changes no answer for any input, verified by reading and by a green
// `go test ./...` without it. The rule that ∅ matches every reference lives in
// ScopesMatch, and THAT is what a change to the scope-set encoding has to
// preserve; a task phrased as "keep this short-circuit" names the wrong artifact
// and an implementer who deletes it sees green and distrusts the rest.
//
// Note: nil useScopes does NOT mean "match any" here. A nil reference scope
// set means "this reference has no scopes" and behaves like an empty set —
// only bindings with no scopes match. Callers that want "match any" ask for it
// with AllScopes and short-circuit on ScopeSet.IsAll() before reaching this
// function (see EnvironmentFrame.resolveLocal).
func ScopesCompatible(bindingScopes, useScopes Scopes) bool {
	return bindingScopes.SubsetOf(useScopes)
}

// HasScope checks if a scope set contains a specific scope.
func HasScope(scopes Scopes, target *Scope) bool {
	return scopes.Has(target)
}

// AddScopeToSet adds a scope to a set if not already present.
//
// The result NEVER shares a backing array with the input. A bare
// append(scopes, newScope) would write into the caller's array whenever it has
// spare capacity, and RemoveScopeFromSet manufactures exactly that: it allocates
// cap len(scopes) and fills len(scopes)-1 entries, so every scope set that has
// ever had a scope removed carries one spare slot. Two symbols whose sets are
// slices of one such array then clobber each other's added scope.
//
// That is not hypothetical. A macro template identifier reaches the expander as
// one shared *SyntaxSymbol under the Scheme syntax layer (quote-syntax yields
// the literal, where the Go template producer minted a fresh copy per
// expansion), so two nested expansions of one macro flip their two DIFFERENT
// introduction scopes into the same spare slot: the second overwrote the first
// and the earlier expansion's binder and its own references stopped agreeing.
// (m1 (m1 #f)) over (syntax-rules () ((_ e) (let ((zz e)) (if zz zz 9))))
// failed with `no such binding "zz" with compatible scopes`; the binder held
// intro_outer and the reference read intro_inner out of the shared array.
// Pinned by TestAddScopeToSet_DoesNotAliasSpareCapacity.
// The aliasing hazard the comment above describes is now STRUCTURALLY absent,
// not merely guarded: a persistent chain has no spare capacity and no backing
// array to share, so two sets cannot clobber each other's added scope however
// they were derived. TestAddScopeToSet_DoesNotAliasSpareCapacity survives with
// its PRECONDITION retired — it can no longer build a set carrying a spare
// backing slot, because there is no backing array — and now asserts the property
// directly by membership. The structural-sharing half is pinned by
// TestScopesAddOfNewMaximumSharesTheTail.
func AddScopeToSet(scopes Scopes, newScope *Scope) Scopes {
	return scopes.Add(newScope)
}

// RemoveScopeFromSet removes a scope from a set. Removing the maximum — the
// intro-scope flip, and every removal measured in production — returns the
// shared tail and allocates nothing.
func RemoveScopeFromSet(scopes Scopes, target *Scope) Scopes {
	return scopes.Remove(target)
}

// FlipScopeInSet toggles the presence of a scope in a set.
// If the scope is present, it is removed; if absent, it is added.
// This is the core operation for syntax-local-introduce.
func FlipScopeInSet(scopes Scopes, target *Scope) Scopes {
	return scopes.Flip(target)
}
