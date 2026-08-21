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
	"slices"
	"strconv"
	"strings"
	"sync/atomic"
)

// Scope is an identity marker for macro hygiene.
// Each macro invocation creates a fresh Scope. Hygiene checking uses pointer
// equality to determine if a binding's scopes are a subset of a reference's scopes.
// This implements Flatt's "sets of scopes" model where scopes are just unique tags,
// not environment hierarchies.
type Scope struct {
	id uint64 // ensures unique pointer identity (empty structs can share addresses in Go)
	// IsRebinding indicates whether this scope can potentially rebind auxiliary syntax.
	// True for let-syntax/letrec-syntax scopes which create local macro bindings.
	// False for with-binding-scope which only adds scopes for binding hygiene.
	// This distinction is used in literalScopesMatchWithDef
	// (pkg/internal/match/syntax_adapter.go) to correctly handle auxiliary
	// syntax like => and else in cond/case. Follow that name, not the
	// literalScopesMatchWithChecker delegate beside it: the delegate is the
	// unpinned comparison and today has no production caller.
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
func ScopeFingerprint(scopes []*Scope) string {
	if len(scopes) == 0 {
		return ""
	}
	ids := make([]string, len(scopes))
	for i, s := range scopes {
		if s == nil {
			ids[i] = "0"
			continue
		}
		ids[i] = strconv.FormatUint(s.ID(), 10)
	}
	slices.Sort(ids)
	return strings.Join(ids, ",")
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
//	  occurrence as reference — plus an equality half on the difference, which
//	  no subset relation can express; see that function).
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
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
	// A binding matches a use if all of the binding's scopes are present in the use's scopes.
	// This is the subset relationship: bindingScopes ⊆ useScopes
	//
	// Empty binding scopes (top-level) match everything since {} ⊆ X for all X.

	// A larger set cannot be a subset of a smaller one.
	if len(bindingScopes) > len(useScopes) {
		return false
	}
	for _, bindScope := range bindingScopes {
		if !slices.Contains(useScopes, bindScope) {
			return false
		}
	}
	return true
}

// ScopesCompatible checks whether a binding with bindingScopes can match a
// reference with useScopes. A binding with no scopes (top-level / pre-hygiene)
// matches any reference.
//
// Both the environment's resolveLocal and the validator's duplicate-binding
// detection use this single function so scope resolution cannot diverge.
//
// Note: nil useScopes does NOT mean "match any" here. A nil reference scope
// set means "this reference has no scopes" and behaves like an empty set —
// only bindings with no scopes match. Callers that want "match any" ask for it
// with AllScopes and short-circuit on ScopeSet.IsAll() before reaching this
// function (see EnvironmentFrame.resolveLocal).
func ScopesCompatible(bindingScopes, useScopes []*Scope) bool {
	if len(bindingScopes) == 0 {
		return true
	}
	return ScopesMatch(useScopes, bindingScopes)
}

// HasScope checks if a scope set contains a specific scope
func HasScope(scopes []*Scope, target *Scope) bool {
	return slices.Contains(scopes, target)
}

// AddScopeToSet adds a scope to a set if not already present
func AddScopeToSet(scopes []*Scope, newScope *Scope) []*Scope {
	if slices.Contains(scopes, newScope) {
		return scopes
	}
	return append(scopes, newScope)
}

// RemoveScopeFromSet removes a scope from a set
func RemoveScopeFromSet(scopes []*Scope, target *Scope) []*Scope {
	result := make([]*Scope, 0, len(scopes))
	for _, s := range scopes {
		if s != target {
			result = append(result, s)
		}
	}
	return result
}

// FlipScopeInSet toggles the presence of a scope in a set.
// If the scope is present, it is removed; if absent, it is added.
// This is the core operation for syntax-local-introduce.
func FlipScopeInSet(scopes []*Scope, target *Scope) []*Scope {
	if HasScope(scopes, target) {
		return RemoveScopeFromSet(scopes, target)
	}
	return AddScopeToSet(scopes, target)
}
