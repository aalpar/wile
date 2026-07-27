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

package syntax

import "github.com/aalpar/wile/pkg/values"

// Scope-set primitives are defined in package values (the empty-list
// duality merge — see values/scope.go). Wrapper functions keep the
// pkg/syntax import surface unchanged for existing callers; the
// implementations live with the Scope type they operate on.

// ScopesMatch checks if two sets of scopes are compatible for binding resolution.
// See values.ScopesMatch for the full hygiene-model documentation.
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
	return values.ScopesMatch(useScopes, bindingScopes)
}

// ScopeFingerprint builds a deterministic map-key string from a scope set.
// See values.ScopeFingerprint for the full documentation.
func ScopeFingerprint(scopes []*Scope) string {
	return values.ScopeFingerprint(scopes)
}

// ScopesCompatible checks whether a binding's scopes can match a reference's.
// A binding with no scopes matches any reference.
func ScopesCompatible(bindingScopes, useScopes []*Scope) bool {
	return values.ScopesCompatible(bindingScopes, useScopes)
}

// ScopeSet is a hygiene query constraint (all / empty / specific). Defined in
// package values alongside the Scope type; re-exported here so environment and
// compiler code can spell it syntax.ScopeSet beside syntax.ScopesCompatible.
type ScopeSet = values.ScopeSet

// AllScopes returns the wildcard scope-set query. See values.AllScopes.
func AllScopes() ScopeSet {
	return values.AllScopes()
}

// ScopesOf returns a query constrained to the given scope set (nil ≡ empty set,
// not wildcard). See values.ScopesOf.
func ScopesOf(scopes []*Scope) ScopeSet {
	return values.ScopesOf(scopes)
}

// EmptyScopes returns the ambient (empty) scope-set query. See values.EmptyScopes.
func EmptyScopes() ScopeSet {
	return values.EmptyScopes()
}

// FlipScopeInSet toggles the presence of a scope in a set.
// It is the set-level half of FlipScope, whose only intended consumer
// (syntax-local-introduce) is not wired; see FlipScope.
func FlipScopeInSet(scopes []*Scope, target *Scope) []*Scope {
	return values.FlipScopeInSet(scopes, target)
}

// mapSyntaxTree recursively transforms a syntax tree.
// The function fn is applied to each node bottom-up: children are transformed first,
// then the parent is constructed with the transformed children.
//
// For pairs, this means:
// 1. Recursively transform car and cdr
// 2. Create new pair with transformed children
// 3. fn is NOT called on the pair itself (only on leaf nodes like symbols)
//
// This is the shared traversal logic used by both AddScope and FlipScope.
func mapSyntaxTree(stx SyntaxValue, fn func(SyntaxValue) SyntaxValue) SyntaxValue {
	if stx == nil {
		return nil
	}

	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return s
		}

		// Recursively transform car and cdr
		var newCar, newCdr SyntaxValue
		if s.Values[0] != nil {
			newCar = mapSyntaxTree(s.Values[0], fn)
		}
		if s.Values[1] != nil {
			newCdr = mapSyntaxTree(s.Values[1], fn)
		}

		// Structural sharing: when a tree transformation leaves children unchanged,
		// the original node is returned instead of allocating a new one. This is
		// the core idea behind persistent data structures (Okasaki, 1998). Most
		// syntax tree nodes are not symbols, so most pairs pass the identity check.
		// See BIBLIOGRAPHY.md "Structural Sharing".
		if newCar == s.Values[0] && newCdr == s.Values[1] {
			return s
		}
		return NewSyntaxCons(newCar, newCdr, s.SourceContext())

	case *SyntaxSymbol:
		// Apply transformation to symbols
		return fn(s)

	case *SyntaxVector:
		if s == nil || len(s.Values) == 0 {
			return s
		}

		// Recursively transform each element
		newValues := make([]SyntaxValue, len(s.Values))
		changed := false
		for i, elem := range s.Values {
			if elem != nil {
				newValues[i] = mapSyntaxTree(elem, fn)
				if newValues[i] != elem {
					changed = true
				}
			} else {
				newValues[i] = nil
			}
		}

		// Return original if nothing changed (optimization)
		if !changed {
			return s
		}

		// Return new vector with transformed elements
		return NewSyntaxVector(s.SourceContext(), newValues...)

	default:
		// Other types (SyntaxObject, etc.) - check if they support the transformation
		// This handles types that might implement the transformation interface
		return fn(stx)
	}
}

// FlipScope toggles the presence of a scope on a syntax object.
// Returns a new syntax object with the scope flipped.
// Intended for syntax-local-introduce, which is currently NOT wired: the
// expander context never carries an introduction scope, so that primitive
// raises werr.ErrNotImplemented and this function has no live production
// caller.
func FlipScope(stx SyntaxValue, scope *Scope) SyntaxValue {
	if stx == nil || scope == nil {
		return stx
	}

	return mapSyntaxTree(stx, func(node SyntaxValue) SyntaxValue {
		switch s := node.(type) {
		case *SyntaxSymbol:
			return flipScopeOnSymbol(s, scope)
		default:
			// SyntaxObject and other types don't store meaningful scopes
			return node
		}
	})
}

// flipScopeOnSymbol flips a scope on a SyntaxSymbol.
func flipScopeOnSymbol(sym *SyntaxSymbol, scope *Scope) *SyntaxSymbol {
	if sym == nil {
		return nil
	}
	sctx := sym.SourceContext()
	if sctx == nil {
		sctx = &SourceContext{}
	}
	newScopes := FlipScopeInSet(sctx.Scopes, scope)
	newSctx := sctx.Clone()
	newSctx.Scopes = newScopes
	return &SyntaxSymbol{
		Sym:             sym.Sym,
		syntaxBase:      values.NewSyntaxBase(newSctx),
		ResolvedBinding: sym.ResolvedBinding,
	}
}

// AddScopeToSyntax adds a scope to a syntax object.
// Returns a new syntax object with the scope added.
// Symbols, pairs, and vectors receive the scope; self-evaluating literals
// (SyntaxObject) and other types are returned unchanged.
// Used by the binding-form expanders (let, letrec, lambda, let-syntax,
// include).
func AddScopeToSyntax(stx SyntaxValue, scope *Scope) SyntaxValue {
	if stx == nil || scope == nil {
		return stx
	}

	switch s := stx.(type) {
	case *SyntaxSymbol:
		return s.AddScope(scope)
	case *SyntaxPair:
		return s.AddScope(scope)
	case *SyntaxVector:
		return s.AddScope(scope)
	default:
		// SyntaxObject and other types don't need scopes
		return stx
	}
}
