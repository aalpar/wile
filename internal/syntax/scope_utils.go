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

import "slices"

// ScopesMatch checks if two sets of scopes are compatible for binding resolution.
// This implements the core hygiene check using Flatt's "sets of scopes" model:
// A reference matches a binding if the binding's scope set is a SUBSET of the reference's scope set.
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

		// Structural sharing: if children are unchanged, return original pair.
		// Only symbols accumulate scopes, so most pairs pass this check.
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
// This is used by syntax-local-introduce to make introduced identifiers
// behave as if they came from the macro use site.
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
	newSctx := &SourceContext{
		Text:   sctx.Text,
		File:   sctx.File,
		Start:  sctx.Start,
		End:    sctx.End,
		Scopes: newScopes,
		Origin: sctx.Origin,
	}
	return &SyntaxSymbol{
		Sym: sym.Sym,
		syntaxBase: syntaxBase{
			sourceContext: newSctx,
		},
		ResolvedBinding: sym.ResolvedBinding,
	}
}

// AddScopeToSyntax adds a scope to a syntax object.
// Returns a new syntax object with the scope added.
// This is used by syntax-local-identifier-as-binding to mark identifiers
// as binding sites.
// Only symbols and pairs receive scopes; self-evaluating literals
// (SyntaxObject) are returned unchanged.
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
