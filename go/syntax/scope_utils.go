// Copyright 2025 Aaron Alpar
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

// ScopesMatch checks if two sets of scopes are compatible for binding resolution.
// This implements the core hygiene check using Flatt's "sets of scopes" model:
// A reference matches a binding if the binding's scope set is a SUBSET of the reference's scope set.
//
// This ensures:
// - Top-level bindings (empty scope set) match any reference: {} ⊆ X for all X
// - A macro-introduced binding only matches references with that macro's intro scope
// - User bindings don't capture macro-introduced identifiers (different scope sets)
func ScopesMatch(useScopes, bindingScopes []*Scope) bool {
	// A binding matches a use if all of the binding's scopes are present in the use's scopes.
	// This is the subset relationship: bindingScopes ⊆ useScopes
	//
	// Empty binding scopes (top-level) match everything since {} ⊆ X for all X.
	for _, bindScope := range bindingScopes {
		found := false
		for _, useScope := range useScopes {
			if bindScope == useScope {
				found = true
				break
			}
		}
		if !found {
			return false // Binding has a scope that use doesn't have
		}
	}
	return true
}

// HasScope checks if a scope set contains a specific scope
func HasScope(scopes []*Scope, target *Scope) bool {
	for _, s := range scopes {
		if s == target {
			return true
		}
	}
	return false
}

// AddScopeToSet adds a scope to a set if not already present
func AddScopeToSet(scopes []*Scope, newScope *Scope) []*Scope {
	if HasScope(scopes, newScope) {
		return scopes // Already has this scope
	}
	// Add the new scope
	result := make([]*Scope, len(scopes)+1)
	result[0] = newScope
	copy(result[1:], scopes)
	return result
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

// FlipScope toggles the presence of a scope on a syntax object.
// Returns a new syntax object with the scope flipped.
// This is used by syntax-local-introduce to make introduced identifiers
// behave as if they came from the macro use site.
func FlipScope(stx SyntaxValue, scope *Scope) SyntaxValue {
	if stx == nil || scope == nil {
		return stx
	}

	switch s := stx.(type) {
	case *SyntaxSymbol:
		return flipScopeOnSymbol(s, scope)
	case *SyntaxPair:
		return flipScopeOnPair(s, scope)
	case *SyntaxObject:
		return flipScopeOnObject(s, scope)
	default:
		return stx
	}
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
	}
	return NewSyntaxSymbol(sym.Key, newSctx)
}

// flipScopeOnPair recursively flips a scope on a SyntaxPair.
func flipScopeOnPair(pair *SyntaxPair, scope *Scope) *SyntaxPair {
	if pair == nil || IsSyntaxEmptyList(pair) {
		return pair
	}

	// Recursively flip on car
	var newCar SyntaxValue
	if car := pair.Car(); car != nil {
		if carStx, ok := car.(SyntaxValue); ok {
			newCar = FlipScope(carStx, scope)
		}
	}

	// Recursively flip on cdr
	var newCdr SyntaxValue
	if cdr := pair.Cdr(); cdr != nil {
		if cdrStx, ok := cdr.(SyntaxValue); ok {
			newCdr = FlipScope(cdrStx, scope)
		}
	}

	return NewSyntaxCons(newCar, newCdr, pair.SourceContext())
}

// flipScopeOnObject flips a scope on a SyntaxObject.
func flipScopeOnObject(obj *SyntaxObject, scope *Scope) *SyntaxObject {
	if obj == nil {
		return nil
	}
	sctx := obj.SourceContext()
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
	}
	return NewSyntaxObject(obj.Datum, newSctx)
}

// AddScopeToSyntax adds a scope to a syntax object.
// Returns a new syntax object with the scope added.
// This is used by syntax-local-identifier-as-binding to mark identifiers
// as binding sites.
func AddScopeToSyntax(stx SyntaxValue, scope *Scope) SyntaxValue {
	if stx == nil || scope == nil {
		return stx
	}

	switch s := stx.(type) {
	case *SyntaxSymbol:
		return s.AddScope(scope)
	case *SyntaxPair:
		return s.AddScope(scope)
	case *SyntaxObject:
		return s.AddScope(scope)
	default:
		return stx
	}
}
