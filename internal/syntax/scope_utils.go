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
	default:
		// SyntaxObject and other types don't store meaningful scopes
		return stx
	}
}

// flipScopeOnPair recursively flips a scope on a SyntaxPair.
func flipScopeOnPair(pair *SyntaxPair, scope *Scope) *SyntaxPair {
	if pair == nil || IsSyntaxEmptyList(pair) {
		return pair
	}

	// Recursively flip on car
	var newCar SyntaxValue
	car := pair.Car()
	if car != nil {
		if carStx, ok := car.(SyntaxValue); ok {
			newCar = FlipScope(carStx, scope)
		}
	}

	// Recursively flip on cdr
	var newCdr SyntaxValue
	cdr := pair.Cdr()
	if cdr != nil {
		if cdrStx, ok := cdr.(SyntaxValue); ok {
			newCdr = FlipScope(cdrStx, scope)
		}
	}

	return NewSyntaxCons(newCar, newCdr, pair.SourceContext())
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
		Sym:             sym.Sym,
		sourceContext:   newSctx,
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
	default:
		// SyntaxObject and other types don't need scopes
		return stx
	}
}
