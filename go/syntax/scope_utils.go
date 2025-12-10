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