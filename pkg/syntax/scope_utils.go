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
func ScopesMatch(useScopes, bindingScopes Scopes) bool {
	return values.ScopesMatch(useScopes, bindingScopes)
}

// ScopeFingerprint builds a deterministic map-key string from a scope set.
// See values.ScopeFingerprint for the full documentation.
func ScopeFingerprint(scopes Scopes) string {
	return values.ScopeFingerprint(scopes)
}

// ScopesCompatible checks whether a binding's scopes can match a reference's.
// A binding with no scopes matches any reference.
func ScopesCompatible(bindingScopes, useScopes Scopes) bool {
	return values.ScopesCompatible(bindingScopes, useScopes)
}

// ScopeSet is a hygiene query constraint (all / empty / specific). Defined in
// package values alongside the Scope type; re-exported here so environment and
// compiler code can spell it syntax.ScopeSet beside syntax.ScopesCompatible.
type ScopeSet = values.ScopeSet

// Scopes is an immutable set of *Scope, the binder-identity side of hygiene.
// Defined in package values with the Scope type and re-exported here for the
// same reason ScopeSet is.
//
// An ALIAS, necessarily, not a defined type: `type Scopes values.Scopes` would
// be a second named type with the same underlying struct, which is NOT
// assignable to the first, and every forwarder in this file would need an
// explicit conversion.
type Scopes = values.Scopes

// ScopesFromSlice builds a Scopes from a slice, discarding duplicates. The
// boundary constructor; see values.ScopesFromSlice.
func ScopesFromSlice(scopes []*Scope) Scopes {
	return values.ScopesFromSlice(scopes)
}

// AllScopes returns the wildcard scope-set query. See values.AllScopes.
func AllScopes() ScopeSet {
	return values.AllScopes()
}

// ScopesOf returns a query constrained to the given scope set (the zero value is
// the empty set, not the wildcard). See values.ScopesOf.
func ScopesOf(scopes Scopes) ScopeSet {
	return values.ScopesOf(scopes)
}

// EmptyScopes returns the ambient (empty) scope-set query. See values.EmptyScopes.
func EmptyScopes() ScopeSet {
	return values.EmptyScopes()
}

// FlipScopeInSet toggles the presence of a scope in a set.
// It is the set-level half of FlipScope; see FlipScope.
func FlipScopeInSet(scopes Scopes, target *Scope) Scopes {
	return values.FlipScopeInSet(scopes, target)
}

// MapSyntaxTree recursively transforms a syntax tree.
// The function fn is applied to each node bottom-up: children are transformed first,
// then the parent is constructed with the transformed children.
//
// For pairs, this means:
// 1. Recursively transform car and cdr
// 2. Create new pair with transformed children
// 3. fn is NOT called on the pair itself (only on leaf nodes like symbols)
//
// This is the shared traversal logic used by AddScope and FlipScope. Exported for
// compilation.CompileQuoteSyntax, which stamps pins on a template's identifiers.
//
// It recurses into pairs, vectors and boxes, so a box's content is reached by
// every scope operation built on it: FlipScope has to take off whatever
// AddScope put into a box, or the two are not duals.
//
// Syntax can be a graph: a datum label reached past its wrapper (`#1#` in
// `#0=(#1=(x . #0#))`) is a raw cyclic pair. Once a walk is deeper than
// treeMapMemoDepth, each compound is visited once, so a cycle comes back as a
// cycle of new nodes (unrolled down to where the memo began) instead of
// overflowing the Go stack. That holds even when nothing in the cycle changes:
// unchanged cyclic data comes back as a copy, not the input (an unchanged
// acyclic subtree hanging off it is still returned by identity). A depth bound
// alone would refuse legal deep data, and there is no error return to refuse
// through.
//
// Sharing is not preserved. A changed subterm reached twice is rebuilt twice in
// a shallow walk and once past treeMapMemoDepth, so whether two references in
// the result are the same node depends on depth.
func MapSyntaxTree(stx SyntaxValue, fn func(SyntaxValue) SyntaxValue) SyntaxValue {
	p := &treeMap{fn: fn}
	return p.walk(stx)
}

// treeMapMemoDepth is the walk depth (pairs, vectors and boxes, cdrs included)
// past which MapSyntaxTree starts memoising compounds. Every cycle gets past it;
// most macro input and output does not. Measured on --check of expander-heavy
// files: an eager memo (one map per walk) cost 14% CPU on 1500 small defines,
// where this one is within noise; on a 300-form body with 150-element macro
// operands, where walks do get deep, this one costs about 6%.
const treeMapMemoDepth = 64

// treeMap is one MapSyntaxTree walk.
//
// done maps each finished compound to its result. A compound on the current
// path maps to nil; meeting it again closes a cycle, and the walk hands out a
// placeholder for it (back) and fills that placeholder in once the node's
// children are known. A revisited node is rebuilt this way whether or not its
// children change, which is why an unchanged cycle comes back as a copy. An
// acyclic walk never allocates a placeholder, and a walk that stays shallower
// than treeMapMemoDepth allocates neither map.
type treeMap struct {
	fn    func(SyntaxValue) SyntaxValue
	depth int
	done  map[SyntaxValue]SyntaxValue
	back  map[SyntaxValue]SyntaxValue
}

// deep reports whether the walk memoises: once it is past treeMapMemoDepth,
// and from then on. A walk that never gets deep pays this test per compound.
func (p *treeMap) deep() bool {
	return p.done != nil || p.depth > treeMapMemoDepth
}

// enter reports a compound's memoised result, or registers it as in progress.
// ok is true when the caller must return q instead of walking stx.
func (p *treeMap) enter(stx SyntaxValue, placeholder func() SyntaxValue) (q SyntaxValue, ok bool) {
	if p.done == nil {
		p.done = make(map[SyntaxValue]SyntaxValue)
	}
	q, seen := p.done[stx]
	if !seen {
		p.done[stx] = nil
		return nil, false
	}
	if q != nil {
		return q, true
	}
	q = p.back[stx]
	if q != nil {
		return q, true
	}
	if p.back == nil {
		p.back = make(map[SyntaxValue]SyntaxValue)
	}
	q = placeholder()
	p.back[stx] = q
	return q, true
}

// leave records q as stx's result and pops one level of depth.
func (p *treeMap) leave(stx, q SyntaxValue) SyntaxValue {
	p.depth--
	if p.done != nil {
		p.done[stx] = q
	}
	return q
}

// pending returns the placeholder handed out for stx, or nil.
func (p *treeMap) pending(stx SyntaxValue) SyntaxValue {
	if p.back == nil {
		return nil
	}
	return p.back[stx]
}

func (p *treeMap) walk(stx SyntaxValue) SyntaxValue {
	if stx == nil {
		return nil
	}

	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return s
		}
		p.depth++
		if p.deep() {
			q, ok := p.enter(s, func() SyntaxValue {
				return NewSyntaxCons(nil, nil, s.SourceContext())
			})
			if ok {
				p.depth--
				return q
			}
		}

		// Recursively transform car and cdr
		var newCar, newCdr SyntaxValue
		if s.Values[0] != nil {
			newCar = p.walk(s.Values[0])
		}
		if s.Values[1] != nil {
			newCdr = p.walk(s.Values[1])
		}

		// Structural sharing: when a tree transformation leaves children unchanged,
		// the original node is returned instead of allocating a new one. This is
		// the core idea behind persistent data structures (Okasaki, 1998). Most
		// syntax tree nodes are not symbols, so most pairs pass the identity check.
		// See BIBLIOGRAPHY.md "Structural Sharing". A placeholder handed out for
		// this node means a child closed a cycle through it; the placeholder is
		// the result even if nothing in the cycle changed.
		var q SyntaxValue
		ph, _ := p.pending(s).(*SyntaxPair)
		switch {
		case ph != nil:
			ph.Values = [2]SyntaxValue{newCar, newCdr}
			q = ph
		case newCar == s.Values[0] && newCdr == s.Values[1]:
			q = s
		default:
			q = NewSyntaxCons(newCar, newCdr, s.SourceContext())
		}
		return p.leave(s, q)

	case *SyntaxSymbol:
		// Apply transformation to symbols
		return p.fn(s)

	case *SyntaxVector:
		if s == nil || len(s.Values) == 0 {
			return s
		}
		p.depth++
		if p.deep() {
			q, ok := p.enter(s, func() SyntaxValue {
				return NewSyntaxVector(s.SourceContext())
			})
			if ok {
				p.depth--
				return q
			}
		}

		// Recursively transform each element
		newValues := make([]SyntaxValue, len(s.Values))
		changed := false
		for i, elem := range s.Values {
			if elem != nil {
				newValues[i] = p.walk(elem)
				if newValues[i] != elem {
					changed = true
				}
			} else {
				newValues[i] = nil
			}
		}

		var q SyntaxValue
		ph, _ := p.pending(s).(*SyntaxVector)
		switch {
		case ph != nil:
			ph.Values = newValues
			q = ph
		case !changed:
			q = s
		default:
			q = NewSyntaxVector(s.SourceContext(), newValues...)
		}
		return p.leave(s, q)

	case *SyntaxBox:
		if s == nil || s.Value == nil {
			return s
		}
		p.depth++
		if p.deep() {
			q, ok := p.enter(s, func() SyntaxValue {
				return NewSyntaxBox(nil, s.SourceContext())
			})
			if ok {
				p.depth--
				return q
			}
		}
		inner := p.walk(s.Value)
		var q SyntaxValue
		ph, _ := p.pending(s).(*SyntaxBox)
		switch {
		case ph != nil:
			ph.Value = inner
			q = ph
		case inner == s.Value:
			q = s
		default:
			q = NewSyntaxBox(inner, s.SourceContext())
		}
		return p.leave(s, q)

	default:
		// Other types (SyntaxObject, etc.) - check if they support the transformation
		// This handles types that might implement the transformation interface
		return p.fn(stx)
	}
}

// FlipScope toggles the presence of a scope on a syntax object.
// Returns a new syntax object with the scope flipped.
//
// Two production callers: the kernel flips each closure-transformer invocation's
// introduction scope on the transformer's output (expandMacroInvocation and
// ExpandOnce), which is Flatt's introduction step, and syntax-local-introduce
// flips that same scope on a syntax object the transformer chooses.
func FlipScope(stx SyntaxValue, scope *Scope) SyntaxValue {
	if stx == nil || scope == nil {
		return stx
	}

	return MapSyntaxTree(stx, func(node SyntaxValue) SyntaxValue {
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
// Symbols and the symbols inside pairs, vectors and boxes receive the scope;
// self-evaluating literals
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
	case *SyntaxBox:
		return s.AddScope(scope)
	default:
		// SyntaxObject and other types don't need scopes
		return stx
	}
}
