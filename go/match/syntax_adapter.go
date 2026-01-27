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

package match

// syntax_adapter.go bridges between syntax objects (with hygiene info) and
// the unhygienic pattern matching VM.
//
// Design: The macro system uses a layered architecture (see go/DESIGN.md):
//   Layer 1: Pattern Matching VM - operates on raw values.Value types
//   Layer 2: Syntax Adapter (this file) - converts syntax ↔ values
//   Layer 3: Hygienic Layer - adds/checks scopes during expansion
//
// This file implements Layer 2. The SyntaxMatcher wraps the core Matcher,
// converting syntax objects to raw values for matching, and converting
// expanded results back to syntax objects (preserving source context).
//
// Key functions:
//   - syntaxToValue: Unwraps syntax objects to raw values for pattern matching
//   - valueToSyntax: Wraps raw values back into syntax objects for the result
//
// The hygiene layer (Layer 3) operates after expansion by adding intro scopes
// to the syntax objects returned by valueToSyntax.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"context"
	"errors"
	"reflect"

	"wile/syntax"
	"wile/values"
)

// localScopesProvider is an interface for getting local scopes from a free ID resolution.
// Implemented by machine.FreeIdResolution to avoid circular imports.
type localScopesProvider interface {
	GetLocalScopes() []*syntax.Scope
}

// globalBindingProvider is an interface for getting global bindings from a free ID resolution.
// Implemented by machine.FreeIdResolution to avoid circular imports.
type globalBindingProvider interface {
	GetGlobal() any
}

// hasLocalBindingProvider is an interface for checking if a local binding was found
// at macro definition time, even if the binding has no scopes. This distinguishes
// "local binding with empty scopes" from "no binding at all" - the former should NOT
// get intro scope added during expansion.
type hasLocalBindingProvider interface {
	GetHasLocalBinding() bool
}

// BindingChecker is an interface for checking if a symbol has a lexical binding.
// This is used for R7RS auxiliary syntax hygiene: literals like => and else
// should not match when the identifier has been locally bound.
// Implemented by machine package to avoid circular imports.
type BindingChecker interface {
	// HasBinding checks if sym with the given scopes has a lexical binding.
	// Returns true if the symbol is bound (to a variable, macro, etc.).
	HasBinding(sym string, scopes []*syntax.Scope) bool

	// GetBinding returns the binding for sym with the given scopes.
	// Returns nil if no binding exists. The returned value is opaque but
	// can be compared for equality to check if two identifiers have the
	// same binding (per R7RS §4.3.2).
	GetBinding(sym string, scopes []*syntax.Scope) any
}

// SyntaxMatcher adapts the unhygienic Matcher to work with syntax objects.
//
// It provides the bridge between:
//   - Syntax objects (SyntaxPair, SyntaxSymbol) with source locations and scopes
//   - Raw values (Pair, Sym) that the pattern matching VM operates on
//
// The separation allows the pattern matcher to be simple and efficient,
// while syntax/scope handling is done in the adapter layer.
//
// Key feature: The syntaxMap preserves original syntax objects for captured
// pattern variables. This is essential for hygiene - content captured from
// the input must retain its original scopes, not receive new ones.
//
// Literal Hygiene: The literalSyntax map stores pattern literals with their
// scopes. During matching, if an input symbol has a literal's name but
// incompatible scopes (e.g., shadowed by let), it won't match the literal.
// This implements R7RS's requirement that auxiliary syntax like => and else
// be treated as regular expressions when locally shadowed.
//
// R7RS Binding Check: For full R7RS compliance (§4.3.2), we also check if
// the input identifier has a lexical binding. If it does and the pattern
// literal doesn't, they don't match. This is handled via the bindingChecker
// field set during Match().
type SyntaxMatcher struct {
	matcher        *Matcher
	syntaxMap      map[values.Value]syntax.SyntaxValue // Maps raw values to their original syntax
	ellipsisID     string                              // Custom ellipsis identifier (default "...")
	literalSyntax  map[string]*syntax.SyntaxSymbol     // Pattern literals with their scopes for hygiene
	bindingChecker BindingChecker                      // For R7RS binding lookup during matching
}

// NewSyntaxMatcher creates a new syntax-aware matcher with default ellipsis ("...").
func NewSyntaxMatcher(variables map[string]struct{}, codes []SyntaxCommand) *SyntaxMatcher {
	return NewSyntaxMatcherWithEllipsisVars(variables, codes, nil)
}

// NewSyntaxMatcherWithEllipsisVars creates a syntax-aware matcher with ellipsis variable mapping.
// The ellipsisVars parameter maps each ellipsis ID to its captured pattern variables.
// Uses the default ellipsis identifier ("...").
func NewSyntaxMatcherWithEllipsisVars(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}) *SyntaxMatcher {
	return NewSyntaxMatcherFull(variables, codes, ellipsisVars, DefaultEllipsis)
}

// NewSyntaxMatcherFull creates a syntax-aware matcher with all parameters including custom ellipsis.
// The ellipsisID parameter specifies the identifier used for ellipsis patterns
// (default is "..." per R7RS, but can be customized per R7RS §4.3.2).
func NewSyntaxMatcherFull(variables map[string]struct{}, codes []SyntaxCommand, ellipsisVars map[int]map[string]struct{}, ellipsisID string) *SyntaxMatcher {
	return NewSyntaxMatcherWithLiterals(variables, codes, ellipsisVars, ellipsisID, nil)
}

// NewSyntaxMatcherWithLiterals creates a syntax-aware matcher with literal syntax for hygiene.
// The literalSyntax parameter maps literal names to their syntax symbols from the pattern.
// This enables scope-aware literal matching: if an input symbol has a literal's name but
// has been shadowed (has additional scopes), it won't match the pattern literal.
// R7RS §4.3.2 requires this for auxiliary syntax like => and else in cond/case.
func NewSyntaxMatcherWithLiterals(
	variables map[string]struct{},
	codes []SyntaxCommand,
	ellipsisVars map[int]map[string]struct{},
	ellipsisID string,
	literalSyntax map[string]*syntax.SyntaxSymbol,
) *SyntaxMatcher {
	if ellipsisID == "" {
		ellipsisID = DefaultEllipsis
	}
	return &SyntaxMatcher{
		matcher:       NewMatcherFull(variables, codes, ellipsisVars, ellipsisID),
		syntaxMap:     make(map[values.Value]syntax.SyntaxValue),
		ellipsisID:    ellipsisID,
		literalSyntax: literalSyntax,
	}
}

// Match performs pattern matching on syntax objects.
// This is the basic method without binding checking. For full R7RS compliance
// with auxiliary syntax hygiene, use MatchWithBindingChecker instead.
func (sm *SyntaxMatcher) Match(input syntax.SyntaxValue) error {
	return sm.MatchWithBindingChecker(input, nil)
}

// MatchWithBindingChecker performs pattern matching on syntax objects with
// R7RS-compliant auxiliary syntax hygiene.
//
// The checker parameter enables R7RS §4.3.2 compliant literal matching:
// literals match only if both identifiers have the same lexical binding,
// or both have no lexical binding. If the input has a binding (from let,
// lambda, etc.) but the pattern literal doesn't, they won't match.
//
// Pass nil for checker to use scope-based matching only (less strict).
func (sm *SyntaxMatcher) MatchWithBindingChecker(input syntax.SyntaxValue, checker BindingChecker) error {
	// Clear the syntax map for this match
	sm.syntaxMap = make(map[values.Value]syntax.SyntaxValue)

	// Store binding checker for use in syntaxToValueWithMap
	sm.bindingChecker = checker

	// Convert syntax to raw values for matching, building the syntax map
	rawInput := sm.syntaxToValueWithMap(input)

	// Clear binding checker after use
	defer func() { sm.bindingChecker = nil }()

	// Ensure it's a pair as the matcher expects
	pair, ok := rawInput.(*values.Pair)
	if !ok {
		return errors.New("pattern matching requires a pair")
	}

	return sm.matcher.Match(pair)
}

// Expand performs template expansion, preserving syntax wrappers
func (sm *SyntaxMatcher) Expand(template syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return sm.ExpandWithIntroScope(template, nil, nil)
}

// ExpandWithIntroScope performs template expansion with hygiene support.
// The introScope is added to newly created syntax objects (from the template),
// but NOT to syntax objects preserved from pattern variable substitution.
// The freeIds map contains identifiers that should not receive the intro scope.
// Values in the map are pre-resolved bindings (nil means just skip intro scope).
func (sm *SyntaxMatcher) ExpandWithIntroScope(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any) (syntax.SyntaxValue, error) {
	return sm.ExpandWithUseSite(template, introScope, freeIds, nil)
}

// ExpandWithUseSite performs template expansion with hygiene support and use-site tracking.
// The introScope is added to newly created syntax objects (from the template),
// but NOT to syntax objects preserved from pattern variable substitution.
// The freeIds map contains identifiers that should not receive the intro scope.
// Values in the map are pre-resolved bindings (nil means just skip intro scope).
// The useSiteCtx, if provided, is used for the source context of newly created syntax
// objects instead of the template's context. This allows error messages to point to
// where the macro was invoked rather than where it was defined.
func (sm *SyntaxMatcher) ExpandWithUseSite(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any, useSiteCtx *syntax.SourceContext) (syntax.SyntaxValue, error) {
	return sm.ExpandWithOrigin(template, introScope, freeIds, useSiteCtx, nil)
}

// ExpandWithOrigin performs template expansion with full hygiene and origin tracking.
// Parameters:
//   - template: The template to expand
//   - introScope: Hygiene scope added to newly created syntax (not pattern variables)
//   - freeIds: Map of free identifier names to their pre-resolved bindings (any type to avoid circular imports)
//   - useSiteCtx: Source context for newly created syntax (use-site vs template-site)
//   - origin: Origin info for tracking macro expansion chains
func (sm *SyntaxMatcher) ExpandWithOrigin(template syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any, useSiteCtx *syntax.SourceContext, origin *syntax.OriginInfo) (syntax.SyntaxValue, error) {
	// Convert template to raw values
	rawTemplate := syntaxToValue(template)

	// Perform unhygienic expansion
	expanded, err := sm.matcher.Expand(rawTemplate)
	if err != nil {
		return nil, err
	}

	// Wrap result back in syntax, using the syntax map to preserve
	// original syntax objects from captured pattern variables.
	// The intro scope is only added to newly created syntax objects.
	// Use use-site context if provided, otherwise fall back to template context.
	return sm.valueToSyntaxWithOrigin(expanded, template, introScope, freeIds, useSiteCtx, origin), nil
}

// ExpandWithPatternVarSyntax performs template expansion with full nested macro hygiene.
// This is the scope-aware expansion that correctly handles the case where an outer macro
// introduces a symbol into an inner macro's template. The patternVarSyntax map contains
// the syntax symbols from the pattern, allowing scope comparison during substitution.
//
// Per Flatt 2016 "sets of scopes" model: when deciding whether to substitute a template
// symbol with a captured value, we compare the template symbol's scopes with the pattern
// variable's scopes. Only substitute if the scopes are compatible (pattern var scopes ⊆
// template symbol scopes). If the template symbol has additional scopes (e.g., from an
// outer macro's intro scope), it should NOT be substituted.
//
// Example: When outer macro `foo` expands `(foo bar x)` producing:
//
//	(define-syntax bar (syntax-rules () ((bar x) 'x)))
//
// The pattern's `x` has scopes from the outer expansion (S_outer), while the template's
// `'x` was substituted from the input and has use-site scopes. When `bar` is compiled,
// both `x` symbols have different scopes, so template `'x` should NOT be substituted
// when inner macro expands.
func (sm *SyntaxMatcher) ExpandWithPatternVarSyntax(
	template syntax.SyntaxValue,
	introScope *syntax.Scope,
	freeIds map[string]any,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
	patternVarSyntax map[string]*syntax.SyntaxSymbol,
) (syntax.SyntaxValue, error) {
	if len(sm.matcher.captureStack) == 0 {
		return nil, errors.New("no capture context for expansion")
	}

	// Perform syntax-preserving expansion with scope comparison
	return sm.expandSyntaxValue(
		template,
		sm.matcher.captureStack[0],
		nil, // ellipsisVars
		introScope,
		freeIds,
		useSiteCtx,
		origin,
		patternVarSyntax,
	)
}

// expandSyntaxValue recursively expands a syntax template with captured bindings,
// preserving scope information and using scope comparison for pattern variable substitution.
// This is the syntax-level expansion that correctly handles nested macro hygiene.
func (sm *SyntaxMatcher) expandSyntaxValue(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	introScope *syntax.Scope,
	freeIds map[string]any,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
	patternVarSyntax map[string]*syntax.SyntaxSymbol,
) (syntax.SyntaxValue, error) {
	if template == nil {
		return nil, nil
	}

	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		symVal := t.Unwrap().(*values.Symbol)

		// Check if it's a pattern variable by name
		if capturedVal, ok := ctx.bindings[symVal.Key]; ok {
			// Check scope compatibility before substituting
			// R7RS nested macro hygiene: only substitute if template symbol's scopes
			// are compatible with pattern variable's scopes
			if patternVarSyntax != nil {
				if patternSym, hasPattern := patternVarSyntax[symVal.Key]; hasPattern {
					templateScopes := t.Scopes()
					patternScopes := patternSym.Scopes()

					// For substitution to occur, pattern var scopes must be subset of template scopes
					// AND template scopes must be subset of pattern scopes (i.e., scope equality)
					// This ensures symbols introduced by outer macros are not captured by inner patterns
					if !scopesCompatibleForSubstitution(templateScopes, patternScopes) {
						// Scopes don't match - keep template symbol as literal (hygiene!)
						// Apply intro scope and free ID handling as normal
						return sm.applyHygieneToSymbol(t, introScope, freeIds, useSiteCtx, origin), nil
					}
				}
			}
			// Scopes match (or no pattern var syntax) - substitute with captured value
			return sm.capturedValueToSyntax(capturedVal, introScope, useSiteCtx, origin)
		}

		// Not a pattern variable - apply hygiene as normal
		return sm.applyHygieneToSymbol(t, introScope, freeIds, useSiteCtx, origin), nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return t, nil
		}

		// Check for ellipsis escape form: (<ellipsis> <template>)
		car := t.SyntaxCar()
		if carSym, ok := car.(*syntax.SyntaxSymbol); ok {
			if carSym.Unwrap().(*values.Symbol).Key == sm.ellipsisID {
				cdr := t.SyntaxCdr()
				if cdrPair, ok := cdr.(*syntax.SyntaxPair); ok && !syntax.IsSyntaxEmptyList(cdrPair) {
					// Escape form - expand inner template without ellipsis handling
					return sm.expandEscapedSyntaxTemplate(
						cdrPair.SyntaxCar(),
						ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
				}
			}
		}

		// Check for ellipsis pattern (something <ellipsis>)
		cdr := t.SyntaxCdr()
		if cdrPair, ok := cdr.(*syntax.SyntaxPair); ok && !syntax.IsSyntaxEmptyList(cdrPair) {
			if sym, ok := cdrPair.SyntaxCar().(*syntax.SyntaxSymbol); ok {
				if sym.Unwrap().(*values.Symbol).Key == sm.ellipsisID {
					// Found ellipsis - handle repetition
					return sm.expandSyntaxEllipsis(
						car, cdrPair.SyntaxCdr(),
						ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
				}
			}
		}

		// Regular pair - expand car and cdr
		expandedCar, err := sm.expandSyntaxValue(car, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
		if err != nil {
			return nil, err
		}
		expandedCdr, err := sm.expandSyntaxValue(cdr, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
		if err != nil {
			return nil, err
		}

		srcCtx := t.SourceContext()
		if useSiteCtx != nil {
			srcCtx = useSiteCtx
		}
		return syntax.NewSyntaxCons(expandedCar, expandedCdr, srcCtx), nil

	case *syntax.SyntaxVector:
		// Expand each element
		expandedElements := make([]syntax.SyntaxValue, len(t.Values))
		for i, elem := range t.Values {
			expanded, err := sm.expandSyntaxValue(elem, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
			if err != nil {
				return nil, err
			}
			expandedElements[i] = expanded
		}
		srcCtx := t.SourceContext()
		if useSiteCtx != nil {
			srcCtx = useSiteCtx
		}
		return syntax.NewSyntaxVector(srcCtx, expandedElements...), nil

	default:
		// Self-evaluating values - return as-is
		return template, nil
	}
}

// scopesCompatibleForSubstitution checks if template symbol scopes are compatible with
// pattern variable scopes for substitution. Returns true if substitution should occur.
//
// For nested macro hygiene, we require bidirectional scope matching (set equality):
// patternScopes ⊆ templateScopes AND templateScopes ⊆ patternScopes
//
// This prevents outer macro-introduced symbols from being captured by inner pattern variables.
func scopesCompatibleForSubstitution(templateScopes, patternScopes []*syntax.Scope) bool {
	return syntax.ScopesMatch(templateScopes, patternScopes) &&
		syntax.ScopesMatch(patternScopes, templateScopes)
}

// applyHygieneToSymbol applies hygiene transformations to a template symbol.
// This handles free identifiers and intro scope for non-pattern-variable symbols.
func (sm *SyntaxMatcher) applyHygieneToSymbol(
	sym *syntax.SyntaxSymbol,
	introScope *syntax.Scope,
	freeIds map[string]any,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
) syntax.SyntaxValue {
	symVal := sym.Unwrap().(*values.Symbol)

	// Determine source context
	srcCtx := sym.SourceContext()
	if useSiteCtx != nil {
		srcCtx = useSiteCtx
	}
	if origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(origin)
	} else if origin != nil {
		srcCtx = &syntax.SourceContext{Origin: origin}
	}

	// Check if this is a free identifier
	var isFree bool
	var resolution any
	if freeIds != nil {
		resolution, isFree = freeIds[symVal.Key]
	}

	if isFree && resolution != nil {
		// Handle free identifier resolution (local or global binding)
		if lsp, ok := resolution.(localScopesProvider); ok {
			localScopes := lsp.GetLocalScopes()
			if len(localScopes) > 0 {
				// Local binding - use definition-site scopes
				scopedCtx := &syntax.SourceContext{
					Text:   srcCtx.Text,
					File:   srcCtx.File,
					Start:  srcCtx.Start,
					End:    srcCtx.End,
					Origin: srcCtx.Origin,
					Scopes: localScopes,
				}
				return syntax.NewSyntaxSymbol(symVal.Key, scopedCtx)
			}
		}

		if gbp, ok := resolution.(globalBindingProvider); ok {
			globalBinding := gbp.GetGlobal()
			if globalBinding != nil {
				symCtx := srcCtx
				if srcCtx != nil && len(srcCtx.Scopes) > 0 {
					symCtx = &syntax.SourceContext{
						Text:   srcCtx.Text,
						File:   srcCtx.File,
						Start:  srcCtx.Start,
						End:    srcCtx.End,
						Origin: srcCtx.Origin,
					}
				}
				newSym := syntax.NewSyntaxSymbol(symVal.Key, symCtx)
				return newSym.WithResolvedBinding(globalBinding)
			}
		}

		if hlp, ok := resolution.(hasLocalBindingProvider); ok && hlp.GetHasLocalBinding() {
			return syntax.NewSyntaxSymbol(symVal.Key, srcCtx)
		}
	}

	// Not a free identifier or unresolved - create symbol with intro scope
	templateCtx := srcCtx
	if srcCtx != nil && len(srcCtx.Scopes) > 0 {
		templateCtx = srcCtx.WithoutScopes()
	}
	newSym := syntax.NewSyntaxSymbol(symVal.Key, templateCtx)
	if introScope != nil {
		newSym = newSym.AddScope(introScope).(*syntax.SyntaxSymbol)
	}
	return newSym
}

// capturedValueToSyntax converts a captured value back to syntax.
// Captured values from pattern variable substitution preserve their original scopes.
func (sm *SyntaxMatcher) capturedValueToSyntax(
	val values.Value,
	introScope *syntax.Scope,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
) (syntax.SyntaxValue, error) {
	// Check if this value has a corresponding original syntax object in syntaxMap
	if origStx, ok := sm.syntaxMap[val]; ok {
		return origStx, nil
	}

	// Otherwise, wrap the value in syntax (for values not from input)
	srcCtx := useSiteCtx
	if origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(origin)
	} else if origin != nil {
		srcCtx = &syntax.SourceContext{Origin: origin}
	}

	switch v := val.(type) {
	case *values.Pair:
		if values.IsEmptyList(v) {
			return syntax.NewSyntaxEmptyList(srcCtx), nil
		}
		car, err := sm.capturedValueToSyntax(v[0], introScope, useSiteCtx, origin)
		if err != nil {
			return nil, err
		}
		cdr, err := sm.capturedValueToSyntax(v[1], introScope, useSiteCtx, origin)
		if err != nil {
			return nil, err
		}
		return syntax.NewSyntaxCons(car, cdr, srcCtx), nil

	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, srcCtx), nil

	default:
		return syntax.NewSyntaxObject(val, srcCtx), nil
	}
}

// expandSyntaxEllipsis handles template repetition with ellipsis at the syntax level.
func (sm *SyntaxMatcher) expandSyntaxEllipsis(
	pattern syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	introScope *syntax.Scope,
	freeIds map[string]any,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
	patternVarSyntax map[string]*syntax.SyntaxSymbol,
) (syntax.SyntaxValue, error) {
	// Find which variables in the pattern are bound in child contexts
	patternVarsInTemplate := sm.findSyntaxPatternVariables(pattern)

	// Find the ellipsis ID that captured these variables
	ellipsisID := sm.matcher.findMatchingEllipsisID(patternVarsInTemplate)
	if ellipsisID < 0 {
		// No matching ellipsis - just expand the rest
		return sm.expandSyntaxValue(rest, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
	}

	// Get children for this specific ellipsis ID
	children := ctx.children[ellipsisID]
	if len(children) == 0 {
		// No repetitions captured, just expand the rest
		return sm.expandSyntaxValue(rest, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
	}

	// Build result by repeating pattern for each child context
	var results []syntax.SyntaxValue
	for _, childCtx := range children {
		// Create a new ellipsis variable set for this expansion
		newEllipsisVars := make(map[string]struct{})
		for k, v := range ellipsisVars {
			newEllipsisVars[k] = v
		}
		for v := range patternVarsInTemplate {
			newEllipsisVars[v] = struct{}{}
		}

		expanded, err := sm.expandSyntaxValue(pattern, childCtx, newEllipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
		if err != nil {
			return nil, err
		}
		results = append(results, expanded)
	}

	// Expand the rest
	expandedRest, err := sm.expandSyntaxValue(rest, ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
	if err != nil {
		return nil, err
	}

	// Combine results into a list and append the rest
	srcCtx := useSiteCtx
	if srcCtx == nil && pattern != nil {
		srcCtx = pattern.SourceContext()
	}
	result := expandedRest
	for i := len(results) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(results[i], result, srcCtx)
	}

	return result, nil
}

// findSyntaxPatternVariables finds all pattern variables in a syntax template.
func (sm *SyntaxMatcher) findSyntaxPatternVariables(template syntax.SyntaxValue) map[string]struct{} {
	vars := make(map[string]struct{})
	sm.findSyntaxVarsRecursive(template, vars)
	return vars
}

func (sm *SyntaxMatcher) findSyntaxVarsRecursive(template syntax.SyntaxValue, vars map[string]struct{}) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		symVal := t.Unwrap().(*values.Symbol)
		if _, ok := sm.matcher.variables[symVal.Key]; ok {
			vars[symVal.Key] = struct{}{}
		}
	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(t) {
			sm.findSyntaxVarsRecursive(t.SyntaxCar(), vars)
			sm.findSyntaxVarsRecursive(t.SyntaxCdr(), vars)
		}
	}
}

// expandEscapedSyntaxTemplate expands a template inside an ellipsis escape form at the syntax level.
func (sm *SyntaxMatcher) expandEscapedSyntaxTemplate(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	introScope *syntax.Scope,
	freeIds map[string]any,
	useSiteCtx *syntax.SourceContext,
	origin *syntax.OriginInfo,
	patternVarSyntax map[string]*syntax.SyntaxSymbol,
) (syntax.SyntaxValue, error) {
	if template == nil {
		return nil, nil
	}

	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		symVal := t.Unwrap().(*values.Symbol)

		// Check if it's a pattern variable by name
		if capturedVal, ok := ctx.bindings[symVal.Key]; ok {
			// Check scope compatibility before substituting
			if patternVarSyntax != nil {
				if patternSym, hasPattern := patternVarSyntax[symVal.Key]; hasPattern {
					templateScopes := t.Scopes()
					patternScopes := patternSym.Scopes()

					if !scopesCompatibleForSubstitution(templateScopes, patternScopes) {
						return sm.applyHygieneToSymbol(t, introScope, freeIds, useSiteCtx, origin), nil
					}
				}
			}
			return sm.capturedValueToSyntax(capturedVal, introScope, useSiteCtx, origin)
		}

		return sm.applyHygieneToSymbol(t, introScope, freeIds, useSiteCtx, origin), nil

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return t, nil
		}
		// In escaped context, don't check for ellipsis patterns
		car, err := sm.expandEscapedSyntaxTemplate(t.SyntaxCar(), ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
		if err != nil {
			return nil, err
		}
		cdr, err := sm.expandEscapedSyntaxTemplate(t.SyntaxCdr(), ctx, ellipsisVars, introScope, freeIds, useSiteCtx, origin, patternVarSyntax)
		if err != nil {
			return nil, err
		}
		srcCtx := t.SourceContext()
		if useSiteCtx != nil {
			srcCtx = useSiteCtx
		}
		return syntax.NewSyntaxCons(car, cdr, srcCtx), nil

	default:
		return template, nil
	}
}

// syntaxToValueWithMap recursively unwraps syntax objects to raw values,
// building a map from raw values back to their original syntax objects.
// This preserves the connection needed for hygiene during expansion.
func (sm *SyntaxMatcher) syntaxToValueWithMap(stx syntax.SyntaxValue) values.Value {
	if stx == nil {
		return nil
	}

	var result values.Value

	switch s := stx.(type) {
	case *syntax.SyntaxPair:
		if s == nil || syntax.IsSyntaxEmptyList(s) {
			result = values.EmptyList
		} else {
			// Recursively unwrap car and cdr, building the map
			var car values.Value
			carVal := s.SyntaxCar()
			if carVal != nil {
				carSyntax := carVal
				car = sm.syntaxToValueWithMap(carSyntax)
			}

			var cdr values.Value
			cdrVal := s.SyntaxCdr()
			if cdrVal != nil {
				cdrSyntax := cdrVal
				cdr = sm.syntaxToValueWithMap(cdrSyntax)
			}

			// Build the result pair
			if cdr == nil || values.IsEmptyList(cdr) {
				result = values.NewCons(car, values.EmptyList)
			} else {
				cdrPair, ok := cdr.(*values.Pair)
				if ok {
					result = values.NewCons(car, cdrPair)
				} else {
					result = values.NewCons(car, cdr)
				}
			}
		}

	case *syntax.SyntaxSymbol:
		// Create a FRESH symbol instance instead of using the globally interned one.
		// This is critical for hygiene: the syntaxMap uses symbol pointers as keys,
		// and if we use interned symbols, different occurrences of the same name
		// (user's 'p' vs template's 'p') would collide.
		//
		// By creating fresh symbols for INPUT, we ensure:
		// 1. Each input position has a unique pointer in syntaxMap
		// 2. Template symbols (which are the original interned pointers) won't be in syntaxMap
		// 3. Template symbols will correctly get intro-scope
		//
		// Literal Hygiene: If this symbol has the same name as a pattern literal,
		// check if the scopes are compatible. If the input has been shadowed
		// (has extra scopes from a let binding), generate a unique symbol that
		// won't match the pattern literal. This implements R7RS auxiliary syntax
		// hygiene for => and else in cond/case.
		symKey := s.Sym.Key
		if sm.literalSyntax != nil {
			if patternLit, hasLit := sm.literalSyntax[symKey]; hasLit {
				if !sm.literalScopesMatchWithChecker(s, patternLit) {
					// Scopes incompatible or input has binding - symbol is shadowed
					// Use a unique key that won't match the pattern literal
					symKey = symKey + "$shadowed$"
				}
			}
		}
		result = values.NewSymbol(symKey)

	case *syntax.SyntaxObject:
		result = s.Unwrap()

	default:
		unwrapper, ok := stx.(interface{ Unwrap() values.Value })
		if ok {
			result = unwrapper.Unwrap()
		} else {
			val := stx
			result = val
		}
	}

	// Store the mapping from raw value to original syntax
	// This allows us to recover the original scopes during expansion
	if result != nil {
		sm.syntaxMap[result] = stx
	}

	return result
}

// valueToSyntaxWithIntroScope wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
func (sm *SyntaxMatcher) valueToSyntaxWithIntroScope(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any) syntax.SyntaxValue {
	return sm.valueToSyntaxWithUseSite(val, templateStx, introScope, freeIds, nil)
}

// valueToSyntaxWithUseSite wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
// When useSiteCtx is provided, it's used as the source context for newly
// created syntax objects instead of the template's context.
func (sm *SyntaxMatcher) valueToSyntaxWithUseSite(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any, useSiteCtx *syntax.SourceContext) syntax.SyntaxValue {
	return sm.valueToSyntaxWithOrigin(val, templateStx, introScope, freeIds, useSiteCtx, nil)
}

// valueToSyntaxWithOrigin wraps raw values back into syntax objects,
// using the syntax map to preserve original syntax objects from captured
// pattern variables. When introScope is provided, it's added to newly
// created symbols (but not to preserved originals or free identifiers).
// When useSiteCtx is provided, it's used as the source context for newly
// created syntax objects instead of the template's context.
// When origin is provided, it's attached to the source context of newly
// created syntax objects to track macro expansion chains.
// The freeIds map contains free identifier names mapped to their pre-resolved
// bindings (any type, typically *environment.GlobalIndex).
func (sm *SyntaxMatcher) valueToSyntaxWithOrigin(val values.Value, templateStx syntax.SyntaxValue, introScope *syntax.Scope, freeIds map[string]any, useSiteCtx *syntax.SourceContext, origin *syntax.OriginInfo) syntax.SyntaxValue {
	if val == nil {
		return nil
	}

	// Check if this value has a corresponding original syntax object in syntaxMap.
	// If so, return the original to preserve scopes (no intro scope added!).
	//
	// This works for symbols too now because syntaxToValue creates FRESH symbol
	// instances for input (not the globally interned ones). This means:
	// - Input symbols have unique pointers, correctly mapped in syntaxMap
	// - Template symbols use the original interned pointers, NOT in syntaxMap
	// - Template symbols will go through the intro-scope path below
	if origStx, ok := sm.syntaxMap[val]; ok {
		return origStx
	}

	// Determine source context: use-site context takes precedence if provided,
	// otherwise fall back to template context
	var srcCtx *syntax.SourceContext
	if useSiteCtx != nil {
		srcCtx = useSiteCtx
	} else if templateStx != nil {
		srcCtx = templateStx.SourceContext()
	}

	// If we have origin info, attach it to the source context
	if origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(origin)
	} else if origin != nil {
		srcCtx = &syntax.SourceContext{Origin: origin}
	}

	switch v := val.(type) {
	case *values.Pair:
		if values.IsEmptyList(v) {
			return syntax.NewSyntaxEmptyList(srcCtx)
		}

		// Recursively wrap car and cdr, checking the map for each
		car := sm.valueToSyntaxWithOrigin(v[0], templateStx, introScope, freeIds, useSiteCtx, origin)

		var cdr syntax.SyntaxValue
		if v[1] == nil || values.IsEmptyList(v[1]) {
			cdr = syntax.NewSyntaxEmptyList(srcCtx)
		} else {
			cdr = sm.valueToSyntaxWithOrigin(v[1], templateStx, introScope, freeIds, useSiteCtx, origin)
		}

		return syntax.NewSyntaxCons(car, cdr, srcCtx)

	case *values.Symbol:
		// Check if this is a free identifier (not a pattern variable)
		// Free identifiers should resolve to their definition-time bindings,
		// so they must NOT inherit use-site scopes.
		var isFree bool
		var resolution any
		if freeIds != nil {
			resolution, isFree = freeIds[v.Key]
		}

		// Handle free identifiers - they can have local or global resolution
		if isFree && resolution != nil {
			// Check for local binding resolution first (for let-syntax hygiene)
			if lsp, ok := resolution.(localScopesProvider); ok {
				localScopes := lsp.GetLocalScopes()
				// Check for non-empty local scopes
				if len(localScopes) > 0 {
					// Local binding with scopes - use ONLY definition-site scopes, NOT use-site scopes
					// This ensures the identifier resolves to the binding from
					// macro definition time, not a shadowing binding at use site.
					// We create a new context with only the definition-site scopes,
					// discarding any use-site scopes that srcCtx may have.
					var scopedCtx *syntax.SourceContext
					if srcCtx != nil {
						scopedCtx = &syntax.SourceContext{
							Text:   srcCtx.Text,
							File:   srcCtx.File,
							Start:  srcCtx.Start,
							End:    srcCtx.End,
							Origin: srcCtx.Origin,
							Scopes: localScopes, // Use ONLY definition-site scopes
						}
					} else {
						scopedCtx = &syntax.SourceContext{Scopes: localScopes}
					}
					sym := syntax.NewSyntaxSymbol(v.Key, scopedCtx)
					// No intro scope for free identifiers - they refer to outside bindings
					return sym
				}
			}

			// Check for global binding resolution (for cross-library hygiene)
			if gbp, ok := resolution.(globalBindingProvider); ok {
				globalBinding := gbp.GetGlobal()
				// Check if globalBinding is actually non-nil (not just interface-non-nil)
				// Go interfaces are nil only if both type and value are nil
				isActuallyNil := globalBinding == nil
				if !isActuallyNil {
					// Use reflection to check if the underlying value is nil
					rv := reflect.ValueOf(globalBinding)
					if !rv.IsValid() || rv.IsNil() {
						isActuallyNil = true
					}
				}
				if !isActuallyNil {
					// Global binding - strip scopes and attach resolved binding
					symCtx := srcCtx
					if srcCtx != nil && len(srcCtx.Scopes) > 0 {
						symCtx = &syntax.SourceContext{
							Text:   srcCtx.Text,
							File:   srcCtx.File,
							Start:  srcCtx.Start,
							End:    srcCtx.End,
							Origin: srcCtx.Origin,
							// Scopes intentionally omitted for global free identifiers
						}
					}
					sym := syntax.NewSyntaxSymbol(v.Key, symCtx)
					sym = sym.WithResolvedBinding(globalBinding)
					return sym
				}
			}

			// Check if a local binding was found (even with empty scopes).
			// This handles the case where a macro references a local variable
			// that doesn't have scopes attached (e.g., from a let binding).
			// The identifier should NOT get intro scope - it should preserve
			// its original scopes so it resolves to the definition-time binding.
			if hlp, ok := resolution.(hasLocalBindingProvider); ok && hlp.GetHasLocalBinding() {
				// Local binding found but no scopes - preserve original scopes
				// (don't add intro scope, which would break hygiene)
				sym := syntax.NewSyntaxSymbol(v.Key, srcCtx)
				return sym
			}

			// Resolution exists but has no local or global binding (e.g., special forms,
			// or template-bound identifiers like 'x' in (let ((x ...)) ...))
			//
			// IMPORTANT: We still add intro scope here! Template-bound identifiers need
			// intro scope to be distinguished from user's identifiers with the same name.
			// This fixes the hygiene bug where (let ((x 8)) (or #f (or #f x))) was returning
			// #f instead of 8 because template's 'x' binding was capturing user's 'x'.
			//
			// For special forms (if, let, etc.), the intro scope is harmless because they
			// resolve via their definition-time binding or compile-time handling.
			symCtx := srcCtx
			if srcCtx != nil && len(srcCtx.Scopes) > 0 {
				symCtx = &syntax.SourceContext{
					Text:   srcCtx.Text,
					File:   srcCtx.File,
					Start:  srcCtx.Start,
					End:    srcCtx.End,
					Origin: srcCtx.Origin,
				}
			}
			sym := syntax.NewSyntaxSymbol(v.Key, symCtx)
			// Add intro scope to template-bound identifiers for hygiene
			if introScope != nil {
				sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
			}
			return sym
		}

		// Not a free identifier - create symbol with intro scope
		// Strip scopes from srcCtx: template identifiers should not inherit
		// use-site scopes, only the explicitly-added intro scope (Flatt 2016).
		// This prevents template's `if` from matching user's `(let ((if f)) ...)`
		templateCtx := srcCtx
		if srcCtx != nil && len(srcCtx.Scopes) > 0 {
			templateCtx = srcCtx.WithoutScopes()
		}
		sym := syntax.NewSyntaxSymbol(v.Key, templateCtx)
		if introScope != nil {
			sym = sym.AddScope(introScope).(*syntax.SyntaxSymbol)
		}
		return sym

	case *values.Integer:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Float:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.String:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Boolean:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Character:
		return syntax.NewSyntaxObject(v, srcCtx)

	case *values.Vector:
		// Recursively wrap each element
		elements := make([]syntax.SyntaxValue, len(*v))
		for i, elem := range *v {
			elements[i] = sm.valueToSyntaxWithOrigin(elem, templateStx, introScope, freeIds, useSiteCtx, origin)
		}
		return syntax.NewSyntaxVector(srcCtx, elements...)

	// Handle already-wrapped syntax types - these may appear if captured values
	// contain syntax objects (e.g., from vector literals in templates)
	case *syntax.SyntaxSymbol:
		// Already a syntax symbol - add intro scope if needed
		if introScope != nil {
			return v.AddScope(introScope).(*syntax.SyntaxSymbol)
		}
		return v

	case *syntax.SyntaxPair:
		// Already a syntax pair - return as-is
		return v

	case *syntax.SyntaxVector:
		// Already a syntax vector - return as-is
		return v

	case *syntax.SyntaxObject:
		// Already a syntax object - return as-is
		return v

	default:
		// For any other value type, wrap in generic syntax object
		return syntax.NewSyntaxObject(val, srcCtx)
	}
}

// syntaxToValue recursively unwraps syntax objects to raw values.Value types.
//
// This is the "datum" extraction from R7RS syntax->datum. It strips away:
//   - Source location information (file, line, column)
//   - Scope sets (used for hygiene)
//
// The pattern matching VM operates on these raw values because:
//  1. Pattern matching is structural - it doesn't care about source locations
//  2. The unhygienic core doesn't need scope information
//  3. Raw values are simpler and faster to traverse
//
// After expansion, valueToSyntax re-wraps the result, and the hygiene layer
// adds intro scopes to the new syntax objects.
func syntaxToValue(stx syntax.SyntaxValue) values.Value {
	if stx == nil {
		return nil
	}

	switch s := stx.(type) {
	case *syntax.SyntaxPair:
		if s == nil || s.IsEmptyList() {
			return values.EmptyList
		}
		// Recursively unwrap car and cdr
		var car values.Value
		carVal := s.Car()
		if carVal != nil {
			carSyntax, ok := carVal.(syntax.SyntaxValue)
			if ok {
				car = syntaxToValue(carSyntax)
			} else {
				// If it's already a value, use it directly
				car = carVal.(values.Value)
			}
		}

		var cdr values.Value
		cdrVal := s.SyntaxCdr()
		if cdrVal != nil {
			cdrSyntax := cdrVal
			cdr = syntaxToValue(cdrSyntax)
			// } else {
			// If it's already a value, use it directly
			//	cdr = cdrVal.(values.Value)
			//}
		}

		// Handle proper lists and improper lists
		if cdr == nil || values.IsEmptyList(cdr) {
			return values.NewCons(car, values.EmptyList)
		}
		cdrPair, ok := cdr.(*values.Pair)
		if ok {
			return values.NewCons(car, cdrPair)
		}
		// Improper list
		return values.NewCons(car, cdr)

	case *syntax.SyntaxSymbol:
		return s.Unwrap()

	case *syntax.SyntaxObject:
		return s.Unwrap()

	default:
		// For other syntax types, try to unwrap
		if unwrapper, ok := stx.(interface{ Unwrap() values.Value }); ok {
			return unwrapper.Unwrap()
		}
		// If it's already a value, return as-is
		val := stx
		return val
	}
}

// valueToSyntax wraps raw values back into syntax objects.
//
// This is the inverse of syntaxToValue, similar to R7RS datum->syntax.
// It reconstructs syntax objects from the expanded template, preserving:
//   - Source context from the original template (for error reporting)
//   - Structure of the expanded form
//
// Note: The scopes are NOT preserved during this conversion. Instead,
// the hygiene layer (in operation_syntax_rules_transform.go) adds a fresh
// "intro scope" to all syntax objects after expansion. This is the key to
// Flatt's "sets of scopes" hygiene model.
//
// The templateStx parameter provides the source context (file, line, etc.)
// that will be attached to the new syntax objects.
func valueToSyntax(val values.Value, templateStx syntax.SyntaxValue) syntax.SyntaxValue {
	if val == nil {
		return nil
	}

	// Get source context from template if available
	var srcCtx *syntax.SourceContext
	if templateStx != nil {
		srcCtx = templateStx.SourceContext()
	}

	switch v := val.(type) {
	case *values.Pair:
		if values.IsEmptyList(v) {
			// Return syntax empty list for empty list
			return syntax.NewSyntaxEmptyList(srcCtx)
		}

		// Recursively wrap car and cdr
		car := valueToSyntax(v[0], templateStx)

		// Handle cdr - could be another pair or an atom (improper list)
		var cdr syntax.SyntaxValue
		if v[1] == nil || values.IsEmptyList(v[1]) {
			cdr = syntax.NewSyntaxEmptyList(srcCtx)
		} else {
			cdr = valueToSyntax(v[1], templateStx)
		}

		return syntax.NewSyntaxCons(car, cdr, srcCtx)

	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, srcCtx)

	default:
		// For any other value type, wrap in generic syntax object
		return syntax.NewSyntaxObject(val, srcCtx)
	}
}

// CompiledPattern contains the compiled bytecode and ellipsis variable mapping.
type CompiledPattern struct {
	Codes        []SyntaxCommand
	EllipsisVars map[int]map[string]struct{}
	EllipsisID   string // The ellipsis identifier used during compilation
}

// CompileSyntaxPattern compiles a syntax pattern into bytecode.
// This is a convenience function that unwraps syntax before compilation.
// Uses the default ellipsis identifier ("...").
func CompileSyntaxPattern(ctx context.Context, pattern syntax.SyntaxValue, variables map[string]struct{}) ([]SyntaxCommand, error) {
	result, err := CompileSyntaxPatternFull(ctx, pattern, variables)
	if err != nil {
		return nil, err
	}
	return result.Codes, nil
}

// CompileSyntaxPatternFull compiles a syntax pattern into bytecode with ellipsis variable mapping.
// Returns a CompiledPattern containing both the bytecode and the ellipsis variable mapping.
// Uses the default ellipsis identifier ("...").
func CompileSyntaxPatternFull(ctx context.Context, pattern syntax.SyntaxValue, variables map[string]struct{}) (*CompiledPattern, error) {
	return CompileSyntaxPatternWithEllipsis(ctx, pattern, variables, DefaultEllipsis)
}

// CompileSyntaxPatternWithEllipsis compiles a syntax pattern into bytecode with a custom ellipsis.
// The ellipsisID parameter specifies the identifier used for ellipsis patterns
// (default is "..." per R7RS, but can be customized per R7RS §4.3.2).
func CompileSyntaxPatternWithEllipsis(ctx context.Context, pattern syntax.SyntaxValue, variables map[string]struct{}, ellipsisID string) (*CompiledPattern, error) {
	return CompileSyntaxPatternWithLiterals(ctx, pattern, variables, nil, ellipsisID)
}

// CompileSyntaxPatternWithLiterals compiles a syntax pattern into bytecode with literals and custom ellipsis.
// The literals parameter contains identifiers that should be matched literally (not as pattern variables).
// The ellipsisID parameter specifies the identifier used for ellipsis patterns.
// R7RS §4.3.2: The first subform of each pattern is the keyword of the macro being transformed;
// it is not matched against the macro use being transformed.
func CompileSyntaxPatternWithLiterals(ctx context.Context, pattern syntax.SyntaxValue, variables map[string]struct{}, literals map[string]struct{}, ellipsisID string) (*CompiledPattern, error) {
	if ellipsisID == "" {
		ellipsisID = DefaultEllipsis
	}

	// Convert syntax pattern to raw values
	rawPattern := syntaxToValue(pattern)

	// Ensure it's a pair
	pair, ok := rawPattern.(*values.Pair)
	if !ok {
		return nil, errors.New("pattern must be a list")
	}

	// Compile using compiler with custom ellipsis and literals
	compiler := NewSyntaxCompilerWithEllipsis(ellipsisID)
	compiler.variables = variables
	if literals != nil {
		compiler.literals = literals
	}
	// Enable macro keyword skipping for syntax-rules patterns.
	// R7RS §4.3.2: The first subform of each pattern is the keyword of the macro.
	compiler.SetSkipMacroKeyword(true)
	err := compiler.Compile(ctx, pair)
	if err != nil {
		return nil, err
	}

	return &CompiledPattern{
		Codes:        compiler.codes,
		EllipsisVars: compiler.ellipsisVars,
		EllipsisID:   ellipsisID,
	}, nil
}

// GetBindings returns the captured pattern variable bindings from the last match,
// converted back to syntax values. This is used by syntax-case to bind pattern
// variables in the body's environment.
func (sm *SyntaxMatcher) GetBindings() map[string]syntax.SyntaxValue {
	rawBindings := sm.matcher.GetBindings()
	if rawBindings == nil {
		return nil
	}

	result := make(map[string]syntax.SyntaxValue)
	for name, rawValue := range rawBindings {
		// Convert raw value back to syntax using the syntax map
		if stx, ok := sm.syntaxMap[rawValue]; ok {
			result[name] = stx
		} else {
			// Wrap the raw value in a SyntaxObject if not in map
			result[name] = syntax.NewSyntaxObject(rawValue, nil)
		}
	}
	return result
}

// literalScopesMatch checks if an input symbol should match a pattern literal.
//
// Per R7RS §4.3.2, a subform in the input matches a literal identifier if and
// only if it is an identifier and either:
//   - both its occurrence in the macro expression and its occurrence in the
//     macro definition have the same lexical binding, or
//   - the two identifiers are the same and both have no lexical binding.
//
// This function returns true if the input symbol refers to the same binding
// as the pattern literal. It performs two checks:
//
// 1. Binding check (R7RS compliant): If a binding checker is available, we check
// if the input has a lexical binding. Pattern literals (like => and else) are
// by definition not bound in the macro definition. If the input IS bound
// (e.g., via let or lambda), it doesn't match the unbound pattern literal.
//
// 2. Scope check (for let-syntax): We also check rebinding scopes from
// let-syntax/letrec-syntax. If input has rebinding scopes that pattern doesn't,
// the literal has been shadowed by a macro binding.
//
// Example with regular let:
//
//	(let ((=> #f)) (cond (#t => 'ok)))
//	The input => has a lexical binding (the let-bound variable)
//	Pattern => has no lexical binding
//	They don't match because one is bound and one isn't
//
// Example with let-syntax:
//
//	(let-syntax ((=> ...)) (cond (#t => 'ok)))
//	The input => has rebinding scope {letSyntaxScope}
//	Pattern => has no rebinding scopes, so they don't match
func (sm *SyntaxMatcher) literalScopesMatchWithChecker(input, pattern *syntax.SyntaxSymbol) bool {
	if input == nil || pattern == nil {
		return false
	}

	// R7RS §4.3.2 binding check: literals match if both have the same lexical
	// binding, or both have no lexical binding. After library import, auxiliary
	// syntax like => gets exported to phase 0, so both input and pattern may
	// have bindings. We compare the actual bindings, not just whether they exist.
	if sm.bindingChecker != nil {
		inputBinding := sm.bindingChecker.GetBinding(input.Sym.Key, input.Scopes())
		patternBinding := sm.bindingChecker.GetBinding(pattern.Sym.Key, pattern.Scopes())

		// R7RS §4.3.2: literals match if both have the same binding, or both unbound
		if inputBinding != patternBinding {
			// Different bindings (or one bound and one not) - don't match
			return false
		}
		// Same binding (including both nil) - continue to scope check below
	}

	// Also check rebinding scopes for let-syntax shadowing.
	// This handles cases where the binding checker isn't available,
	// and provides defense-in-depth for let-syntax cases.
	inputRebindingScopes := filterRebindingScopes(input.Scopes())
	patternRebindingScopes := filterRebindingScopes(pattern.Scopes())

	// For the input to match the pattern literal, the input must not have
	// any rebinding scopes that the pattern doesn't have.
	return syntax.ScopesMatch(patternRebindingScopes, inputRebindingScopes)
}

// literalScopesMatch is the standalone version for backward compatibility.
// It only checks rebinding scopes, not actual bindings.
func literalScopesMatch(input, pattern *syntax.SyntaxSymbol) bool {
	if input == nil || pattern == nil {
		return false
	}

	inputRebindingScopes := filterRebindingScopes(input.Scopes())
	patternRebindingScopes := filterRebindingScopes(pattern.Scopes())

	return syntax.ScopesMatch(patternRebindingScopes, inputRebindingScopes)
}

// filterRebindingScopes returns only the scopes that are marked as rebinding scopes.
// These are scopes from let-syntax/letrec-syntax that could shadow auxiliary syntax.
func filterRebindingScopes(scopes []*syntax.Scope) []*syntax.Scope {
	var result []*syntax.Scope
	for _, s := range scopes {
		if s != nil && s.IsRebinding {
			result = append(result, s)
		}
	}
	return result
}
