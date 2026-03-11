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

package match

// syntax_expand.go contains template expansion methods for SyntaxMatcher.
//
// Responsibility: Recursively expand syntax templates with captured bindings,
// applying intro scopes for hygiene (Flatt 2016 "sets of scopes" model).
//
// Entry point: SyntaxMatcher.Expand()
// Key operations:
//   - Pattern variable substitution with scope compatibility checks
//   - Ellipsis repetition expansion
//   - Free identifier resolution (local, global, library-scoped)
//   - Intro scope application to newly created syntax

import (
	"maps"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ExpandOptions holds the hygiene and source-tracking parameters for template expansion.
// All fields are optional; the zero value expands without hygiene (useful for testing).
//
// Per Flatt 2016 "sets of scopes" model: when deciding whether to substitute a template
// symbol with a captured value, we compare the template symbol's scopes with the pattern
// variable's scopes. Only substitute if the scopes are compatible (pattern var scopes ⊆
// template symbol scopes). If the template symbol has additional scopes (e.g., from an
// outer macro's intro scope), it should NOT be substituted.
type ExpandOptions struct {
	// IntroScope is the hygiene scope added to newly created syntax objects (from the
	// template), but NOT to syntax objects preserved from pattern variable substitution.
	IntroScope *syntax.Scope

	// FreeIds maps free identifier names to their pre-resolved bindings.
	// Identifiers in this map do not receive the intro scope.
	// Values use type any to avoid circular imports (implemented by machine.FreeIdResolution).
	// A nil value means just skip intro scope; a non-nil value carries resolved binding info.
	FreeIds map[string]any

	// UseSiteCtx, if provided, is used for the source context of newly created syntax
	// objects instead of the template's context. This allows error messages to point to
	// where the macro was invoked rather than where it was defined.
	UseSiteCtx *syntax.SourceContext

	// Origin tracks the macro expansion chain for debugging and error reporting.
	// WithOrigin replaces (not appends to) a SourceContext's Origin field, so the
	// last expansion pass wins. This is correct because the caller constructs the
	// OriginInfo with the full chain: it reads the previous SourceContext.Origin and
	// sets it as Parent on the new OriginInfo before calling Expand. The chain lives
	// inside OriginInfo.Parent, not across successive SourceContext.Origin values.
	//
	// The nil guard at each call site skips when no origin is provided — avoiding a
	// pointless allocation and preserving any existing origin on the SourceContext
	// (e.g., syntax-case expands with ExpandOptions{}, where Origin is nil).
	Origin *syntax.OriginInfo

	// PatternVarSyntax contains the syntax symbols from the pattern, enabling nested
	// macro hygiene via scope comparison. When set, template symbols are only substituted
	// if their scopes match the corresponding pattern variable's scopes.
	PatternVarSyntax map[string]*syntax.SyntaxSymbol
}

// Expand performs template expansion with the given hygiene options.
// Pass a zero-value ExpandOptions{} for expansion without hygiene.
func (p *SyntaxMatcher) Expand(template syntax.SyntaxValue, opts ExpandOptions) (syntax.SyntaxValue, error) {
	if len(p.matcher.captureStack) == 0 {
		return nil, werr.WrapForeignErrorf(werr.ErrNoCaptureContext, "Expand: no captures available for template expansion")
	}

	// Perform syntax-preserving expansion with scope comparison
	return p.expandSyntaxValue(
		template,
		p.matcher.captureStack[0],
		nil, // ellipsisVars
		&opts,
	)
}

// expandSyntaxValue recursively expands a syntax template with captured bindings,
// preserving scope information and using scope comparison for pattern variable substitution.
// This is the syntax-level expansion that correctly handles nested macro hygiene.
func (p *SyntaxMatcher) expandSyntaxValue(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	if template == nil {
		return nil, nil
	}

	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		return p.expandSymbol(t, ctx, opts)

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return t, nil
		}

		// Check for ellipsis escape form: (<ellipsis> <template>)
		car := t.SyntaxCar()
		carSym, ok := car.(*syntax.SyntaxSymbol)
		if ok {
			if carSym.Unwrap().(*values.Symbol).Key == p.ellipsisID {
				cdr := t.SyntaxCdr()
				cdrPair, ok := cdr.(*syntax.SyntaxPair)
				if ok && !syntax.IsSyntaxEmptyList(cdrPair) {
					// Escape form - expand inner template without ellipsis handling
					return p.expandEscapedSyntaxTemplate(
						cdrPair.SyntaxCar(), ctx, ellipsisVars, opts)
				}
			}
		}

		// Check for ellipsis pattern (something <ellipsis>)
		cdr := t.SyntaxCdr()
		cdrPair, ok := cdr.(*syntax.SyntaxPair)
		if ok && !syntax.IsSyntaxEmptyList(cdrPair) {
			sym, ok := cdrPair.SyntaxCar().(*syntax.SyntaxSymbol)
			if ok {
				if sym.Unwrap().(*values.Symbol).Key == p.ellipsisID {
					// Found ellipsis - handle repetition
					return p.expandSyntaxEllipsis(
						car, cdrPair.SyntaxCdr(), ctx, ellipsisVars, opts)
				}
			}
		}

		// Regular pair - expand car and cdr
		expandedCar, err := p.expandSyntaxValue(car, ctx, ellipsisVars, opts)
		if err != nil {
			return nil, err
		}
		expandedCdr, err := p.expandSyntaxValue(cdr, ctx, ellipsisVars, opts)
		if err != nil {
			return nil, err
		}

		srcCtx := t.SourceContext()
		if opts.UseSiteCtx != nil {
			srcCtx = opts.UseSiteCtx
		}
		// Stamp origin onto structural nodes so error traces identify which macro
		// produced them. See ExpandOptions.Origin for why "last caller wins" is correct.
		if opts.Origin != nil {
			srcCtx = srcCtx.WithOrigin(opts.Origin)
		}
		return syntax.NewSyntaxCons(expandedCar, expandedCdr, srcCtx), nil

	case *syntax.SyntaxVector:
		// Expand each element
		expandedElements := make([]syntax.SyntaxValue, len(t.Values))
		for i, elem := range t.Values {
			expanded, err := p.expandSyntaxValue(elem, ctx, ellipsisVars, opts)
			if err != nil {
				return nil, err
			}
			expandedElements[i] = expanded
		}
		srcCtx := t.SourceContext()
		if opts.UseSiteCtx != nil {
			srcCtx = opts.UseSiteCtx
		}
		if opts.Origin != nil {
			srcCtx = srcCtx.WithOrigin(opts.Origin)
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
func (p *SyntaxMatcher) applyHygieneToSymbol(
	sym *syntax.SyntaxSymbol,
	opts *ExpandOptions,
) syntax.SyntaxValue {
	symVal := sym.Unwrap().(*values.Symbol)

	// Determine source context
	srcCtx := sym.SourceContext()
	if opts.UseSiteCtx != nil {
		srcCtx = opts.UseSiteCtx
	}
	if opts.Origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(opts.Origin)
	} else if opts.Origin != nil {
		srcCtx = &syntax.SourceContext{Origin: opts.Origin}
	}

	// Check if this is a free identifier
	var isFree bool
	var resolution any
	if opts.FreeIds != nil {
		resolution, isFree = opts.FreeIds[symVal.Key]
	}

	if isFree && resolution != nil {
		// Handle free identifier resolution (local or global binding)
		lsp, ok := resolution.(localScopesProvider)
		if ok {
			localScopes := lsp.GetLocalScopes()
			if len(localScopes) > 0 {
				// Local binding - use definition-site scopes
				scopedCtx := srcCtx.Clone()
				scopedCtx.Scopes = localScopes
				return syntax.NewSyntaxSymbol(symVal.Key, scopedCtx)
			}
		}

		gbp, ok := resolution.(globalBindingProvider)
		if ok {
			globalBinding := gbp.GetGlobal()
			if globalBinding != nil {
				// Check if we have a library scope — if so, add it to the
				// identifier so CompileSymbol can redirect to the library's env
				// via the TLE scope registry. This replaces WithResolvedBinding
				// for library-scoped bindings.
				lscp, lscpOk := resolution.(libraryScopeProvider)
				if lscpOk {
					libScope := lscp.GetLibraryScope()
					if libScope != nil {
						symCtx := srcCtx
						if srcCtx != nil && len(srcCtx.Scopes) > 0 {
							symCtx = srcCtx.WithoutScopes()
						}
						newSym := syntax.NewSyntaxSymbol(symVal.Key, symCtx)
						newSym = newSym.AddScope(libScope).(*syntax.SyntaxSymbol)
						return newSym
					}
				}

				// No library scope — fall back to WithResolvedBinding
				// (backward compat for non-library macros or during transition)
				symCtx := srcCtx
				if srcCtx != nil && len(srcCtx.Scopes) > 0 {
					symCtx = srcCtx.WithoutScopes()
				}
				newSym := syntax.NewSyntaxSymbol(symVal.Key, symCtx)
				return newSym.WithResolvedBinding(globalBinding)
			}
		}

		hlp, ok := resolution.(hasLocalBindingProvider)
		if ok && hlp.GetHasLocalBinding() {
			return syntax.NewSyntaxSymbol(symVal.Key, srcCtx)
		}
	}

	// Not a free identifier or unresolved - create symbol with intro scope
	templateCtx := srcCtx
	if srcCtx != nil && len(srcCtx.Scopes) > 0 {
		templateCtx = srcCtx.WithoutScopes()
	}
	newSym := syntax.NewSyntaxSymbol(symVal.Key, templateCtx)
	if opts.IntroScope != nil {
		newSym = newSym.AddScope(opts.IntroScope).(*syntax.SyntaxSymbol)
	}
	return newSym
}

// expandSymbol handles symbol expansion for both normal and escaped template contexts.
// It checks pattern variable bindings, scope compatibility, and applies hygiene.
func (p *SyntaxMatcher) expandSymbol(
	t *syntax.SyntaxSymbol,
	ctx *captureContext,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	symVal := t.Unwrap().(*values.Symbol)

	capturedVal, ok := ctx.bindings[symVal.Key]
	if ok {
		if opts.PatternVarSyntax != nil {
			patternSym, hasPattern := opts.PatternVarSyntax[symVal.Key]
			if hasPattern {
				templateScopes := t.Scopes()
				patternScopes := patternSym.Scopes()
				if !scopesCompatibleForSubstitution(templateScopes, patternScopes) {
					return p.applyHygieneToSymbol(t, opts), nil
				}
			}
		}
		return p.capturedValueToSyntax(capturedVal, opts)
	}

	return p.applyHygieneToSymbol(t, opts), nil
}

// capturedValueToSyntax converts a captured value back to syntax.
// Captured values from pattern variable substitution preserve their original scopes.
// Since bindings now store syntax.SyntaxValue directly, this typically just returns
// the value if it's already syntax.
//
//nolint:unparam
func (p *SyntaxMatcher) capturedValueToSyntax(
	val values.Value,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	// If the value is already a syntax value (from syntax-native capture), return it directly.
	// This is the normal case because captureContext.bindings stores syntax.SyntaxValue directly.
	sv, ok := val.(syntax.SyntaxValue)
	if ok {
		return sv, nil
	}

	// Fallback: wrap the value in syntax (for edge cases like nil or empty list)
	srcCtx := opts.UseSiteCtx
	if opts.Origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(opts.Origin)
	} else if opts.Origin != nil {
		srcCtx = &syntax.SourceContext{Origin: opts.Origin}
	}

	switch v := val.(type) {
	case *values.Pair:
		car, err := p.capturedValueToSyntax(v.Car(), opts)
		if err != nil {
			return nil, err
		}
		cdr, err := p.capturedValueToSyntax(v.Cdr(), opts)
		if err != nil {
			return nil, err
		}
		return syntax.NewSyntaxCons(car, cdr, srcCtx), nil

	case values.Tuple:
		if v.IsEmptyList() {
			return syntax.SyntaxEmptyList, nil
		}
		return syntax.NewSyntaxObject(val, srcCtx), nil

	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, srcCtx), nil

	default:
		return syntax.NewSyntaxObject(val, srcCtx), nil
	}
}

// expandSyntaxEllipsis handles template repetition with ellipsis at the syntax level.
func (p *SyntaxMatcher) expandSyntaxEllipsis(
	pattern syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	// Find which variables in the pattern are bound in child contexts
	patternVarsInTemplate := p.findSyntaxPatternVariables(pattern)

	// Find the ellipsis ID that captured these variables
	ellipsisID := p.matcher.findMatchingEllipsisID(patternVarsInTemplate)
	if ellipsisID < 0 {
		// No matching ellipsis - just expand the rest
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, opts)
	}

	// Get children for this specific ellipsis ID
	children := ctx.children[ellipsisID]
	if len(children) == 0 {
		// No repetitions captured, just expand the rest
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, opts)
	}

	// Build result by repeating pattern for each child context
	var results []syntax.SyntaxValue
	for _, childCtx := range children {
		// Create a new ellipsis variable set for this expansion
		newEllipsisVars := make(map[string]struct{})
		maps.Copy(newEllipsisVars, ellipsisVars)
		for v := range patternVarsInTemplate {
			newEllipsisVars[v] = struct{}{}
		}

		expanded, err := p.expandSyntaxValue(pattern, childCtx, newEllipsisVars, opts)
		if err != nil {
			return nil, err
		}
		results = append(results, expanded)
	}

	// Expand the rest
	expandedRest, err := p.expandSyntaxValue(rest, ctx, ellipsisVars, opts)
	if err != nil {
		return nil, err
	}

	// Combine results into a list and append the rest
	srcCtx := opts.UseSiteCtx
	if srcCtx == nil && pattern != nil {
		srcCtx = pattern.SourceContext()
	}
	if opts.Origin != nil {
		srcCtx = srcCtx.WithOrigin(opts.Origin)
	}
	result := expandedRest
	for i := len(results) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(results[i], result, srcCtx)
	}

	return result, nil
}

// findSyntaxPatternVariables finds all pattern variables in a syntax template.
func (p *SyntaxMatcher) findSyntaxPatternVariables(template syntax.SyntaxValue) map[string]struct{} {
	vars := make(map[string]struct{})
	p.findSyntaxVarsRecursive(template, vars)
	return vars
}

func (p *SyntaxMatcher) findSyntaxVarsRecursive(template syntax.SyntaxValue, vars map[string]struct{}) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		symVal := t.Unwrap().(*values.Symbol)
		_, ok := p.matcher.variables[symVal.Key]
		if ok {
			vars[symVal.Key] = struct{}{}
		}
	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(t) {
			p.findSyntaxVarsRecursive(t.SyntaxCar(), vars)
			p.findSyntaxVarsRecursive(t.SyntaxCdr(), vars)
		}
	case *syntax.SyntaxVector:
		for _, elem := range t.Values {
			p.findSyntaxVarsRecursive(elem, vars)
		}
	}
}

// expandEscapedSyntaxTemplate expands a template inside an ellipsis escape form at the syntax level.
//
//nolint:unparam
func (p *SyntaxMatcher) expandEscapedSyntaxTemplate(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	if template == nil {
		return nil, nil
	}

	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		return p.expandSymbol(t, ctx, opts)

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return t, nil
		}
		// In escaped context, don't check for ellipsis patterns
		car, err := p.expandEscapedSyntaxTemplate(t.SyntaxCar(), ctx, ellipsisVars, opts)
		if err != nil {
			return nil, err
		}
		cdr, err := p.expandEscapedSyntaxTemplate(t.SyntaxCdr(), ctx, ellipsisVars, opts)
		if err != nil {
			return nil, err
		}
		srcCtx := t.SourceContext()
		if opts.UseSiteCtx != nil {
			srcCtx = opts.UseSiteCtx
		}
		if opts.Origin != nil {
			srcCtx = srcCtx.WithOrigin(opts.Origin)
		}
		return syntax.NewSyntaxCons(car, cdr, srcCtx), nil

	default:
		return template, nil
	}
}
