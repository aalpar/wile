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
// variable's scopes. Only substitute if the scopes are compatible, meaning the template
// symbol and pattern variable have exactly the same set of scopes (pattern scopes ⊆
// template scopes and template scopes ⊆ pattern scopes). A template symbol with any
// additional or missing scopes (e.g., from an outer macro's intro scope) is not
// substituted.
type ExpandOptions struct {
	// IntroScope is the hygiene scope added to newly created syntax objects (from the
	// template), but NOT to syntax objects preserved from pattern variable substitution.
	IntroScope *syntax.Scope

	// FreeIds maps free identifier names to their pre-resolved bindings.
	// A non-nil value carries resolved binding info from macro definition time
	// (local scopes, global index, library scope). A nil value is treated the
	// same as an absent key — the identifier receives the intro scope normally.
	FreeIds map[string]FreeIdResolver

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
		nil, // excludeEllipsisIDs
		&opts,
	)
}

// expandSyntaxValue recursively expands a syntax template with captured bindings,
// preserving scope information and using scope comparison for pattern variable substitution.
// This is the syntax-level expansion that correctly handles nested macro hygiene.
//
// The excludeEllipsisIDs parameter tracks ellipsis IDs that have already been consumed
// by an outer expansion level. This prevents nested ellipsis from re-selecting the
// same outer ID — e.g., for pattern ((a ...) ...) the outer ID is consumed first,
// and the inner expansion selects the inner ID.
func (p *SyntaxMatcher) expandSyntaxValue(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	excludeEllipsisIDs map[int]struct{},
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
						car, cdrPair.SyntaxCdr(), ctx, ellipsisVars, excludeEllipsisIDs, opts)
				}
			}
		}

		// Regular pair - expand car and cdr
		expandedCar, err := p.expandSyntaxValue(car, ctx, ellipsisVars, excludeEllipsisIDs, opts)
		if err != nil {
			return nil, err
		}
		expandedCdr, err := p.expandSyntaxValue(cdr, ctx, ellipsisVars, excludeEllipsisIDs, opts)
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
			expanded, err := p.expandSyntaxValue(elem, ctx, ellipsisVars, excludeEllipsisIDs, opts)
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
	var resolution FreeIdResolver
	if opts.FreeIds != nil {
		resolution, isFree = opts.FreeIds[symVal.Key]
	}

	if isFree && resolution != nil {
		// Handle free identifier resolution (local or global binding)
		localScopes := resolution.GetLocalScopes()
		if len(localScopes) > 0 {
			// Local binding - use definition-site scopes
			var scopedCtx *syntax.SourceContext
			if srcCtx != nil {
				scopedCtx = srcCtx.Clone()
			} else {
				scopedCtx = &syntax.SourceContext{}
			}
			scopedCtx.Scopes = localScopes
			return syntax.NewSyntaxSymbol(symVal.Key, scopedCtx)
		}

		globalBinding := resolution.GetGlobal()
		if globalBinding != nil {
			// Check if we have a library scope — if so, add it to the
			// identifier so CompileSymbol can redirect to the library's env
			// via the TLE scope registry.
			libScope := resolution.GetLibraryScope()
			if libScope != nil {
				symCtx := srcCtx
				if srcCtx != nil && len(srcCtx.Scopes) > 0 {
					symCtx = srcCtx.WithoutScopes()
				}
				newSym := syntax.NewSyntaxSymbol(symVal.Key, symCtx)
				newSym = newSym.AddScope(libScope).(*syntax.SyntaxSymbol)
				return newSym
			}

			// No library scope — fall back to WithResolvedBinding
			symCtx := srcCtx
			if srcCtx != nil && len(srcCtx.Scopes) > 0 {
				symCtx = srcCtx.WithoutScopes()
			}
			newSym := syntax.NewSyntaxSymbol(symVal.Key, symCtx)
			return newSym.WithResolvedBinding(globalBinding)
		}

		if resolution.GetHasLocalBinding() {
			return syntax.NewSyntaxSymbol(symVal.Key, srcCtx)
		}

		// Resolution is non-nil but all methods returned zero — the binding was
		// unresolvable at definition time. Fall through to intro scope.
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
	excludeEllipsisIDs map[int]struct{},
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	// Find which variables in the pattern are bound in child contexts
	patternVarsInTemplate := p.findSyntaxPatternVariables(pattern)

	// Find ALL ellipsis IDs that contribute variables to this template,
	// excluding any IDs already consumed by an outer expansion level.
	matchingIDs := p.matcher.findMatchingEllipsisIDs(patternVarsInTemplate, excludeEllipsisIDs)
	if len(matchingIDs) == 0 {
		// No matching ellipsis — the pattern element contains no variables that
		// were captured under any ellipsis, so it acts as a constant template
		// (e.g., a literal symbol followed by `...`). R7RS §4.3.2: "It is an
		// error if the template ... contains a pattern variable that does not
		// appear in the pattern." The pattern compiler validates this earlier;
		// reaching here means the element has no pattern variables at all, so
		// repeating it zero times (dropping it) is correct.
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	}

	if len(matchingIDs) == 1 {
		// Single-group fast path — no extra allocation.
		return p.expandEllipsisSingleGroup(
			pattern, rest, ctx, ellipsisVars, excludeEllipsisIDs, opts,
			patternVarsInTemplate, matchingIDs[0],
		)
	}

	// Multi-group path: variables span multiple ellipsis groups (cross-group zipping).
	return p.expandEllipsisCrossGroup(
		pattern, rest, ctx, ellipsisVars, excludeEllipsisIDs, opts,
		patternVarsInTemplate, matchingIDs,
	)
}

// expandEllipsisSingleGroup handles the common case where all template variables
// come from a single ellipsis group.
func (p *SyntaxMatcher) expandEllipsisSingleGroup(
	pattern syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	excludeEllipsisIDs map[int]struct{},
	opts *ExpandOptions,
	patternVarsInTemplate map[string]struct{},
	ellipsisID int,
) (syntax.SyntaxValue, error) {
	children := ctx.children[ellipsisID]
	if len(children) == 0 {
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	}

	// Add this ellipsis ID to the exclude set so nested ellipsis expansions
	// within the children select the inner ID instead of re-selecting this one.
	innerExclude := make(map[int]struct{}, len(excludeEllipsisIDs)+1)
	for id := range excludeEllipsisIDs {
		innerExclude[id] = struct{}{}
	}
	innerExclude[ellipsisID] = struct{}{}

	results, err := p.expandEllipsisChildren(pattern, children, ellipsisVars, innerExclude, opts, patternVarsInTemplate)
	if err != nil {
		return nil, err
	}

	return p.combineEllipsisResults(results, rest, ctx, ellipsisVars, excludeEllipsisIDs, opts, pattern)
}

// expandEllipsisCrossGroup handles cross-group zipping where template variables
// come from multiple ellipsis groups that must iterate in lockstep.
func (p *SyntaxMatcher) expandEllipsisCrossGroup(
	pattern syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	excludeEllipsisIDs map[int]struct{},
	opts *ExpandOptions,
	patternVarsInTemplate map[string]struct{},
	matchingIDs []int,
) (syntax.SyntaxValue, error) {
	// Verify all groups have the same iteration count.
	count := len(ctx.children[matchingIDs[0]])
	for _, id := range matchingIDs[1:] {
		idCount := len(ctx.children[id])
		if idCount != count {
			return nil, werr.WrapForeignErrorf(
				werr.ErrExpansion,
				"expandSyntaxEllipsis: ellipsis groups have different repetition counts (%d vs %d)",
				count, idCount,
			)
		}
	}

	if count == 0 {
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	}

	// Build merged children: for each iteration k, combine bindings from all groups.
	mergedChildren := make([]*captureContext, count)
	for k := range count {
		merged := &captureContext{
			bindings: make(map[string]syntax.SyntaxValue),
			children: make(map[int][]*captureContext),
		}
		for _, id := range matchingIDs {
			child := ctx.children[id][k]
			maps.Copy(merged.bindings, child.bindings)
			for cid, cchildren := range child.children {
				merged.children[cid] = append(merged.children[cid], cchildren...)
			}
		}
		mergedChildren[k] = merged
	}

	// Add all matching IDs to the exclude set for recursive expansion.
	innerExclude := make(map[int]struct{}, len(excludeEllipsisIDs)+len(matchingIDs))
	for id := range excludeEllipsisIDs {
		innerExclude[id] = struct{}{}
	}
	for _, id := range matchingIDs {
		innerExclude[id] = struct{}{}
	}

	results, err := p.expandEllipsisChildren(pattern, mergedChildren, ellipsisVars, innerExclude, opts, patternVarsInTemplate)
	if err != nil {
		return nil, err
	}

	return p.combineEllipsisResults(results, rest, ctx, ellipsisVars, excludeEllipsisIDs, opts, pattern)
}

// expandEllipsisChildren expands the template pattern once for each child context,
// returning the list of expanded results.
func (p *SyntaxMatcher) expandEllipsisChildren(
	pattern syntax.SyntaxValue,
	children []*captureContext,
	ellipsisVars map[string]struct{},
	excludeEllipsisIDs map[int]struct{},
	opts *ExpandOptions,
	patternVarsInTemplate map[string]struct{},
) ([]syntax.SyntaxValue, error) {
	results := make([]syntax.SyntaxValue, 0, len(children))
	for _, childCtx := range children {
		newEllipsisVars := make(map[string]struct{})
		maps.Copy(newEllipsisVars, ellipsisVars)
		for v := range patternVarsInTemplate {
			newEllipsisVars[v] = struct{}{}
		}

		expanded, err := p.expandSyntaxValue(pattern, childCtx, newEllipsisVars, excludeEllipsisIDs, opts)
		if err != nil {
			return nil, err
		}
		results = append(results, expanded)
	}
	return results, nil
}

// combineEllipsisResults expands the rest of the template and prepends
// the ellipsis-expanded results.
func (p *SyntaxMatcher) combineEllipsisResults(
	results []syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars map[string]struct{},
	excludeEllipsisIDs map[int]struct{},
	opts *ExpandOptions,
	pattern syntax.SyntaxValue,
) (syntax.SyntaxValue, error) {
	expandedRest, err := p.expandSyntaxValue(rest, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	if err != nil {
		return nil, err
	}

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
