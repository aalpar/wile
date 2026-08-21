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
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ExpandOptions holds the hygiene and source-tracking parameters for template expansion.
// All fields are optional; the zero value expands without hygiene (useful for testing).
//
// Whether a template symbol is replaced by a capture is decided by
// TemplateDenotesPatternVariable over PatternVarSyntax; see that function for
// the rule and for why plain subset resolution is sufficient.
type ExpandOptions struct {
	// IntroScope is the hygiene scope added to template-introduced symbols. It is
	// not added to newly created pairs or vectors (those carry only a source
	// context), nor to symbols preserved from pattern variable substitution, nor
	// to symbols resolved to a definition-site local binding.
	IntroScope *syntax.Scope

	// FreeIds maps free identifiers to their pre-resolved bindings. A non-nil
	// value carries resolved binding info from macro definition time (local
	// scopes, global index, library scope). A nil value is treated the same as an
	// absent key — the identifier receives the intro scope normally.
	//
	// Keyed by FreeIdKey (name + scope fingerprint), NOT by bare name: one clause
	// template can hold two same-named free identifiers under different scope sets
	// (a macro-generating macro splices the name in from two scope sources), and
	// each resolves to its own binding. A bare-name key let the second collapse
	// onto the first. The collector writes each occurrence under its own key; this
	// map reads back under the template symbol's own scope set at expansion.
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

// FreeIdKey is the map key for a free identifier's pre-resolved binding in
// ExpandOptions.FreeIds and its compile-time source. It discriminates on the
// scope set as well as the name, so two same-named template identifiers scoped
// differently keep separate resolutions. The producer (collectFreeIdentifiers)
// and this consumer must build the key identically.
//
// The fingerprint contains only [0-9,], so the first '|' unambiguously ends it —
// the name that follows may itself contain '|' without colliding with another
// (name, scopes) pair.
func FreeIdKey(name string, scopes []*syntax.Scope) string {
	return syntax.ScopeFingerprint(scopes) + "|" + name
}

// FreeIdName recovers the bare identifier from a FreeIdKey — the inverse of FreeIdKey.
// The fingerprint contains only [0-9,], so the first '|' unambiguously ends it (a name may
// itself contain '|'). Callers that indexed a freeIds map get the composite key, not the bare
// name; use this to compare against a symbol's own name (e.g. a template's self-reference).
func FreeIdName(key string) string {
	_, name, found := strings.Cut(key, "|")
	if !found {
		return key
	}
	return name
}

// resolveSourceContext returns the source context for a newly-constructed
// structural node derived from template t. The template's context is the
// default; UseSiteCtx overrides it (so error messages point at the invocation
// site rather than the macro definition); Origin is stamped on top so the
// trace records which macro produced the node — see ExpandOptions.Origin for
// why "last caller wins" is correct.
//
// The variants in applyHygieneToSymbol, combineEllipsisResults, and
// capturedValueToSyntax handle different base contexts or nil-bases and are
// deliberately kept separate.
func (opts *ExpandOptions) resolveSourceContext(t syntax.SyntaxValue) *syntax.SourceContext {
	srcCtx := t.SourceContext()
	if opts.UseSiteCtx != nil {
		srcCtx = opts.UseSiteCtx
	}
	if opts.Origin != nil {
		srcCtx = srcCtx.WithOrigin(opts.Origin)
	}
	return srcCtx
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
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
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
			if carSym.Key() == p.ellipsisID {
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
				if sym.Key() == p.ellipsisID {
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

		return syntax.NewSyntaxCons(expandedCar, expandedCdr, opts.resolveSourceContext(t)), nil

	case *syntax.SyntaxVector:
		return p.expandVectorTemplate(t, ctx, ellipsisVars, excludeEllipsisIDs, opts)

	case *syntax.SyntaxBox:
		// A box is a container, like a vector: `#&<template>` holds a template,
		// not a datum, so its content is expanded (pkg/syntax/syntax_box.go's
		// own contract: "a box in a macro template propagates scopes to what it
		// holds"). Falling into the self-evaluating arm below emitted the whole
		// boxed subtree verbatim, ellipses included — `#&x` stayed `#&x` while
		// the vector analogue `#(x x)` already gave `#(5 5)`.
		//
		// Routing through expandSyntaxValue rather than element-wise is what
		// makes `#&(x ...)` repeat: the ellipsis follows the content's first
		// element, so only the pair walk can see it.
		if t.Value == nil {
			return t, nil
		}
		inner, err := p.expandSyntaxValue(t.Value, ctx, ellipsisVars, excludeEllipsisIDs, opts)
		if err != nil {
			return nil, err
		}
		return syntax.NewSyntaxBox(inner, opts.resolveSourceContext(t)), nil

	default:
		// Self-evaluating values - return as-is
		return template, nil
	}
}

// expandVectorTemplate expands a vector template.
//
// R7RS §4.3.2: a vector template's elements obey the same rules as a list
// template's, ellipsis included — `#(x ...)` must repeat `x` just as `(x ...)`
// does. Element-wise expansion cannot see an ellipsis, which follows an element
// rather than containing it, so expansion routes through the equivalent pair
// chain and re-vectorizes the result.
func (p *SyntaxMatcher) expandVectorTemplate(
	t *syntax.SyntaxVector,
	ctx *captureContext,
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	srcCtx := opts.resolveSourceContext(t)
	if len(t.Values) == 0 {
		return syntax.NewSyntaxVector(srcCtx), nil
	}

	chain := vectorElementsToPairChain(t)
	expanded, err := p.expandSyntaxValue(chain, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	if err != nil {
		return nil, err
	}
	elements, err := syntaxListToSlice(expanded)
	if err != nil {
		return nil, err
	}
	return syntax.NewSyntaxVector(srcCtx, elements...), nil
}

// syntaxListToSlice flattens a proper syntax list into its elements. Used to
// re-vectorize a vector template that was expanded through its pair chain.
func syntaxListToSlice(v syntax.SyntaxValue) ([]syntax.SyntaxValue, error) {
	var q []syntax.SyntaxValue
	for cur := v; ; {
		// The terminator is the shared empty-list singleton, which is a
		// SyntaxTuple rather than a *SyntaxPair — test it before the assertion.
		if syntax.IsSyntaxEmptyList(cur) {
			return q, nil
		}
		pr, ok := cur.(*syntax.SyntaxPair)
		if !ok {
			return nil, werr.WrapForeignErrorf(
				werr.ErrExpansion,
				"syntaxListToSlice: vector template expanded to an improper list",
			)
		}
		q = append(q, pr.SyntaxCar())
		cur = pr.SyntaxCdr()
	}
}

// TemplateDenotesPatternVariable reports whether a template occurrence denotes
// the pattern variable a capture was recorded under, and may therefore be
// replaced by it.
//
// **One caller: the runtime expander in this file.** It used to gate both
// template paths, but the compile-time emit in compileSyntaxTemplateToOps now
// asks an ordinary scoped GetLocalIndex instead — pattern variables are bound at
// their pattern identifier's scopes, so resolution answers this question without
// a separate predicate. This path cannot follow, and the reason is structural
// rather than pending work: captureContext.bindings is a map[string]SyntaxValue
// filled by the match VM from pattern-variable NAMES in the compiled pattern
// bytecode, so there is no environment frame here and no *Binding to compare.
// Making it resolution-driven means re-keying the capture context and the
// bytecode by scope set — a redesign of this package's core, not a query change.
//
// The pattern variable is the BINDER and the template occurrence is a REFERENCE
// to it, so the rule is Flatt's resolution relation and nothing else:
//
//	patternScopes ⊆ templateScopes
//
// It is ordinary subset resolution because use-site scopes make it sufficient.
// Every macro invocation stamps a fresh scope on its input form and never
// removes it (compilation.newUseSiteScope), so use-site syntax wears a scope
// macro-introduced syntax does not. An identifier an OUTER macro introduced
// therefore lacks the use-site scope the inner macro's pattern variable carries,
// is not a superset of it, and the capture `(outer inner (_ x))` — outer's
// template saying `(list x)` — is refused here rather than by any extra
// condition. R7RS §4.3, Chez and Racket agree that `x` denotes outer's
// definition-site binding.
//
// Before use-site scopes this predicate carried a ceiling as well, naming the
// scopes binding forms inside the clause body had minted, because bare subset
// admitted that capture and set equality refused a `(syntax ...)` written under
// a `let` in a clause body. Both endpoints are now handled by the floor alone:
// the body binder's scope is on the template and absent from the pattern, which
// subset permits, while the outer macro's introduction leaves the pattern's
// use-site scope off the template, which subset refuses. The ceiling, and the
// binder-scope log that fed it, are gone — see
// plans/2026-08-20-use-site-scopes-impl.local.md.
func TemplateDenotesPatternVariable(templateScopes, patternScopes []*syntax.Scope) bool {
	return syntax.ScopesMatch(templateScopes, patternScopes)
}

// applyHygieneToSymbol applies hygiene transformations to a template symbol.
// This handles free identifiers and intro scope for non-pattern-variable symbols.
func (p *SyntaxMatcher) applyHygieneToSymbol(
	sym *syntax.SyntaxSymbol,
	opts *ExpandOptions,
) syntax.SyntaxValue {
	key := sym.Key()

	srcCtx := sym.SourceContext()
	if opts.UseSiteCtx != nil {
		srcCtx = opts.UseSiteCtx
	}
	if opts.Origin != nil && srcCtx != nil {
		srcCtx = srcCtx.WithOrigin(opts.Origin)
	} else if opts.Origin != nil {
		srcCtx = &syntax.SourceContext{Origin: opts.Origin}
	}

	// A template identifier carries the scopes it had at the macro's DEFINITION
	// site (Flatt 2016). UseSiteCtx overrides srcCtx for diagnostics — position
	// and origin — but its scope set belongs to the invoking code, and letting
	// that reach a template identifier would let the macro capture use-site
	// bindings.
	//
	// Clearing the set outright defends against that, but also discards the
	// definition-site scopes, leaving a template reference unable to name
	// anything bound in the macro's own lexical context: a sibling
	// define-syntax in the same let body resolves to a binder keyed on that
	// let's scope, which a scope-less reference is not a superset of.
	// Substituting the definition-site set defends against the use site without
	// that collateral loss. The intro scope added below is what keeps the
	// identifier distinct from a same-named one at the use site.
	templateCtx := srcCtx
	if opts.UseSiteCtx != nil && srcCtx != nil {
		var defScopes []*syntax.Scope
		defCtx := sym.SourceContext()
		if defCtx != nil {
			defScopes = defCtx.Scopes
		}
		// Pointer-wise comparison — scopes have identity, not structure. Skips the
		// clone on the common case where the two sets already agree (both empty).
		if !slices.Equal(srcCtx.Scopes, defScopes) {
			templateCtx = srcCtx.Clone()
			templateCtx.Scopes = defScopes
		}
	}

	var isFree bool
	var resolution FreeIdResolver
	if opts.FreeIds != nil {
		// The template symbol's OWN scope set — the definition-site scopes the
		// collector resolved and keyed under (sym.Scopes() reads the same
		// immutable SourceContext.Scopes at both points). UseSiteCtx above rebuilds
		// templateCtx for diagnostics but must not perturb this key.
		resolution, isFree = opts.FreeIds[FreeIdKey(key, sym.Scopes())]
	}

	// Not a free identifier (or no resolver) — apply intro scope and return.
	if !isFree || resolution == nil {
		newSym := syntax.NewSyntaxSymbol(key, templateCtx)
		if opts.IntroScope != nil {
			newSym = newSym.AddScope(opts.IntroScope).(*syntax.SyntaxSymbol)
		}
		return newSym
	}

	// Local binding — use definition-site scopes.
	localScopes := resolution.GetLocalScopes()
	if len(localScopes) > 0 {
		scopedCtx := func() *syntax.SourceContext {
			if srcCtx != nil {
				return srcCtx.Clone()
			}
			return &syntax.SourceContext{}
		}()
		scopedCtx.Scopes = localScopes
		return syntax.NewSyntaxSymbol(key, scopedCtx)
	}

	// Global binding — carry the intro scope (as the non-free path does above)
	// so this reference shares a scope with a binding co-introduced by the same
	// template; CompileSymbol's scope-set local match (GetLocalIndex) can then
	// pair them, letting the introduced binder shadow the global. The recorded
	// global / library scope is only a fallback, consulted AFTER that local match
	// (see CompileSymbol; Change 2). Previously this reference lacked the intro
	// scope, so it could not match the co-introduced binder.
	// See plans/2026-06-15-macro-hygiene-global-shadow-fix.local.md (Change 1).
	globalBinding := resolution.GetGlobal()
	if globalBinding != nil {
		newSym := syntax.NewSyntaxSymbol(key, templateCtx)
		if opts.IntroScope != nil {
			newSym = newSym.AddScope(opts.IntroScope).(*syntax.SyntaxSymbol)
		}
		// The library scope and the resolved binding are CUMULATIVE, not
		// alternatives. Returning early with only the scope (which is what this did)
		// threw away the *GlobalIndex that collectFreeIdentifiers had already
		// resolved at macro-definition time, and it broke R7RS §4.3.2 referential
		// transparency for every library macro whose template names something the
		// library does not itself define.
		//
		// The scope alone can only redirect into the LIBRARY's env
		// (GetGlobalIndexFromLibraryScopes). A core primitive is not there — it lives
		// in the sealed base — so that lookup missed, CompileSymbol fell through to
		// the use-site global, and a user's top-level (define car 42) captured the
		// template's `car`. Shipped stdlib was reachable this way: a top-level
		// (define %prompt-reinstall …) broke (wile control)'s reset/shift.
		//
		// Keeping both makes the pin the primary resolution (CompileSymbol consults
		// it before the library scope) and leaves the scope as the fallback for a
		// free identifier that was NOT resolvable at definition time. The pin stays
		// BELOW the scope-set local match, so a binder co-introduced by the same
		// template still shadows it (the R1 fix, plans/2026-06-15-macro-hygiene-...).
		libScope := resolution.GetLibraryScope()
		if libScope != nil {
			newSym = newSym.AddScope(libScope).(*syntax.SyntaxSymbol)
		}
		return newSym.WithResolvedBinding(globalBinding)
	}

	// Local binding whose binder carries NO scopes — a pattern variable in a
	// syntax-case clause body's frame is bound that way. There is no scope set
	// to substitute, so this arm keeps the template symbol scope-less; what it
	// must not do is hand it srcCtx, which UseSiteCtx has already replaced with
	// the INVOKING code's context. That let a syntax-rules macro defined inside
	// a syntax-case clause body resolve a free template reference to the use
	// site's binding: 99 where hygiene requires 42 (Racket gives 42), and the
	// sibling local-binding arm above gives 42 for the identical shape.
	//
	// templateCtx differs from srcCtx only in its scope set (definition-site
	// rather than use-site); position and origin are the same object.
	if resolution.GetHasLocalBinding() {
		return syntax.NewSyntaxSymbol(key, templateCtx)
	}

	// Resolution non-nil but all methods returned zero — binding was unresolvable
	// at definition time. Fall through to intro scope.
	newSym := syntax.NewSyntaxSymbol(key, templateCtx)
	if opts.IntroScope != nil {
		newSym = newSym.AddScope(opts.IntroScope).(*syntax.SyntaxSymbol)
	}
	return newSym
}

// lookupCapturedBinding resolves a pattern variable by walking the capture
// context's ancestor chain from innermost to root. This is what lets a
// lower-depth (e.g. depth-0) pattern variable be broadcast into a deeper
// ellipsis sub-template (R7RS §4.3.2): the per-iteration child context holds
// only its own ellipsis captures, while the broadcast variable lives in an
// ancestor. Composes to arbitrary nesting depth because the chain is built
// one link per ellipsis level during matching.
func lookupCapturedBinding(ctx *captureContext, key string) (syntax.SyntaxValue, bool) {
	for c := ctx; c != nil; c = c.parent {
		v, ok := c.bindings[key]
		if ok {
			return v, true
		}
	}
	return nil, false
}

// expandSymbol handles symbol expansion for both normal and escaped template contexts.
// It checks pattern variable bindings, scope compatibility, and applies hygiene.
func (p *SyntaxMatcher) expandSymbol(
	t *syntax.SyntaxSymbol,
	ctx *captureContext,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	key := t.Key()

	// Walk the capture-context chain: a per-iteration child context holds only
	// the bindings captured under its own ellipsis, but a lower-depth pattern
	// variable referenced inside the ellipsis (R7RS §4.3.2 broadcast) lives in
	// an ancestor. Innermost binding wins; pattern variables are unique per
	// clause, so there is no shadowing across levels.
	capturedVal, ok := lookupCapturedBinding(ctx, key)
	if !ok {
		return p.applyHygieneToSymbol(t, opts), nil
	}

	// Captured under this name, but this occurrence does not denote that pattern
	// variable — an outer expansion introduced it. Bail to hygiene, which resolves
	// it at its own definition site (R7RS §4.3).
	patternSym, hasPattern := opts.PatternVarSyntax[key]
	if hasPattern && !TemplateDenotesPatternVariable(t.Scopes(), patternSym.Scopes()) {
		return p.applyHygieneToSymbol(t, opts), nil
	}

	patternVarSubstitutions.Add(1)
	return p.capturedValueToSyntax(capturedVal, opts)
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
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	opts *ExpandOptions,
) (syntax.SyntaxValue, error) {
	// Find which variables in the pattern are bound in child contexts
	patternVarsInTemplate := p.findSyntaxPatternVariables(pattern)

	// Find ALL ellipsis IDs that contribute variables to this template,
	// excluding any IDs already consumed by an outer expansion level.
	matchingIDs := p.matcher.findMatchingEllipsisIDs(patternVarsInTemplate, excludeEllipsisIDs)
	if len(matchingIDs) == 0 {
		if len(patternVarsInTemplate) > 0 {
			// Pattern variables exist but all their ellipsis IDs were excluded
			// by outer expansion levels. This indicates a bug in depth tracking
			// or exclude-set propagation.
			return nil, werr.WrapForeignErrorf(
				werr.ErrExpansion,
				"expandSyntaxEllipsis: pattern variables found but all ellipsis IDs excluded",
			)
		}
		// No pattern variables at all — constant template followed by `...`.
		// Repeating it zero times (dropping it) is correct per R7RS §4.3.2.
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
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	opts *ExpandOptions,
	patternVarsInTemplate values.StringSet,
	ellipsisID int,
) (syntax.SyntaxValue, error) {
	children := ctx.children[ellipsisID]
	if len(children) == 0 {
		return p.expandSyntaxValue(rest, ctx, ellipsisVars, excludeEllipsisIDs, opts)
	}

	// Add this ellipsis ID to the exclude set so nested ellipsis expansions
	// within the children select the inner ID instead of re-selecting this one.
	innerExclude := values.NewIntSet(len(excludeEllipsisIDs) + 1)
	for id := range excludeEllipsisIDs {
		innerExclude.Set(id)
	}
	innerExclude.Set(ellipsisID)

	results, err := p.expandEllipsisChildren(pattern, children, ellipsisVars, innerExclude, opts, patternVarsInTemplate)
	if err != nil {
		return nil, err
	}

	return p.combineEllipsisResults(results, rest, ctx, ellipsisVars, excludeEllipsisIDs, pattern, opts)
}

// expandEllipsisCrossGroup handles cross-group zipping where template variables
// come from multiple ellipsis groups that must iterate in lockstep.
func (p *SyntaxMatcher) expandEllipsisCrossGroup(
	pattern syntax.SyntaxValue,
	rest syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	opts *ExpandOptions,
	patternVarsInTemplate values.StringSet,
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
			// Lower-depth (broadcast) variables live above the zipped groups,
			// in the shared parent context — preserve the chain to them.
			parent: ctx,
		}
		for _, id := range matchingIDs {
			child := ctx.children[id][k]
			maps.Copy(merged.bindings, child.bindings)
			for cid, cchildren := range child.children {
				// Reparent grandchildren onto merged so the tree/parent
				// invariant (c ∈ n.children ⟹ c.parent == n) holds here too:
				// a lookup walking up from a grandchild then sees the zipped
				// bindings merged at this level, not only its original group's.
				// Each grandchild belongs to exactly one (group, k) and is
				// copied into exactly one merged context, so this never aliases.
				for _, gc := range cchildren {
					gc.parent = merged
				}
				merged.children[cid] = append(merged.children[cid], cchildren...)
			}
		}
		mergedChildren[k] = merged
	}

	// Add all matching IDs to the exclude set for recursive expansion.
	innerExclude := values.NewIntSet(len(excludeEllipsisIDs) + len(matchingIDs))
	for id := range excludeEllipsisIDs {
		innerExclude.Set(id)
	}
	for _, id := range matchingIDs {
		innerExclude.Set(id)
	}

	results, err := p.expandEllipsisChildren(pattern, mergedChildren, ellipsisVars, innerExclude, opts, patternVarsInTemplate)
	if err != nil {
		return nil, err
	}

	return p.combineEllipsisResults(results, rest, ctx, ellipsisVars, excludeEllipsisIDs, pattern, opts)
}

// expandEllipsisChildren expands the template pattern once for each child context,
// returning the list of expanded results.
func (p *SyntaxMatcher) expandEllipsisChildren(
	pattern syntax.SyntaxValue,
	children []*captureContext,
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	opts *ExpandOptions,
	patternVarsInTemplate values.StringSet,
) ([]syntax.SyntaxValue, error) {
	results := make([]syntax.SyntaxValue, 0, len(children))
	for _, childCtx := range children {
		newEllipsisVars := values.NewStringSet(0)
		maps.Copy(newEllipsisVars, ellipsisVars)
		for v := range patternVarsInTemplate {
			newEllipsisVars.Set(v)
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
	ellipsisVars values.StringSet,
	excludeEllipsisIDs values.IntSet,
	pattern syntax.SyntaxValue,
	opts *ExpandOptions,
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
	for i := range slices.Backward(results) {
		result = syntax.NewSyntaxCons(results[i], result, srcCtx)
	}

	return result, nil
}

// findSyntaxPatternVariables finds all pattern variables in a syntax template.
func (p *SyntaxMatcher) findSyntaxPatternVariables(template syntax.SyntaxValue) values.StringSet {
	vars := values.NewStringSet(0)
	p.findSyntaxVarsRecursive(template, vars)
	return vars
}

func (p *SyntaxMatcher) findSyntaxVarsRecursive(template syntax.SyntaxValue, vars values.StringSet) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		key := t.Key()
		ok := p.matcher.variables.Get(key)
		if ok {
			vars.Set(key)
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
	case *syntax.SyntaxBox:
		if t.Value != nil {
			p.findSyntaxVarsRecursive(t.Value, vars)
		}
	}
}

// expandEscapedSyntaxTemplate expands a template inside an ellipsis escape form at the syntax level.
//
//nolint:unparam
func (p *SyntaxMatcher) expandEscapedSyntaxTemplate(
	template syntax.SyntaxValue,
	ctx *captureContext,
	ellipsisVars values.StringSet,
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
		return syntax.NewSyntaxCons(car, cdr, opts.resolveSourceContext(t)), nil

	case *syntax.SyntaxVector:
		// R7RS §4.3.2: `(... <template>)` suppresses the ellipsis's special
		// meaning, not pattern-variable substitution — and a vector is a
		// structured template like a pair, so the escape must descend into it or
		// its variables are left standing as literal symbols.
		elements := make([]syntax.SyntaxValue, len(t.Values))
		for i, elem := range t.Values {
			expanded, err := p.expandEscapedSyntaxTemplate(elem, ctx, ellipsisVars, opts)
			if err != nil {
				return nil, err
			}
			elements[i] = expanded
		}
		return syntax.NewSyntaxVector(opts.resolveSourceContext(t), elements...), nil

	case *syntax.SyntaxBox:
		// Same reason as the vector arm: a box is a structured template, so the
		// escape descends into it rather than leaving its variables standing.
		if t.Value == nil {
			return t, nil
		}
		inner, err := p.expandEscapedSyntaxTemplate(t.Value, ctx, ellipsisVars, opts)
		if err != nil {
			return nil, err
		}
		return syntax.NewSyntaxBox(inner, opts.resolveSourceContext(t)), nil

	default:
		return template, nil
	}
}
