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

package compilation

// compile_syntax_rules.go implements R7RS syntax-rules macro compilation.
//
// This file is part of the macro system's compile-time phase. It transforms
// (syntax-rules (literal ...) (pattern template) ...) forms into transformer
// closures that can be invoked during macro expansion.
//
// Design: The macro system uses a layered architecture (see DESIGN.md):
//   1. Pattern Matching VM (match/) - unhygienic core engine
//   2. Syntax Adapter Layer - bridges syntax objects and raw values
//   3. Hygienic Layer - scope management per Flatt's "sets of scopes" model
//
// This file handles layer 2: compiling syntax-rules into a transformer closure.
// The transformer is stored in the environment with BindingTypeSyntax, allowing
// the expander to recognize and invoke it during macro expansion.
//
// Reference: R7RS Section 4.3.2 (syntax-rules)

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// FreeIdResolution stores the resolution info for a free identifier in a macro template.
// Free identifiers can be resolved to either global bindings (via GlobalIndex) or
// local bindings (via their scope set at macro definition time).
// FreeIdResolution, SyntaxRulesClause, and ClausesWrapper are defined in
// machine/syntax_bridge_types.go (shared between compiler and runtime).

// CompileSyntaxRules compiles a syntax-rules form into a transformer procedure.
//
// R7RS Forms:
//
//	(syntax-rules (literal ...) (pattern template) ...)
//	(syntax-rules <ellipsis> (literal ...) (pattern template) ...)  ; custom ellipsis
//
// The compilation process:
//  1. Parse optional custom ellipsis identifier
//  2. Parse the literals list - these symbols are matched literally, not as variables
//  3. For each clause, identify pattern variables (symbols not in literals list)
//  4. Compile each pattern to bytecode (see match/syntax_compiler.go)
//  5. Create a machine.MachineClosure that, when invoked:
//     - Tries each pattern in order against the input form
//     - On first match, expands the template with captured bindings
//     - Adds an "intro scope" to the expansion for hygiene
//
// The returned closure is stored in the environment with BindingTypeSyntax,
// allowing the expander to recognize it as a macro transformer.
func CompileSyntaxRules(ctx context.Context, env *environment.EnvironmentFrame, syntaxRulesForm syntax.SyntaxValue, libraryScope *syntax.Scope) (*machine.MachineClosure, error) {
	// syntaxRulesForm should be (syntax-rules (literals...) clause1 clause2 ...)
	// or (syntax-rules <ellipsis> (literals...) clause1 clause2 ...)
	src := syntaxRulesForm.SourceContext()
	formPair, ok := syntaxRulesForm.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrNotAList, "syntax-rules: expected a list"))
	}

	// Skip 'syntax-rules' keyword
	cdr := formPair.SyntaxCdr()
	if cdr == nil {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing literals list and clauses"))
	}

	argsPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: expected literals list and clauses"))
	}

	// Check for optional custom ellipsis identifier
	// R7RS §4.3.2: (syntax-rules <ellipsis> (literal ...) clause ...)
	ellipsis := match.DefaultEllipsis
	firstArg := argsPair.SyntaxCar()
	if firstArg == nil {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing literals list"))
	}

	var literalsStx syntax.SyntaxValue
	var clausesCdr syntax.SyntaxValue

	// If firstArg is a symbol, it's a custom ellipsis identifier
	sym, ok := firstArg.(*syntax.SyntaxSymbol)
	if ok {
		symVal := sym.Unwrap()
		symValSym, ok := symVal.(*values.Symbol)
		if ok {
			ellipsis = symValSym.Key
		}

		// Move to next element for literals list
		nextCdr := argsPair.SyntaxCdr()
		if nextCdr == nil {
			return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing literals list after ellipsis identifier"))
		}
		nextPair, ok := nextCdr.(*syntax.SyntaxPair)
		if !ok {
			return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: expected literals list after ellipsis identifier"))
		}

		literalsStx = nextPair.SyntaxCar()
		clausesCdr = nextPair.SyntaxCdr()
	} else {
		// No custom ellipsis, firstArg is the literals list
		literalsStx = firstArg
		clausesCdr = argsPair.SyntaxCdr()
	}

	if literalsStx == nil {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing literals list"))
	}

	literals := make(map[string]struct{})
	literalSyntax := make(map[string]*syntax.SyntaxSymbol)

	// Process literals list
	literalsList, ok := literalsStx.(*syntax.SyntaxPair)
	if ok && !syntax.IsSyntaxEmptyList(literalsList) {
		err := extractLiteralsWithSyntax(ctx, literalsList, literals, literalSyntax, ellipsis)
		if err != nil {
			return nil, wrapSourcedError(src, werr.WrapForeignErrorf(err, "syntax-rules: invalid literals list"))
		}
	}
	// Empty literals list is also valid

	// R7RS §4.3.2: If ellipsis appears in literals, it becomes a literal
	// and ellipsis functionality is disabled for this syntax-rules form.
	// Use "\x00" as sentinel since it cannot appear in Scheme symbols and
	// won't be replaced with default by the match package.
	_, ellipsisIsLiteral := literals[ellipsis]
	if ellipsisIsLiteral {
		ellipsis = "\x00"
	}

	// Process clauses
	if clausesCdr == nil {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: no clauses provided"))
	}

	clausesList, ok := clausesCdr.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrNotAList, "syntax-rules: expected clause list"))
	}

	// Compile each clause
	var clauses []*SyntaxRulesClause
	v, err := clausesList.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, clause syntax.SyntaxValue) error {
		clauseSrc := clause.SourceContext()
		clausePair, ok := clause.(*syntax.SyntaxPair)
		if !ok {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(werr.ErrNotAList, "syntax-rules: clause must be a list"))
		}

		// Extract pattern and template
		pattern := clausePair.SyntaxCar()
		if pattern == nil {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing pattern in clause"))
		}

		cdrVal := clausePair.SyntaxCdr()
		if cdrVal == nil {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing template in clause"))
		}
		templateCdr, ok := cdrVal.(*syntax.SyntaxPair)
		if !ok {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(werr.ErrNotAList, "syntax-rules: template must be in a list"))
		}

		template := templateCdr.SyntaxCar()
		if template == nil {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: missing template in clause"))
		}

		// Compile the pattern with custom ellipsis
		// Pass env so free identifiers can be resolved to their definition-time bindings
		// Pass literalSyntax for scope-aware literal matching (hygiene)
		compiledClause, err := compileClauseWithEllipsisAndLiterals(ctx, env, pattern, template, literals, literalSyntax, ellipsis, libraryScope)
		if err != nil {
			return wrapSourcedError(clauseSrc, werr.WrapForeignErrorf(err, "syntax-rules: error compiling clause"))
		}

		clauses = append(clauses, compiledClause)
		return nil
	})
	if err != nil {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(err, "syntax-rules: error compiling clause"))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrNotAList, "syntax-rules: expected proper list of clauses"))
	}

	if len(clauses) == 0 {
		return nil, wrapSourcedError(src, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-rules: no valid clauses"))
	}

	// Create transformer closure
	return createTransformerClosure(env, clauses, literals)
}

// compileClauseWithEllipsisAndLiterals compiles a single pattern-template pair with
// custom ellipsis and literal syntax for hygiene.
// The env parameter is used to resolve free identifiers to their definition-time bindings.
// The literalSyntax parameter stores syntax symbols for scope-aware literal matching.
func compileClauseWithEllipsisAndLiterals(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pattern, template syntax.SyntaxValue,
	literals map[string]struct{},
	literalSyntax map[string]*syntax.SyntaxSymbol,
	ellipsis string,
	libraryScope *syntax.Scope,
) (*SyntaxRulesClause, error) {
	// Determine pattern variables (anything not a literal, keyword, or ellipsis)
	// Use literalSyntax for scope-aware literal matching (R7RS bound-identifier=? semantics)
	// Also collect pattern variable syntax for nested macro hygiene
	variables := make(map[string]struct{})
	varSyntax := make(map[string]*syntax.SyntaxSymbol)
	err := collectPatternVariablesWithEllipsis(pattern, literalSyntax, true, variables, varSyntax, ellipsis)
	if err != nil {
		return nil, err
	}

	// R7RS §4.3.2 diagnostic: reject, at definition time, a template that follows
	// a pattern variable with more ellipses than its pattern depth (e.g. `(list a
	// ...)` where `a` is bound at depth 0). Such an ellipsis sub-template has no
	// captured sequence to iterate; without this check it is accepted here and
	// fails only at expansion with the opaque internal error "all ellipsis IDs
	// excluded". Validating now yields a clean, located CompilationError.
	patternDepths := make(map[string]int)
	computePatternVarDepths(pattern, variables, ellipsis, 0, patternDepths)
	err = validateTemplateEllipsisDepth(template, variables, patternDepths, ellipsis, 0)
	if err != nil {
		return nil, err
	}

	// Compile pattern to bytecode with ellipsis variable mapping and literals
	// Literals are needed so the compiler knows to match _ literally if it's in the literals list
	compiled, err := match.CompileSyntaxPattern(ctx, pattern, variables, &match.CompilePatternOpts{
		Literals:   literals,
		EllipsisID: ellipsis,
	})
	if err != nil {
		return nil, err
	}

	// Create matcher with ellipsis variable mapping, custom ellipsis, and literal syntax
	// The literalSyntax enables scope-aware matching for auxiliary syntax hygiene
	matcher := match.NewSyntaxMatcher(variables, compiled.Codes, &match.SyntaxMatcherOpts{
		EllipsisVars:   compiled.EllipsisVars,
		EllipsisDepths: compiled.EllipsisDepths,
		EllipsisID:     ellipsis,
		LiteralSyntax:  literalSyntax,
	})

	// Collect free identifiers from template (identifiers that are NOT pattern variables)
	// These should NOT get the intro scope during expansion, so they can resolve
	// to bindings outside the macro (including recursive references to the macro itself)
	// Resolve each free identifier to its definition-time binding:
	// - For global bindings: store GlobalIndex for cross-library hygiene
	// - For local bindings: store scopes so the identifier resolves to definition-time binding
	freeIds := make(map[string]*FreeIdResolution)
	collectFreeIdentifiersWithEllipsis(env, template, variables, freeIds, ellipsis, libraryScope)

	return &SyntaxRulesClause{
		Template:         template,
		Bytecode:         compiled.Codes,
		Matcher:          matcher,
		PatternVars:      variables,
		PatternVarSyntax: varSyntax,
		EllipsisVars:     compiled.EllipsisVars,
		FreeIds:          freeIds,
		Ellipsis:         ellipsis,
		LiteralSyntax:    literalSyntax,
	}, nil
}

// collectFreeIdentifiersWithEllipsis walks the template and collects all
// identifiers that are NOT pattern variables, using a custom ellipsis
// identifier. These "free identifiers" refer to bindings outside the macro and
// should NOT get the intro scope during expansion.
//
// This is critical for recursive macros: the macro's own name (e.g., "and" in
// (and test2 ...)) must resolve to the macro's binding, not get an intro scope
// that would break the lookup.
//
// The env parameter resolves each free identifier to its definition-time
// binding, either:
//   - LocalScopes (for local bindings) - enables hygiene for let-syntax capturing local vars
//   - GlobalIndex (for global bindings) - enables cross-library macro hygiene
//
// Each resolution is stored under match.FreeIdKey (name + scope fingerprint), not
// the bare name: one template can hold two same-named free identifiers under
// different scope sets — a macro-generating macro splices the name in from two
// scope sources — and each already resolves individually here (both arms key on
// t.Scopes()). A bare-name key let the second write clobber the first, so at
// expansion both occurrences received the surviving resolution. The consumer
// (syntax_expand.go applyHygieneToSymbol) reads back under the same key.
func collectFreeIdentifiersWithEllipsis(env *environment.EnvironmentFrame, template syntax.SyntaxValue, patternVars map[string]struct{}, freeIds map[string]*FreeIdResolution, ellipsis string, libraryScope *syntax.Scope) {
	switch t := template.(type) {
	case *syntax.SyntaxSymbol:
		sym := t.Unwrap()
		symVal, ok := sym.(*values.Symbol)
		if ok {
			// Skip ellipsis marker
			if symVal.Key == ellipsis {
				return
			}
			// If it's not a pattern variable, it's a free identifier
			_, isPatternVar := patternVars[symVal.Key]
			if !isPatternVar {
				// Resolve the free identifier to its definition-time binding.
				// Check local binding first (for let-syntax hygiene).
				//
				// The reference's own scope set decides WHICH same-named binding,
				// for the same reason it does on the global arm below: two
				// expansions of a macro-generating macro each bind under their own
				// intro scope, so the name owns two slots. Passing nil here means
				// MATCH ANY (resolveLocal), which takes the first live slot and
				// hands both generated macros the first expansion's binder.
				li := env.GetLocalIndex(symVal, t.Scopes())
				if li != nil {
					binding := env.GetLocalBinding(li)
					if binding != nil {
						// Local binding found - store scopes for hygiene
						// Set HasLocalBinding=true even if scopes are empty,
						// so expansion knows not to add intro scope.
						freeIds[match.FreeIdKey(symVal.Key, t.Scopes())] = &FreeIdResolution{
							LocalScopes:     binding.Scopes(),
							HasLocalBinding: true,
						}
						return
					}
				}

				// Use cross-phase lookup to find bindings in any phase
				// (runtime for define, expand for define-syntax, compile for auxiliary syntax).
				// The reference's own scope set decides WHICH same-named binding:
				// a template identifier introduced by an enclosing macro carries that
				// expansion's intro scope, and so does the binder it means, so a second
				// expansion of the same outer macro resolves to its own binder rather
				// than to the first expansion's.
				gi := env.GetGlobalIndexAcrossPhases(symVal, t.Scopes())
				// Store the resolved GlobalIndex (may be nil if unbound, which is ok -
				// unbound free identifiers like special forms will be handled normally)
				freeIds[match.FreeIdKey(symVal.Key, t.Scopes())] = &FreeIdResolution{
					Global:   gi,
					LibScope: libraryScope,
				}
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(t) {
			// Check if this is a nested macro definition form (syntax-rules, define-syntax)
			// that should not be walked for free identifiers
			car := t.SyntaxCar()
			if car != nil {
				carSym, ok := car.(*syntax.SyntaxSymbol)
				if ok {
					carVal := carSym.Unwrap()
					sym, ok := carVal.(*values.Symbol)
					if ok {
						// Skip nested syntax-rules and define-syntax forms
						// These define their own scope and shouldn't pollute the
						// outer macro's free identifier set
						if sym.Key == TransformerSyntaxRules || sym.Key == FormDefineSyntax {
							return
						}
					}
				}
			}

			// Recurse into car
			if car != nil {
				carStx := car
				collectFreeIdentifiersWithEllipsis(env, carStx, patternVars, freeIds, ellipsis, libraryScope)
			}
			// Recurse into cdr
			cdr := t.SyntaxCdr()
			if cdr != nil {
				cdrStx := cdr
				collectFreeIdentifiersWithEllipsis(env, cdrStx, patternVars, freeIds, ellipsis, libraryScope)
			}
		}

	case *syntax.SyntaxVector:
		// R7RS §4.3.2: recurse into vector elements for free identifiers
		for _, elem := range t.Values {
			collectFreeIdentifiersWithEllipsis(env, elem, patternVars, freeIds, ellipsis, libraryScope)
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals don't contain identifiers
		// Do nothing
	}
}

// collectPatternVariablesWithEllipsis walks the pattern and identifies all pattern variables,
// using a custom ellipsis identifier.
// The literalSyntax parameter enables bound-identifier=? comparison for R7RS hygiene:
// a pattern symbol only matches a literal if they have the same name AND the same scopes.
// The varSyntax parameter, if non-nil, will be populated with the syntax symbol of each
// pattern variable. This is critical for nested macro hygiene: when an outer macro introduces
// a symbol into an inner macro's template, the symbol's scopes distinguish it from inner
// pattern variables.
func collectPatternVariablesWithEllipsis(pattern syntax.SyntaxValue, literalSyntax map[string]*syntax.SyntaxSymbol, isFirst bool, variables map[string]struct{}, varSyntax map[string]*syntax.SyntaxSymbol, ellipsis string) error {
	switch p := pattern.(type) {
	case *syntax.SyntaxSymbol:
		sym := p.Unwrap()
		symVal, ok := sym.(*values.Symbol)
		if ok {
			// Skip if it's a keyword (first position) or ellipsis
			if !isFirst && symVal.Key != ellipsis {
				// Check if this identifier matches a literal via bound-identifier=?
				// R7RS §4.3.2: literals are matched by bound-identifier=?, not just name
				litSym, ok := literalSyntax[symVal.Key]
				if ok {
					// Name matches - now check scopes (bound-identifier=? semantics)
					patternScopes := p.Scopes()
					litScopes := litSym.Scopes()
					// For bound-identifier=?, scopes must match bidirectionally (set equality)
					scopesMatch := syntax.ScopesMatch(patternScopes, litScopes) &&
						syntax.ScopesMatch(litScopes, patternScopes)
					if !scopesMatch {
						// Same name but different scopes - treat as pattern variable
						variables[symVal.Key] = struct{}{}
						// Record the pattern variable's syntax for nested macro hygiene
						if varSyntax != nil {
							varSyntax[symVal.Key] = p
						}
					}
					// If scopes match, it's a literal - don't add to variables
				} else {
					// Name not in literals - it's a pattern variable
					variables[symVal.Key] = struct{}{}
					// Record the pattern variable's syntax for nested macro hygiene
					if varSyntax != nil {
						varSyntax[symVal.Key] = p
					}
				}
			}
		}

	case *syntax.SyntaxPair:
		if !syntax.IsSyntaxEmptyList(p) {
			// First element in a form is considered a keyword
			err := collectPatternVariablesWithEllipsis(p.SyntaxCar(), literalSyntax, isFirst, variables, varSyntax, ellipsis)
			if err != nil {
				return err
			}

			// Rest of the form
			cdr := p.SyntaxCdr()
			if cdr != nil {
				err = collectPatternVariablesWithEllipsis(cdr, literalSyntax, false, variables, varSyntax, ellipsis)
				if err != nil {
					return err
				}
			}
		}

	case *syntax.SyntaxVector:
		// R7RS §4.3.2: #(<pattern> ...) — recurse into vector elements
		for _, elem := range p.Values {
			err := collectPatternVariablesWithEllipsis(elem, literalSyntax, false, variables, varSyntax, ellipsis)
			if err != nil {
				return err
			}
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals are not pattern variables
		// Do nothing

	default:
		// Other syntax types are not pattern variables
	}

	return nil
}

// skipEllipses counts the consecutive ellipsis symbols at the head of lst and
// returns that count together with the remaining tail after them. It is used by
// both the pattern-depth computation and the template depth validation so the two
// agree on how many ellipses govern a sub-form.
func skipEllipses(lst syntax.SyntaxValue, ellipsis string) (int, syntax.SyntaxValue) {
	count := 0
	cur := lst
	for {
		pair, ok := cur.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(pair) {
			break
		}
		sym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
		if !ok {
			break
		}
		symVal, ok := sym.Unwrap().(*values.Symbol)
		if !ok || symVal.Key != ellipsis {
			break
		}
		count++
		cur = pair.SyntaxCdr()
	}
	return count, cur
}

// computePatternVarDepths walks a syntax-rules pattern and records, for each
// pattern variable, its ellipsis depth: the number of ellipses governing it. A
// variable bound directly (e.g. `a` in `(_ a)`) has depth 0; under one ellipsis
// (`(_ a ...)`) depth 1; under nested ellipses (`(_ (a ...) ...)`) depth 2. Only
// names already known to be pattern variables are recorded; literals, the macro
// keyword, and the ellipsis marker are naturally excluded.
//
// INVARIANT: this walker must descend into the same sub-forms as
// collectPatternVariablesWithEllipsis (proper list, dotted tail, vector). The
// template check reads patternDepths[name] with a 0 default, so a pattern
// variable this walker fails to reach would be silently mis-depthed as 0 — a
// spurious (loud) rejection. Keep the two case sets in lockstep.
func computePatternVarDepths(pattern syntax.SyntaxValue, variables map[string]struct{}, ellipsis string, depth int, out map[string]int) {
	switch p := pattern.(type) {
	case *syntax.SyntaxSymbol:
		symVal, ok := p.Unwrap().(*values.Symbol)
		if !ok {
			return
		}
		_, isVar := variables[symVal.Key]
		if !isVar {
			return
		}
		// A pattern variable occurs once, but take the maximum defensively.
		d, seen := out[symVal.Key]
		if !seen || depth > d {
			out[symVal.Key] = depth
		}

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(p) {
			return
		}
		cur := syntax.SyntaxValue(p)
		for {
			pair, ok := cur.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(pair) {
				break
			}
			elem := pair.SyntaxCar()
			k, rest := skipEllipses(pair.SyntaxCdr(), ellipsis)
			computePatternVarDepths(elem, variables, ellipsis, depth+k, out)
			cur = rest
		}
		// Dotted tail (improper list pattern): governed at the current depth.
		_, isPair := cur.(*syntax.SyntaxPair)
		if cur != nil && !isPair {
			computePatternVarDepths(cur, variables, ellipsis, depth, out)
		}

	case *syntax.SyntaxVector:
		// Vector patterns: account for ellipsis the same way as lists so a
		// vector-bound variable's depth is not undercounted (undercounting could
		// cause a false positive in the template check). Walk the element slice
		// as if it were a list.
		for i := 0; i < len(p.Values); i++ {
			elem := p.Values[i]
			// Count following ellipsis elements within the vector.
			k := 0
			for j := i + 1; j < len(p.Values); j++ {
				sym, ok := p.Values[j].(*syntax.SyntaxSymbol)
				if !ok {
					break
				}
				symVal, ok := sym.Unwrap().(*values.Symbol)
				if !ok || symVal.Key != ellipsis {
					break
				}
				k++
			}
			computePatternVarDepths(elem, variables, ellipsis, depth+k, out)
			i += k
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals are not pattern variables — no depth to record.
	}
}

// collectTemplatePatternVars walks a template sub-form and collects the pattern
// variables it mentions, mapping each name to its first occurrence's syntax
// symbol (for source-location reporting). Mirrors the runtime expander's
// findSyntaxPatternVariables: every pattern variable textually present, at any
// nesting depth, contributes.
func collectTemplatePatternVars(tmpl syntax.SyntaxValue, variables map[string]struct{}, ellipsis string, out map[string]*syntax.SyntaxSymbol) {
	switch t := tmpl.(type) {
	case *syntax.SyntaxSymbol:
		symVal, ok := t.Unwrap().(*values.Symbol)
		if !ok || symVal.Key == ellipsis {
			return
		}
		_, isVar := variables[symVal.Key]
		if !isVar {
			return
		}
		_, seen := out[symVal.Key]
		if !seen {
			out[symVal.Key] = t
		}

	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(t) {
			return
		}
		collectTemplatePatternVars(t.SyntaxCar(), variables, ellipsis, out)
		collectTemplatePatternVars(t.SyntaxCdr(), variables, ellipsis, out)

	case *syntax.SyntaxVector:
		for _, elem := range t.Values {
			collectTemplatePatternVars(elem, variables, ellipsis, out)
		}

	case *syntax.SyntaxObject:
		// Self-evaluating literals contain no pattern variables.
	}
}

// checkEllipsisGroupDriver enforces the R7RS §4.3.2 invariant that an ellipsis
// sub-template at template depth requiredDepth must contain at least one "driver":
// a pattern variable whose pattern depth is at least requiredDepth, supplying the
// sequence the ellipsis iterates over. A sub-template with pattern variables but
// no driver (every variable bound shallower than requiredDepth) cannot be
// iterated — this is the over-ellipsis violation. A sub-template with no pattern
// variables at all is permitted (R7RS: repeated zero times, dropped).
func checkEllipsisGroupDriver(sub syntax.SyntaxValue, variables map[string]struct{}, patternDepths map[string]int, ellipsis string, requiredDepth int) error {
	vars := make(map[string]*syntax.SyntaxSymbol)
	collectTemplatePatternVars(sub, variables, ellipsis, vars)
	if len(vars) == 0 {
		return nil
	}
	// Look for a driver; meanwhile track the most clearly offending variable
	// (the one bound at the shallowest pattern depth) to report. The name
	// tie-break makes the reported variable deterministic when several share the
	// shallowest depth — `vars` is a Go map, so iteration order is randomized.
	offendingName := ""
	offendingDepth := 0
	var offendingSym *syntax.SyntaxSymbol
	for name, sym := range vars {
		pd := patternDepths[name]
		if pd >= requiredDepth {
			return nil // driver found — group is well-formed
		}
		isFirst := offendingSym == nil
		shallower := pd < offendingDepth
		sameDepthEarlierName := pd == offendingDepth && name < offendingName
		if isFirst || shallower || sameDepthEarlierName {
			offendingName = name
			offendingDepth = pd
			offendingSym = sym
		}
	}
	src := offendingSym.SourceContext()
	return wrapSourcedError(src, werr.WrapForeignErrorf(
		werr.ErrInvalidSyntax,
		"syntax-rules: template variable %q used at ellipsis depth %d but pattern binds it at depth %d",
		offendingName, requiredDepth, offendingDepth,
	))
}

// validateTemplateEllipsisDepth walks a syntax-rules template, tracking the
// ellipsis depth, and validates each ellipsis sub-template against the pattern
// variable depths via checkEllipsisGroupDriver. Errors are wrapped with the
// offending node's source location and surface as a CompilationError at
// macro-definition time.
func validateTemplateEllipsisDepth(tmpl syntax.SyntaxValue, variables map[string]struct{}, patternDepths map[string]int, ellipsis string, depth int) error {
	pair, ok := tmpl.(*syntax.SyntaxPair)
	if !ok {
		return nil
	}
	if syntax.IsSyntaxEmptyList(pair) {
		return nil
	}

	// Ellipsis escape form (<ellipsis> <template>): the inner template is emitted
	// literally with no ellipsis interpretation, so it carries no depth obligation.
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	if ok {
		symVal, ok := carSym.Unwrap().(*values.Symbol)
		if ok && symVal.Key == ellipsis {
			return nil
		}
	}

	cur := syntax.SyntaxValue(pair)
	for {
		p, ok := cur.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(p) {
			break
		}
		elem := p.SyntaxCar()
		k, rest := skipEllipses(p.SyntaxCdr(), ellipsis)
		if k >= 1 {
			err := checkEllipsisGroupDriver(elem, variables, patternDepths, ellipsis, depth+k)
			if err != nil {
				return err
			}
			err = validateTemplateEllipsisDepth(elem, variables, patternDepths, ellipsis, depth+k)
			if err != nil {
				return err
			}
		} else {
			err := validateTemplateEllipsisDepth(elem, variables, patternDepths, ellipsis, depth)
			if err != nil {
				return err
			}
		}
		cur = rest
	}

	// Dotted tail: a non-pair, non-empty cdr is validated at the current depth.
	_, isPair := cur.(*syntax.SyntaxPair)
	if cur != nil && !isPair {
		err := validateTemplateEllipsisDepth(cur, variables, patternDepths, ellipsis, depth)
		if err != nil {
			return err
		}
	}
	return nil
}

// extractLiteralsWithSyntax extracts literal symbols from the literals list,
// also storing the syntax symbols for scope-aware matching.
// The literalSyntax map stores each literal's SyntaxSymbol to enable hygiene:
// if an input symbol has the same name but different scopes (shadowed),
// it won't match the pattern literal.
// R7RS §4.3.2: It is a syntax violation if the ellipsis appears in <literals>.
//
//nolint:unparam
func extractLiteralsWithSyntax(ctx context.Context,
	literalsList *syntax.SyntaxPair,
	literals map[string]struct{},
	literalSyntax map[string]*syntax.SyntaxSymbol,
	ellipsis string,
) error {
	v, err := literalsList.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, literal syntax.SyntaxValue) error {
		sym, ok := literal.(*syntax.SyntaxSymbol)
		if !ok {
			return wrapSourcedError(literal.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "extractLiterals: literal must be a symbol"))
		}

		symVal := sym.Unwrap()
		symbol, ok := symVal.(*values.Symbol)
		if !ok {
			return wrapSourcedError(literal.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "extractLiterals: literal must be a symbol"))
		}
		literals[symbol.Key] = struct{}{}
		// Store syntax symbol for scope-aware matching if map provided
		if literalSyntax != nil {
			literalSyntax[symbol.Key] = sym
		}
		return nil
	})
	if err != nil {
		return wrapSourcedError(literalsList.SourceContext(), werr.WrapForeignErrorf(err, "extractLiterals: literals must be a symbol"))
	}
	if !syntax.IsSyntaxEmptyList(v) {
		return wrapSourcedError(literalsList.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAList, "extractLiterals: literals must be a list"))
	}
	return nil
}

// createTransformerClosure creates a closure that implements the transformer
func createTransformerClosure(env *environment.EnvironmentFrame, clauses []*SyntaxRulesClause, literals map[string]struct{}) (*machine.MachineClosure, error) { //nolint:unparam
	// Create a native template that implements the transformer logic
	// This will be called with the input form on the eval stack

	// For now, create a simple template that will be filled in
	// In a complete implementation, this would generate bytecode that:
	// 1. Gets the input form from parameter 0
	// 2. Tries each clause's pattern in order
	// 3. On first match, expands the template
	// 4. Returns the expanded result

	// Takes 1 parameter - the input form to transform
	template := machine.NewNativeTemplate(1, 0, false)

	// Add transformer logic operations
	// This is a placeholder - the actual implementation would generate
	// operations that implement the pattern matching and expansion

	// For now, store the clauses as a literal and use a special operation
	// Need to create a values.Value wrapper for the clauses
	clausesValue := &ClausesWrapper{Clauses: clauses}
	clausesIdx := template.MaybeAppendLiteral(clausesValue)
	template.AppendOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(clausesIdx),
		NewOperationSyntaxRulesTransform(), // New operation type needed
	)
	template.Optimize()

	// Create closure with a local environment frame for the input parameter
	// This is required because machine.MachineContext.Apply expects LocalEnvironment() to be non-nil
	lenv := environment.NewLocalEnvironment(1) // 1 parameter: the input form
	closureEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	return machine.NewClosureWithTemplate(template, closureEnv), nil
}
