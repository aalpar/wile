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

package validate

import (
	"context"
	"slices"
	"strconv"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// bindingIdentity uniquely identifies a binding by name and scope set.
// Two identifiers with the same name but different scope sets (e.g., introduced
// by different macro expansions) are considered distinct.
type bindingIdentity struct {
	key      string
	scopeKey string
}

// scopeFingerprint builds a deterministic string from sorted scope IDs.
// Identifiers with different scope sets produce different fingerprints,
// allowing the duplicate-binding check to respect hygiene.
func scopeFingerprint(scopes []*syntax.Scope) string {
	if len(scopes) == 0 {
		return ""
	}
	ids := make([]string, len(scopes))
	for i, s := range scopes {
		if s == nil {
			ids[i] = "0"
			continue
		}
		ids[i] = strconv.FormatUint(s.ID(), 10)
	}
	slices.Sort(ids)
	return strings.Join(ids, ",")
}

// validateLetCommon validates all four binding forms (let, let*, letrec, letrec*).
// The Kind parameter determines scoping and evaluation semantics.
func validateLetCommon(
	kind LetKind,
	formName string,
) validatorFunc {
	return func(
		ctx context.Context,
		env *environment.EnvironmentFrame,
		pair *syntax.SyntaxPair,
		result *ValidationResult,
	) ValidatedExpr {
		source, elements, ok := formPrologue(pair, formName, 2, -1, result)
		if !ok {
			return nil
		}

		// Named let detection (only for plain let)
		if kind == LetKindLet {
			sym, symOk := asSyntaxSymbol(elements[1])
			if symOk {
				return validateNamedLet(ctx, env, source, sym, elements, result)
			}
		}

		if len(elements) < 3 {
			result.addError(source, formName,
				formName+" requires at least one body expression")
			return nil
		}

		bindingsPair, empty, pairOk := parseBindingsList(elements[1], formName, result)
		if !pairOk {
			return nil
		}

		if empty {
			body, ok := validateBodySlice(ctx, env, elements, 2, result)
			if !ok {
				return nil
			}
			return &ValidatedLet{
				validatedBase: validatedBase{formName: formName, source: source},
				Kind:          kind,
				body:          body,
			}
		}

		switch kind {
		case LetKindLet:
			return validateLetBindingsAndBody(ctx, env, kind, formName, source,
				bindingsPair, elements, result)
		case LetKindLetStar:
			return validateLetStarBindingsAndBody(ctx, env, formName, source,
				bindingsPair, elements, result)
		default: // LetKindLetrec, LetKindLetrecStar
			return validateLetrecBindingsAndBody(ctx, env, kind, formName, source,
				bindingsPair, elements, result)
		}
	}
}

// validateLetBindingsAndBody handles plain let: inits validated in outer env.
func validateLetBindingsAndBody(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	kind LetKind,
	formName string,
	source *syntax.SourceContext,
	bindingsPair *syntax.SyntaxPair,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	bindings, ok := validateLetBindingPairs(ctx, env, bindingsPair, formName, result)
	if !ok {
		return nil
	}

	if !checkDuplicateBindingNames(bindings, formName, result) {
		return nil
	}

	childEnv := createLetValidationEnv(env, bindings)

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, false)
	markEscapedBindings(childEnv, bindings, body, false)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          kind,
		Bindings:      bindings,
		body:          body,
	}
}

// letStarRawBinding holds a parsed but not-yet-validated let* binding pair.
type letStarRawBinding struct {
	name    *syntax.SyntaxSymbol
	initStx syntax.SyntaxValue
}

// validateLetStarBindingsAndBody handles let*: inits validated incrementally.
//
// When bindings contain duplicate names, each binding that shadows an earlier
// one starts a new env frame. This produces nested ValidatedLet nodes that the
// compiler handles naturally — matching the R7RS semantics of nested let forms.
// The common case (no duplicates) uses a single flat env frame.
func validateLetStarBindingsAndBody(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	formName string,
	source *syntax.SourceContext,
	bindingsPair *syntax.SyntaxPair,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	bindingsListRaw, improper := collectList(bindingsPair)
	if improper {
		result.addError(getSourceContext(elements[1]), formName,
			formName+" bindings must be a proper list")
		return nil
	}

	// First pass: parse all binding pairs (names + init syntax), validate structure.
	var raw []letStarRawBinding
	allOk := true
	for _, bindingExpr := range bindingsListRaw {
		bPair, bOk := bindingExpr.(*syntax.SyntaxPair)
		if !bOk || syntax.IsSyntaxEmptyList(bPair) {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(bPair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		nameSym, symOk := asSyntaxSymbol(elems[0])
		if !symOk {
			result.addError(getSourceContext(elems[0]), formName,
				formName+" binding name must be a symbol")
			allOk = false
			continue
		}

		raw = append(raw, letStarRawBinding{name: nameSym, initStx: elems[1]})
	}
	if !allOk {
		return nil
	}

	// Check for duplicate names — determines which code path to use.
	// Uses scope-aware identity so hygienic bindings with the same name
	// but different scopes are not falsely treated as duplicates.
	hasDups := false
	if len(raw) >= 2 {
		seen := make(map[bindingIdentity]bool, len(raw))
		for _, r := range raw {
			id := bindingIdentity{
				key:      r.name.Sym.Key,
				scopeKey: scopeFingerprint(r.name.Scopes()),
			}
			if seen[id] {
				hasDups = true
				break
			}
			seen[id] = true
		}
	}

	if !hasDups {
		return validateLetStarFlat(ctx, env, formName, source, raw, elements, result)
	}
	return validateLetStarNested(ctx, env, formName, source, raw, elements, result)
}

// validateLetStarFlat is the efficient single-frame path for let* without
// duplicate binding names. All bindings share one env frame.
func validateLetStarFlat(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	formName string,
	source *syntax.SourceContext,
	raw []letStarRawBinding,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var bindings []ValidatedLetBinding
	for _, r := range raw {
		init := validateExpr(ctx, childEnv, r.initStx, result)
		if init == nil {
			return nil
		}

		bindings = append(bindings, ValidatedLetBinding{Name: r.name, Init: init})

		childEnv.MaybeCreateLocalBinding(
			r.name.Sym,
			environment.BindingTypeVariable,
			r.name.Scopes(),
			r.name.SourceContext(),
		)
	}

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
	markEscapedBindings(childEnv, bindings, body, true)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          LetKindLetStar,
		Bindings:      bindings,
		body:          body,
	}
}

// validateLetStarNested handles let* with duplicate binding names by nesting
// each binding in its own env frame. Produces nested ValidatedLet nodes that
// the compiler handles naturally, matching R7RS nested-let semantics.
func validateLetStarNested(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	formName string,
	source *syntax.SourceContext,
	raw []letStarRawBinding,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	// Validate each binding incrementally, creating a new env frame per binding.
	type validatedBinding struct {
		binding  ValidatedLetBinding
		childEnv *environment.EnvironmentFrame
	}
	validated := make([]validatedBinding, 0, len(raw))
	currentEnv := env
	for _, r := range raw {
		init := validateExpr(ctx, currentEnv, r.initStx, result)
		if init == nil {
			return nil
		}

		// let* binds each name in a fresh child frame; subsequent inits see
		// the evolving env. Use extendEnvWithSymbols with a single-symbol
		// slice so the per-name child-frame creation lives in one helper.
		childEnv := extendEnvWithSymbols(currentEnv, []*syntax.SyntaxSymbol{r.name})

		validated = append(validated, validatedBinding{
			binding:  ValidatedLetBinding{Name: r.name, Init: init},
			childEnv: childEnv,
		})
		currentEnv = childEnv
	}

	// Validate body in innermost env
	body, ok := validateBodySlice(ctx, currentEnv, elements, 2, result)
	if !ok {
		return nil
	}

	// Build nested ValidatedLet from innermost to outermost.
	// Each binding wraps the next as its body.
	var innerBody []ValidatedExpr
	innerBody = body
	for i := range slices.Backward(validated) {
		vb := validated[i]
		bindings := []ValidatedLetBinding{vb.binding}
		markMutableBindings(vb.childEnv, bindings, result)
		markCapturedBindings(vb.childEnv, bindings, innerBody, true)
		markEscapedBindings(vb.childEnv, bindings, innerBody, true)
		node := &ValidatedLet{
			validatedBase: validatedBase{formName: formName, source: source},
			Kind:          LetKindLetStar,
			Bindings:      bindings,
			body:          innerBody,
		}
		innerBody = []ValidatedExpr{node}
	}

	// The outermost node is innerBody[0]
	return innerBody[0]
}

// validateLetrecBindingsAndBody handles letrec/letrec*: all bindings visible
// in all inits.
func validateLetrecBindingsAndBody(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	kind LetKind,
	formName string,
	source *syntax.SourceContext,
	bindingsPair *syntax.SyntaxPair,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	bindingsListRaw, improper := collectList(bindingsPair)
	if improper {
		result.addError(getSourceContext(elements[1]), formName,
			formName+" bindings must be a proper list")
		return nil
	}

	// First pass: collect names, create child env with ALL bindings visible
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var nameSyms []*syntax.SyntaxSymbol
	var initExprs []syntax.SyntaxValue
	allOk := true
	for _, bindingExpr := range bindingsListRaw {
		bPair, bOk := bindingExpr.(*syntax.SyntaxPair)
		if !bOk || syntax.IsSyntaxEmptyList(bPair) {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(bPair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		nameSym, symOk := asSyntaxSymbol(elems[0])
		if !symOk {
			result.addError(getSourceContext(elems[0]), formName,
				formName+" binding name must be a symbol")
			allOk = false
			continue
		}

		nameSyms = append(nameSyms, nameSym)
		initExprs = append(initExprs, elems[1])

		bindLocalSymbol(childEnv, nameSym)
	}

	if !allOk {
		return nil
	}

	// Check for duplicate binding names (R7RS §4.2.2)
	// Uses scope-aware identity so that identifiers with the same name but
	// different scope sets (introduced by hygienic macro expansion) are distinct.
	if len(nameSyms) >= 2 {
		seen := make(map[bindingIdentity]bool, len(nameSyms))
		for _, ns := range nameSyms {
			id := bindingIdentity{
				key:      ns.Sym.Key,
				scopeKey: scopeFingerprint(ns.Scopes()),
			}
			if seen[id] {
				result.addErrorf(getSourceContext(ns), formName,
					"duplicate binding name %q", ns.Sym.Key)
				allOk = false
				continue
			}
			seen[id] = true
		}
		if !allOk {
			return nil
		}
	}

	// Second pass: validate init expressions in child env
	var bindings []ValidatedLetBinding
	for i, initExpr := range initExprs {
		init := validateExpr(ctx, childEnv, initExpr, result)
		if init == nil {
			allOk = false
			continue
		}
		bindings = append(bindings, ValidatedLetBinding{Name: nameSyms[i], Init: init})
	}

	if !allOk {
		return nil
	}

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
	markEscapedBindings(childEnv, bindings, body, true)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          kind,
		Bindings:      bindings,
		body:          body,
	}
}

// validateNamedLet validates (let tag ((name val) ...) body ...)
// by producing a ValidatedLet with LetKindLetrec semantics and Tag set.
func validateNamedLet(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	source *syntax.SourceContext,
	tag *syntax.SyntaxSymbol,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	if len(elements) < 4 {
		result.addError(source, "let",
			"named let requires bindings and at least one body expression")
		return nil
	}

	var bindings []ValidatedLetBinding
	bindingsPair, empty, pairOk := parseBindingsList(elements[2], "let", result)
	if !pairOk {
		return nil
	}
	if !empty {
		var ok bool
		bindings, ok = validateLetBindingPairs(ctx, env, bindingsPair, "let", result)
		if !ok {
			return nil
		}
		if !checkDuplicateBindingNames(bindings, "let", result) {
			return nil
		}
	}

	// Create env with tag visible (for recursive calls in body)
	tagEnv := extendEnvWithSymbols(env, []*syntax.SyntaxSymbol{tag})

	bodyEnv := createLetValidationEnv(tagEnv, bindings)

	body, ok := validateBodySlice(ctx, bodyEnv, elements, 3, result)
	if !ok {
		return nil
	}

	lambdaInit := buildNamedLetLambda(bindings, body, source)

	callArgs := make([]ValidatedExpr, len(bindings))
	for i, b := range bindings {
		callArgs[i] = b.Init
	}
	callExpr := &ValidatedCall{
		validatedBase: validatedBase{formName: "@call", source: source},
		proc: &ValidatedSymbol{
			validatedBase: validatedBase{formName: "@symbol"},
			Symbol:        tag,
		},
		args: callArgs,
	}

	tagBindings := []ValidatedLetBinding{{Name: tag, Init: lambdaInit}}
	tagBody := []ValidatedExpr{callExpr}
	markMutableBindings(tagEnv, tagBindings, result)
	markCapturedBindings(tagEnv, tagBindings, tagBody, true)
	markEscapedBindings(tagEnv, tagBindings, tagBody, true)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: "letrec", source: source},
		Kind:          LetKindLetrec,
		Bindings:      tagBindings,
		Tag:           tag,
		body:          tagBody,
	}
}

// buildNamedLetLambda constructs a ValidatedLambda from the binding names
// (as parameters) and the validated body.
func buildNamedLetLambda(
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	source *syntax.SourceContext,
) ValidatedExpr {
	params := &ValidatedParams{
		Required: make([]*syntax.SyntaxSymbol, len(bindings)),
	}
	for i, b := range bindings {
		params.Required[i] = b.Name
	}
	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda", source: source},
		validatedProcBase: validatedProcBase{
			params: params,
			body:   body,
		},
	}
}

// --- Shared helpers ---

// checkDuplicateBindingNames reports an error for each duplicate name in
// the binding list. R7RS §4.2.2: "It is an error for a ⟨variable⟩ to
// appear more than once" in let, letrec, and letrec*. (let* allows
// duplicates — sequential shadowing.)
func checkDuplicateBindingNames(
	bindings []ValidatedLetBinding,
	formName string,
	result *ValidationResult,
) bool {
	if len(bindings) < 2 {
		return true
	}
	seen := make(map[bindingIdentity]bool, len(bindings))
	allOk := true
	for _, b := range bindings {
		id := bindingIdentity{
			key:      b.Name.Sym.Key,
			scopeKey: scopeFingerprint(b.Name.Scopes()),
		}
		if seen[id] {
			result.addErrorf(getSourceContext(b.Name), formName,
				"duplicate binding name %q", b.Name.Sym.Key)
			allOk = false
			continue
		}
		seen[id] = true
	}
	return allOk
}

// validateLetBindingPairs parses and validates a ((name val) ...) binding list.
func validateLetBindingPairs(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	bindingsPair *syntax.SyntaxPair,
	formName string,
	result *ValidationResult,
) ([]ValidatedLetBinding, bool) {
	bindingsList, improper := collectList(bindingsPair)
	if improper {
		result.addError(getSourceContext(bindingsPair), formName,
			formName+" bindings must be a proper list")
		return nil, false
	}

	var bindings []ValidatedLetBinding
	allOk := true
	for _, bindingExpr := range bindingsList {
		pair, ok := bindingExpr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(pair) {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(pair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		nameSym, symOk := asSyntaxSymbol(elems[0])
		if !symOk {
			result.addError(getSourceContext(elems[0]), formName,
				formName+" binding name must be a symbol")
			allOk = false
			continue
		}

		init := validateExpr(ctx, env, elems[1], result)
		if init == nil {
			allOk = false
			continue
		}

		bindings = append(bindings, ValidatedLetBinding{Name: nameSym, Init: init})
	}

	if !allOk {
		return nil, false
	}
	return bindings, true
}

// createLetValidationEnv creates a child environment with let bindings.
func createLetValidationEnv(
	env *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
) *environment.EnvironmentFrame {
	if len(bindings) == 0 {
		return env
	}
	syms := make([]*syntax.SyntaxSymbol, len(bindings))
	for i, b := range bindings {
		syms[i] = b.Name
	}
	return extendEnvWithSymbols(env, syms)
}

// parseBindingsList extracts the bindings pair from a syntax value.
func parseBindingsList(
	bindingsStx syntax.SyntaxValue,
	formName string,
	result *ValidationResult,
) (*syntax.SyntaxPair, bool, bool) {
	if syntax.IsSyntaxEmptyList(bindingsStx) {
		return nil, true, true
	}
	pair, ok := bindingsStx.(*syntax.SyntaxPair)
	if !ok {
		result.addError(getSourceContext(bindingsStx), formName,
			formName+" bindings must be a list")
		return nil, false, false
	}
	return pair, false, true
}

// markMutableBindings checks which let bindings were targeted by set!
// in the body and marks them accordingly. Best-effort: if binding
// resolution fails (scope mismatch), the binding stays immutable.
// Must not gate correctness-critical optimizations without re-validation.
func markMutableBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	result *ValidationResult,
) {
	if childEnv == nil {
		return
	}
	for i, b := range bindings {
		bid, ok := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if ok && result.isMutated(bid) {
			bindings[i].Mutable = true
		}
	}
}
