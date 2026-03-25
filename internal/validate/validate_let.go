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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

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

	childEnv := createLetValidationEnv(env, bindings)

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          kind,
		Bindings:      bindings,
		body:          body,
	}
}

// validateLetStarBindingsAndBody handles let*: inits validated incrementally.
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

	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var bindings []ValidatedLetBinding
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

		init := validateExpr(ctx, childEnv, elems[1], result)
		if init == nil {
			allOk = false
			continue
		}

		bindings = append(bindings, ValidatedLetBinding{Name: nameSym, Init: init})

		childEnv.MaybeCreateLocalBindingWithScopes(
			nameSym.Sym,
			environment.BindingTypeVariable,
			nameSym.Scopes(),
			nameSym.SourceContext(),
		)
	}

	if !allOk {
		return nil
	}

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          LetKindLetStar,
		Bindings:      bindings,
		body:          body,
	}
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

		childEnv.MaybeCreateLocalBindingWithScopes(
			nameSym.Sym,
			environment.BindingTypeVariable,
			nameSym.Scopes(),
			nameSym.SourceContext(),
		)
	}

	if !allOk {
		return nil
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
	}

	// Create env with tag visible (for recursive calls in body)
	lenv := environment.NewLocalEnvironment(0)
	tagEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	tagEnv.MaybeCreateLocalBindingWithScopes(
		tag.Sym,
		environment.BindingTypeVariable,
		tag.Scopes(),
		tag.SourceContext(),
	)

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

	return &ValidatedLet{
		validatedBase: validatedBase{formName: "letrec", source: source},
		Kind:          LetKindLetrec,
		Bindings:      []ValidatedLetBinding{{Name: tag, Init: lambdaInit}},
		Tag:           tag,
		body:          []ValidatedExpr{callExpr},
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
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	for _, b := range bindings {
		childEnv.MaybeCreateLocalBindingWithScopes(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
	}
	return childEnv
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
// in the body and marks them accordingly.
func markMutableBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	result *ValidationResult,
) {
	if childEnv == nil {
		return
	}
	for i, b := range bindings {
		binding := childEnv.GetBindingWithScopes(b.Name.Sym, b.Name.Scopes())
		if binding != nil && result.isMutated(binding) {
			bindings[i].Mutable = true
		}
	}
}
