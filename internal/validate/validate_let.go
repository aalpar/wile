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

// validateLetBindings parses and validates a ((name val) ...) binding list.
// Each binding must be a two-element list with a symbol as the first element.
// Init expressions are validated in the provided env.
func validateLetBindings(
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

// createLetValidationEnv creates a child environment with let bindings
// for body validation. Mirrors createLambdaValidationEnv.
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

// parseBindingsList extracts and validates the structure of a bindings
// argument, returning the *SyntaxPair if non-empty or nil for empty lists.
func parseBindingsList(
	bindingsStx syntax.SyntaxValue,
	formName string,
	result *ValidationResult,
) (*syntax.SyntaxPair, bool, bool) {
	if syntax.IsSyntaxEmptyList(bindingsStx) {
		return nil, true, true // empty, ok
	}
	pair, ok := bindingsStx.(*syntax.SyntaxPair)
	if !ok {
		result.addError(getSourceContext(bindingsStx), formName,
			formName+" bindings must be a list")
		return nil, false, false
	}
	return pair, false, true // non-empty, ok
}

// validateLet validates (let ((name val) ...) body ...).
// R7RS §4.2.2: Init expressions are evaluated in the current environment;
// the body is evaluated in a child environment where all bindings are visible.
func validateLet(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "let", 2, -1, result)
	if !ok {
		return nil
	}

	// Detect named let: (let tag ((bindings...) ...) body ...)
	sym, symOk := asSyntaxSymbol(elements[1])
	if symOk {
		return validateNamedLet(ctx, env, source, sym, elements, result)
	}

	if len(elements) < 3 {
		result.addError(source, "let", "let requires at least one body expression")
		return nil
	}

	// Validate bindings — init exprs validated in current env (not child).
	var bindings []ValidatedLetBinding
	bindingsPair, empty, pairOk := parseBindingsList(elements[1], "let", result)
	if !pairOk {
		return nil
	}
	if !empty {
		var bindOk bool
		bindings, bindOk = validateLetBindings(ctx, env, bindingsPair, "let", result)
		if !bindOk {
			return nil
		}
	}

	childEnv := createLetValidationEnv(env, bindings)

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	markMutableBindings(childEnv, bindings, result)

	return &ValidatedLet{
		validatedBase: validatedBase{formName: "let", source: source},
		Bindings:      bindings,
		body:          body,
	}
}

// validateNamedLet validates (let tag ((name val) ...) body ...)
// by producing a ValidatedLetrec equivalent to
// (letrec ((tag (lambda (names...) body...))) (tag vals...)).
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

	// elements[2] = bindings, elements[3..] = body

	// Validate bindings in OUTER env (init exprs don't see tag).
	var bindings []ValidatedLetBinding
	bindingsPair, empty, pairOk := parseBindingsList(elements[2], "let", result)
	if !pairOk {
		return nil
	}
	if !empty {
		var ok bool
		bindings, ok = validateLetBindings(ctx, env, bindingsPair, "let", result)
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

	// Create body env with tag + binding names visible
	bodyEnv := createLetValidationEnv(tagEnv, bindings)

	body, ok := validateBodySlice(ctx, bodyEnv, elements, 3, result)
	if !ok {
		return nil
	}

	// Build the lambda init: (lambda (names...) body...)
	lambdaInit := buildNamedLetLambda(bindings, body, source)

	// Build the call: (tag val1 val2 ...)
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

	return &ValidatedLetrec{
		validatedBase: validatedBase{formName: "letrec", source: source},
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

// validateLetStar validates (let* ((name val) ...) body ...).
// R7RS §4.2.2: Each init sees all preceding bindings.
func validateLetStar(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "let*", 2, -1, result)
	if !ok {
		return nil
	}

	if len(elements) < 3 {
		result.addError(source, "let*", "let* requires at least one body expression")
		return nil
	}

	bindingsPair, empty, pairOk := parseBindingsList(elements[1], "let*", result)
	if !pairOk {
		return nil
	}

	// Empty bindings — skip to body
	if empty {
		body, ok := validateBodySlice(ctx, env, elements, 2, result)
		if !ok {
			return nil
		}
		return &ValidatedLetStar{
			validatedBase: validatedBase{formName: "let*", source: source},
			body:          body,
		}
	}

	bindingsListRaw, improper := collectList(bindingsPair)
	if improper {
		result.addError(getSourceContext(elements[1]), "let*",
			"let* bindings must be a proper list")
		return nil
	}

	// Build child env incrementally
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var bindings []ValidatedLetBinding
	allOk := true
	for _, bindingExpr := range bindingsListRaw {
		bPair, bOk := bindingExpr.(*syntax.SyntaxPair)
		if !bOk || syntax.IsSyntaxEmptyList(bPair) {
			result.addError(getSourceContext(bindingExpr), "let*",
				"let* binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(bPair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), "let*",
				"let* binding must be (name init)")
			allOk = false
			continue
		}

		nameSym, symOk := asSyntaxSymbol(elems[0])
		if !symOk {
			result.addError(getSourceContext(elems[0]), "let*",
				"let* binding name must be a symbol")
			allOk = false
			continue
		}

		// Validate init in current childEnv (sees preceding bindings)
		init := validateExpr(ctx, childEnv, elems[1], result)
		if init == nil {
			allOk = false
			continue
		}

		bindings = append(bindings, ValidatedLetBinding{Name: nameSym, Init: init})

		// Add this binding to childEnv for subsequent inits
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

	return &ValidatedLetStar{
		validatedBase: validatedBase{formName: "let*", source: source},
		Bindings:      bindings,
		body:          body,
	}
}

// validateLetrec validates (letrec ((name val) ...) body ...).
// R7RS §4.2.2: All bindings are in scope for all inits and body.
func validateLetrec(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	return validateLetrecCommon(ctx, env, pair, "letrec", false, result)
}

// validateLetrecStar validates (letrec* ((name val) ...) body ...).
// R7RS §4.2.2: Same scoping as letrec; left-to-right evaluation with
// preceding bindings having their values.
func validateLetrecStar(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	return validateLetrecCommon(ctx, env, pair, "letrec*", true, result)
}

// validateLetrecCommon is shared logic for letrec and letrec*.
func validateLetrecCommon(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	formName string,
	isStar bool,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, formName, 2, -1, result)
	if !ok {
		return nil
	}

	if len(elements) < 3 {
		result.addError(source, formName,
			formName+" requires at least one body expression")
		return nil
	}

	bindingsPairStx, empty, pairOk := parseBindingsList(elements[1], formName, result)
	if !pairOk {
		return nil
	}

	// Empty bindings
	if empty {
		body, ok := validateBodySlice(ctx, env, elements, 2, result)
		if !ok {
			return nil
		}
		return &ValidatedLetrec{
			validatedBase: validatedBase{formName: formName, source: source},
			LetrecStar:    isStar,
			body:          body,
		}
	}

	bindingsListRaw, improper := collectList(bindingsPairStx)
	if improper {
		result.addError(getSourceContext(elements[1]), formName,
			formName+" bindings must be a proper list")
		return nil
	}

	// First pass: collect names and create child env with ALL bindings visible
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

	// Second pass: validate init expressions in child env (all names visible)
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

	return &ValidatedLetrec{
		validatedBase: validatedBase{formName: formName, source: source},
		Bindings:      bindings,
		LetrecStar:    isStar,
		body:          body,
	}
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
