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
	"github.com/aalpar/wile/values"
)

// validateDefine validates both forms:
// (define name expr) and (define (name params...) body...)
func validateDefine(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "define", 2, -1, result)
	if !ok {
		return nil
	}

	// Check the second element to determine which form
	second := elements[1]

	switch s := second.(type) {
	case *syntax.SyntaxSymbol:
		// (define name expr) - simple variable definition
		return validateDefineVariable(ctx, env, source, s, elements, result)

	case *syntax.SyntaxPair:
		// (define (name params...) body...) - function definition
		return validateDefineFunction(ctx, env, source, s, elements, result)

	default:
		result.addErrorf(source, "define", "expected symbol or list after define, got %T", second)
		return nil
	}
}

// validateDefineVariable validates (define name expr)
func validateDefineVariable(ctx context.Context, env *environment.EnvironmentFrame, source *syntax.SourceContext, name *syntax.SyntaxSymbol, elements []syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	if len(elements) != 3 {
		result.addErrorf(source, "define", "variable definition requires exactly 1 value, got %d", len(elements)-2)
		return nil
	}

	value := validateExpr(ctx, env, elements[2], result)
	if value == nil {
		return nil
	}

	return &ValidatedDefine{
		validatedBase: validatedBase{formName: "define", source: source},
		name:          name,
		subExp:        value,
		IsFunction:    false,
	}
}

// validateDefineFunction validates (define (name params...) body...)
func validateDefineFunction(ctx context.Context, env *environment.EnvironmentFrame, source *syntax.SourceContext, nameAndParams *syntax.SyntaxPair, elements []syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	// Extract the function name from car of the list
	nameExpr := nameAndParams.Car()
	name, ok := nameExpr.(*syntax.SyntaxSymbol)
	if !ok {
		result.addErrorf(source, "define", "expected symbol as function name, got %T", nameExpr)
		return nil
	}

	// Extract and validate parameters from cdr
	paramsCdr := nameAndParams.Cdr()
	params := validateParams(paramsCdr.(syntax.SyntaxValue), result)

	// Validate body - must have at least one expression
	if len(elements) < 3 {
		result.addError(source, "define", "function definition requires at least one body expression")
		return nil
	}

	body, ok := validateBodySlice(ctx, env, elements, 2, result)
	if !ok {
		return nil
	}

	docstring, body := extractDocstring(body)

	return &ValidatedDefine{
		validatedBase:     validatedBase{formName: "define", source: source},
		validatedProcBase: validatedProcBase{params: params, body: body, docstring: docstring},
		name:              name,
		IsFunction:        true,
	}
}

// validateParams validates a parameter list
// Handles: (a b c), (a b . rest), and just rest
func validateParams(paramExpr syntax.SyntaxValue, result *ValidationResult) *ValidatedParams {
	params := &ValidatedParams{formName: "@params"}

	// Handle single symbol as rest parameter: (lambda x body)
	sym, ok := paramExpr.(*syntax.SyntaxSymbol)
	if ok {
		params.Rest = sym
		return params
	}

	// Handle empty list: (lambda () body)
	if syntax.IsSyntaxEmptyList(paramExpr) {
		return params
	}

	// Must be a pair
	pair, ok := paramExpr.(*syntax.SyntaxPair)
	if !ok {
		result.addErrorf(getSourceContext(paramExpr), params.formName, "expected parameter list, got %T", paramExpr)
		return nil
	}

	// Walk the list, collecting required params and optional rest.
	// Duplicate detection runs post-parse via findDuplicateSymbols below.
	var current values.Value = pair
	for !values.IsEmptyList(current) {
		p, ok := current.(*syntax.SyntaxPair)
		if !ok {
			// Not a pair - check if it's a rest parameter (improper list)
			sv, ok := current.(syntax.SyntaxValue)
			if ok {
				sym, ok := sv.(*syntax.SyntaxSymbol)
				if !ok {
					result.addErrorf(getSourceContext(sv), params.formName, "expected symbol as rest parameter, got %T", current)
					return nil
				}
				params.Rest = sym
			}
			break
		}

		// Get the parameter
		car := p.Car()
		paramVal, ok := car.(syntax.SyntaxValue)
		if !ok {
			result.addErrorf(nil, params.formName, "expected syntax value in parameter list, got %T", car)
			return nil
		}

		sym, ok := paramVal.(*syntax.SyntaxSymbol)
		if !ok {
			result.addErrorf(getSourceContext(paramVal), params.formName, "expected symbol in parameter list, got %T", paramVal)
			return nil
		}

		params.Required = append(params.Required, sym)
		current = p.Cdr()
	}

	// Post-parse duplicate detection over required + rest. Allocate a fresh
	// slice (don't alias params.Required) so the append for Rest cannot
	// mutate the params field.
	//
	// Per validate/CLAUDE.md's error-accumulation convention, emit one error
	// per duplicate rather than stopping at the first — matches the peer
	// paths in validate_let.go (letrec dup loop, checkDuplicateBindingNames).
	allSyms := append([]*syntax.SyntaxSymbol{}, params.Required...)
	if params.Rest != nil {
		allSyms = append(allSyms, params.Rest)
	}
	allOk := true
	for _, dup := range findDuplicateSymbols(allSyms) {
		result.addErrorf(getSourceContext(dup), params.formName,
			"duplicate parameter name: %s", dup.Key())
		allOk = false
	}
	if !allOk {
		return nil
	}
	return params
}
