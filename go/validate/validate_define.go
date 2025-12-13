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

package validate

import (
	"context"
	"wile/syntax"
	"wile/values"
)

// validateDefine validates both forms:
// (define name expr) and (define (name params...) body...)
func validateDefine(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "define", "define form must be a proper list")
		return nil
	}

	// elements[0] is 'define', need at least name and value/body
	if len(elements) < 3 {
		result.addErrorf(source, "define", "define requires at least 2 arguments, got %d", len(elements)-1)
		return nil
	}

	// Check the second element to determine which form
	second := elements[1]

	switch s := second.(type) {
	case *syntax.SyntaxSymbol:
		// (define name expr) - simple variable definition
		return validateDefineVariable(nil, source, s, elements, result)

	case *syntax.SyntaxPair:
		// (define (name params...) body...) - function definition
		return validateDefineFunction(nil, source, s, elements, result)

	default:
		result.addErrorf(source, "define", "expected symbol or list after define, got %T", second)
		return nil
	}
}

// validateDefineVariable validates (define name expr)
func validateDefineVariable(ctx context.Context, source *syntax.SourceContext, name *syntax.SyntaxSymbol, elements []syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	if len(elements) != 3 {
		result.addErrorf(source, "define", "variable definition requires exactly 1 value, got %d", len(elements)-2)
		return nil
	}

	value := validateExpr(ctx, elements[2], result)
	if value == nil {
		return nil
	}

	return &ValidatedDefine{
		formName:   "define",
		source:     source,
		Name:       name,
		Value:      value,
		IsFunction: false,
	}
}

// validateDefineFunction validates (define (name params...) body...)
func validateDefineFunction(ctx context.Context, source *syntax.SourceContext, nameAndParams *syntax.SyntaxPair, elements []syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	// Extract the function name from car of the list
	nameExpr := nameAndParams.Car()
	name, ok := asSyntaxSymbol(nameExpr.(syntax.SyntaxValue))
	if !ok {
		result.addErrorf(source, "define", "expected symbol as function name, got %T", nameExpr)
		return nil
	}

	// Extract and validate parameters from cdr
	paramsCdr := nameAndParams.Cdr()
	params := validateParams(paramsCdr.(syntax.SyntaxValue), result)
	if params == nil && result.Errors != nil {
		// params can be nil for () but we should have errors if validation failed
		// Check if we actually had errors or just empty params
	}

	// Validate body - must have at least one expression
	if len(elements) < 3 {
		result.addError(source, "define", "function definition requires at least one body expression")
		return nil
	}

	var body []ValidatedExpr
	for i := 2; i < len(elements); i++ {
		expr := validateExpr(ctx, elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}

	// If body validation failed, return nil
	if len(body) != len(elements)-2 {
		return nil
	}

	return &ValidatedDefine{
		formName:   "define",
		source:     source,
		Name:       name,
		IsFunction: true,
		Params:     params,
		Body:       body,
	}
}

// validateParams validates a parameter list
// Handles: (a b c), (a b . rest), and just rest
func validateParams(paramExpr syntax.SyntaxValue, result *ValidationResult) *ValidatedParams {
	params := &ValidatedParams{formName: "@params"}

	// Handle single symbol as rest parameter: (lambda x body)
	if sym, ok := asSyntaxSymbol(paramExpr); ok {
		params.Rest = sym
		return params
	}

	// Must be a pair (possibly empty list)
	pair, ok := paramExpr.(*syntax.SyntaxPair)
	if !ok {
		result.addErrorf(getSourceContext(paramExpr), params.formName, "expected parameter list, got %T", paramExpr)
		return nil
	}

	// Handle empty list: (lambda () body)
	if pair.IsEmptyList() {
		return params
	}

	// Walk the list
	seen := make(map[string]bool)
	var current values.Value = pair

	for {
		p, ok := current.(*syntax.SyntaxPair)
		if !ok {
			// Not a pair - check if it's a rest parameter (improper list)
			if sv, ok := current.(syntax.SyntaxValue); ok {
				sym, ok := asSyntaxSymbol(sv)
				if !ok {
					result.addErrorf(getSourceContext(sv), params.formName, "expected symbol as rest parameter, got %T", current)
					return nil
				}

				// Check for duplicate with rest param
				symKey := sym.Unwrap().(*values.Symbol).Key
				if seen[symKey] {
					result.addErrorf(getSourceContext(sv), params.formName, "duplicate parameter name: %s", symKey)
					return nil
				}

				params.Rest = sym
			}
			return params
		}

		// Check for empty list marker
		if p.IsEmptyList() {
			return params
		}

		// Get the parameter
		car := p.Car()
		paramVal, ok := car.(syntax.SyntaxValue)
		if !ok {
			result.addErrorf(nil, params.formName, "expected syntax value in parameter list, got %T", car)
			return nil
		}

		sym, ok := asSyntaxSymbol(paramVal)
		if !ok {
			result.addErrorf(getSourceContext(paramVal), params.formName, "expected symbol in parameter list, got %T", paramVal)
			return nil
		}

		// Check for duplicates
		symKey := sym.Unwrap().(*values.Symbol).Key
		if seen[symKey] {
			result.addErrorf(getSourceContext(paramVal), params.formName, "duplicate parameter name: %s", symKey)
			return nil
		}
		seen[symKey] = true

		params.Required = append(params.Required, sym)
		current = p.Cdr()
	}
}
