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
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// ValidateExpression validates a syntax expression and returns
// a validated form or a list of errors.
// The env parameter provides the environment context for checking local variable
// shadowing of special forms (R7RS §4.2.2).
func ValidateExpression(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue) *ValidationResult {
	result := &ValidationResult{}
	validated := validateExpr(ctx, env, expr, result)
	result.Expr = validated
	return result
}

// validateBodySlice validates a contiguous slice of elements as body expressions.
// Returns (body, true) if all elements validated, (nil, false) if any failed.
func validateBodySlice(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	elements []syntax.SyntaxValue,
	start int,
	result *ValidationResult,
) ([]ValidatedExpr, bool) {
	var body []ValidatedExpr
	for i := start; i < len(elements); i++ {
		expr := validateExpr(ctx, env, elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}
	if len(body) != len(elements)-start {
		return nil, false
	}
	return body, true
}

func validateExpr(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	switch e := expr.(type) {
	case *syntax.SyntaxPair:
		// Empty list '() is a self-evaluating literal, not a form.
		// R7RS §4.1.2: The empty list is a literal expression.
		if e.IsEmptyList() {
			return newLiteralExpr(e.SourceContext(), e)
		}
		return validateForm(ctx, env, e, result)
	case *syntax.SyntaxSymbol:
		return &ValidatedSymbol{validatedBase: validatedBase{formName: "@symbol", source: e.SourceContext()}, Symbol: e}
	case *syntax.SyntaxObject:
		return validateSyntaxObject(e, result)
	default:
		// Self-evaluating: numbers, strings, booleans, etc.
		return newLiteralExpr(nil, expr)
	}
}

func validateSyntaxObject(obj *syntax.SyntaxObject, result *ValidationResult) ValidatedExpr {
	wrapped := obj.Unwrap()
	switch wrapped.(type) {
	case *values.Symbol:
		// This shouldn't happen - symbols should be SyntaxSymbol, not SyntaxObject
		// But handle it defensively
		result.addError(obj.SourceContext(), "expression", "unexpected symbol wrapped in SyntaxObject")
		return nil
	default:
		// Self-evaluating literal wrapped in syntax
		return newLiteralExpr(obj.SourceContext(), obj)
	}
}

func validateForm(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	// Get the first element to determine the form type
	car := pair.SyntaxCar()

	// Check if it's a special form by looking at the head
	sym, ok := car.(*syntax.SyntaxSymbol)
	if ok {
		symVal, ok := sym.Unwrap().(*values.Symbol)
		if ok {
			// R7RS §4.2.2: Local variable bindings shadow special forms
			// Check if there's a local variable binding that shadows this form
			hasLocal := env.HasLocalVariableBinding(symVal, sym.Scopes())
			if hasLocal {
				// Local variable shadows the special form - treat as procedure call
				return validateCall(ctx, env, pair, result)
			}

			// Look up the form in the registry
			spec := forms.Lookup(symVal.Key)
			if spec != nil && spec.Validate != nil {
				expr := spec.Validate(ctx, env, pair, result)
				if expr != nil {
					// Override formName only for passthrough forms (prefixed with "@")
					// that haven't been given a proper form name by the validator.
					// This allows validators like validateNamedLet to return a type
					// with a different formName than the keyword (e.g., *ValidatedLetrec
					// with formName "letrec" for the keyword "let").
					fn := expr.FormName()
					if fn == "" || fn[0] == '@' {
						expr.SetFormName(symVal.Key)
					}
				}
				return expr
			}
		}
	}

	// Not a special form - it's a function call
	return validateCall(ctx, env, pair, result)
}

// collectList converts a syntax list to a slice of elements.
// Returns the elements and whether the list is improper.
func collectList(pair *syntax.SyntaxPair) ([]syntax.SyntaxValue, bool) {
	var elements []syntax.SyntaxValue
	var current values.Value = pair

	for {
		if values.IsEmptyList(current) {
			return elements, false // proper list
		}

		p, ok := current.(*syntax.SyntaxPair)
		if !ok {
			// Not a pair - improper list
			return elements, true
		}

		// Get the car element
		car := p.Car()
		carSyntax, ok := car.(syntax.SyntaxValue)
		if ok {
			elements = append(elements, carSyntax)
		} else if car != nil {
			// Wrap non-syntax values
			elements = append(elements, syntax.NewSyntaxObject(car, nil))
		}
		current = p.Cdr()
	}
}

// formPrologue collects list elements, validates the list is proper,
// and checks argument count (excluding the form keyword at elements[0]).
// minArgs and maxArgs define acceptable argument counts.
// Use maxArgs < 0 for unlimited.
func formPrologue(
	pair *syntax.SyntaxPair,
	formName string,
	minArgs, maxArgs int,
	result *ValidationResult,
) (*syntax.SourceContext, []syntax.SyntaxValue, bool) {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, formName, formName+" form must be a proper list")
		return nil, nil, false
	}

	argCount := len(elements) - 1

	if minArgs == maxArgs && maxArgs >= 0 {
		if argCount != minArgs {
			result.addErrorf(source, formName,
				"%s requires exactly %d argument(s), got %d",
				formName, minArgs, argCount)
			return nil, nil, false
		}
	} else {
		if argCount < minArgs {
			result.addErrorf(source, formName,
				"%s requires at least %d argument(s), got %d",
				formName, minArgs, argCount)
			return nil, nil, false
		}
		if maxArgs >= 0 && argCount > maxArgs {
			result.addErrorf(source, formName,
				"%s requires at most %d argument(s), got %d",
				formName, maxArgs, argCount)
			return nil, nil, false
		}
	}

	return source, elements, true
}

// getSourceContext extracts SourceContext from a SyntaxValue if available
func getSourceContext(v syntax.SyntaxValue) *syntax.SourceContext {
	switch sv := v.(type) {
	case *syntax.SyntaxPair:
		return sv.SourceContext()
	case *syntax.SyntaxSymbol:
		return sv.SourceContext()
	case *syntax.SyntaxObject:
		return sv.SourceContext()
	default:
		return nil
	}
}

// isSyntaxSymbol checks if a SyntaxValue is a symbol
func asSyntaxSymbol(v syntax.SyntaxValue) (*syntax.SyntaxSymbol, bool) {
	sym, ok := v.(*syntax.SyntaxSymbol)
	if ok {
		return sym, true
	}
	// Also check for SyntaxObject wrapping a symbol
	obj, ok := v.(*syntax.SyntaxObject)
	if ok {
		_, ok = obj.Unwrap().(*values.Symbol)
		if ok {
			// This is unusual - symbols should be SyntaxSymbol
			return nil, false
		}
	}
	return nil, false
}
