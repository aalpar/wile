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
	"errors"
	"fmt"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// ValidateExpression validates a syntax expression and returns
// a validated form or a list of errors.
// The env parameter provides the environment context for checking local variable
// shadowing of special forms (R7RS §4.2.2).
func ValidateExpression(ctx context.Context, env *environment.EnvironmentFrame, expr syntax.SyntaxValue) *ValidationResult {
	result := &ValidationResult{}
	validated := validateExpr(ctx, env, expr, result)
	result.Expr = validated
	result.finalizeStability()
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
		// Empty list '() is accepted here as a self-evaluating literal.
		// R7RS §4.1.3 makes () a syntax error (a combination needs at least one
		// subexpression); Wile deliberately admits it.
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
			hasLocal := env.HasLocalVariableBinding(symVal, syntax.ScopesOf(sym.Scopes()))
			if hasLocal {
				// Local variable shadows the special form - treat as procedure call
				return validateCall(ctx, env, pair, result)
			}

			// Look up the form in the registry
			spec := forms.RegistryFor(env).Lookup(symVal.Key)
			if spec != nil && spec.Validate != nil {
				expr := spec.Validate(ctx, env, pair, result)
				if expr != nil {
					// Override formName only for passthrough forms (prefixed with "@")
					// that haven't been given a proper form name by the validator.
					// This allows validators like validateNamedLet to return a type
					// with a different formName than the keyword (e.g., *ValidatedLet
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

// collectList converts a syntax list to a slice of elements, reporting whether
// the list is improper. It shares the traversal primitive used by
// syntax.FormParts: a *SyntaxPair's car and cdr are always SyntaxValues (the
// Values array is typed [2]SyntaxValue and SetCar/SetCdr enforce it), so the
// elements need no wrapping and SyntaxForEach's returned tail is non-empty
// exactly when the list is improper.
func collectList(pair *syntax.SyntaxPair) ([]syntax.SyntaxValue, bool) {
	var elements []syntax.SyntaxValue
	tail, _ := pair.SyntaxForEach(context.Background(),
		func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
			elements = append(elements, v)
			return nil
		})
	return elements, !syntax.IsSyntaxEmptyList(tail)
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

	// FormParts counts the form keyword at element 0; formPrologue's bounds
	// are keyword-exclusive ("arguments"), so shift them by one.
	maxLen := maxArgs
	if maxArgs >= 0 {
		maxLen = maxArgs + 1
	}
	elements, err := syntax.FormParts(pair, formName, minArgs+1, maxLen)
	if err != nil {
		var ae *syntax.FormArityError
		if errors.As(err, &ae) {
			result.addError(source, formName, arityArgMessage(ae))
		} else {
			// Structural failure (improper list); no data to restate.
			result.addError(source, formName, formName+" form must be a proper list")
		}
		return nil, nil, false
	}
	return source, elements, true
}

// arityArgMessage restates a FormArityError in the validator's keyword-exclusive
// "argument" vocabulary — the form keyword is not an argument, so element counts
// drop by one. The wording matches the messages formPrologue emitted before it
// delegated structural checks to syntax.FormParts.
func arityArgMessage(ae *syntax.FormArityError) string {
	argCount := ae.Got - 1
	if ae.Min == ae.Max && ae.Max >= 0 {
		return fmt.Sprintf("%s requires exactly %d argument(s), got %d", ae.Name, ae.Min-1, argCount)
	}
	if ae.Got < ae.Min {
		return fmt.Sprintf("%s requires at least %d argument(s), got %d", ae.Name, ae.Min-1, argCount)
	}
	return fmt.Sprintf("%s requires at most %d argument(s), got %d", ae.Name, ae.Max-1, argCount)
}

// getSourceContext extracts SourceContext from a SyntaxValue if available
func getSourceContext(v syntax.SyntaxValue) *syntax.SourceContext {
	switch sv := v.(type) {
	case *syntax.SyntaxPair, *syntax.SyntaxSymbol, *syntax.SyntaxObject:
		return sv.SourceContext()
	default:
		return nil
	}
}
