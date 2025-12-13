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
	"wile/forms"
	"wile/syntax"
	"wile/values"
)

// ValidateExpression validates a syntax expression and returns
// a validated form or a list of errors
func ValidateExpression(ctx context.Context, expr syntax.SyntaxValue) *ValidationResult {
	result := &ValidationResult{}
	validated := validateExpr(ctx, expr, result)
	result.Expr = validated
	return result
}

func validateExpr(ctx context.Context, expr syntax.SyntaxValue, result *ValidationResult) ValidatedExpr {
	switch e := expr.(type) {
	case *syntax.SyntaxPair:
		return validateForm(ctx, e, result)
	case *syntax.SyntaxSymbol:
		return &ValidatedSymbol{source: e.SourceContext(), formName: "@symbol", Symbol: e}
	case *syntax.SyntaxObject:
		return validateSyntaxObject(e, result)
	default:
		// Self-evaluating: numbers, strings, booleans, etc.
		return &ValidatedLiteral{source: nil, formName: "@literal", Value: expr}
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
		return &ValidatedLiteral{source: obj.SourceContext(), formName: "@literal", Value: obj}
	}
}

func validateForm(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	// Get the first element to determine the form type
	car := pair.SyntaxCar()

	// Check if it's a special form by looking at the head
	if sym, ok := car.(*syntax.SyntaxSymbol); ok {
		symVal, ok := sym.Unwrap().(*values.Symbol)
		if ok {
			// Look up the form in the registry
			if spec := forms.Lookup(symVal.Key); spec != nil && spec.Validate != nil {
				validated := spec.Validate(ctx, pair, result)
				if validated == nil {
					return nil
				}
				return validated.(ValidatedExpr)
			}
		}
	}

	// Not a special form - it's a function call
	return validateCall(nil, pair, result)
}

// collectList converts a syntax list to a slice of elements.
// Returns the elements and whether the list is improper.
func collectList(pair *syntax.SyntaxPair) ([]syntax.SyntaxValue, bool) {
	var elements []syntax.SyntaxValue
	var current values.Value = pair

	for {
		p, ok := current.(*syntax.SyntaxPair)
		if !ok {
			// Not a pair - improper list
			return elements, true
		}

		// Check for empty list marker (SyntaxPair with nil values)
		if p.IsEmptyList() {
			return elements, false // proper list
		}

		// Get the car element
		car := p.Car()
		if carSyntax, ok := car.(syntax.SyntaxValue); ok {
			elements = append(elements, carSyntax)
		} else if car != nil {
			// Wrap non-syntax values
			elements = append(elements, syntax.NewSyntaxObject(car, nil))
		}
		current = p.Cdr()
	}
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
	if sym, ok := v.(*syntax.SyntaxSymbol); ok {
		return sym, true
	}
	// Also check for SyntaxObject wrapping a symbol
	if obj, ok := v.(*syntax.SyntaxObject); ok {
		if _, ok := obj.Unwrap().(*values.Symbol); ok {
			// This is unusual - symbols should be SyntaxSymbol
			return nil, false
		}
	}
	return nil, false
}
