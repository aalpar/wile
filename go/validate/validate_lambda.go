package validate

import (
	"wile/syntax"
)

// validateLambda validates (lambda (params...) body...)
func validateLambda(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "lambda", "lambda form must be a proper list")
		return nil
	}

	// elements[0] is 'lambda', need params and at least one body expr
	if len(elements) < 3 {
		result.addErrorf(source, "lambda", "lambda requires parameters and at least one body expression, got %d parts", len(elements)-1)
		return nil
	}

	// Validate parameters
	params := validateParams(elements[1], "lambda", result)

	// Validate body - must have at least one expression
	var body []ValidatedExpr
	for i := 2; i < len(elements); i++ {
		expr := validateExpr(elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}

	// If any validation failed, return nil
	if len(body) != len(elements)-2 {
		return nil
	}

	return &ValidatedLambda{
		source: source,
		Params: params,
		Body:   body,
	}
}
