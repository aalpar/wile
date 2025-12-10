package validate

import (
	"wile/syntax"
)

// validateBegin validates (begin expr...)
func validateBegin(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "begin", "begin form must be a proper list")
		return nil
	}

	// elements[0] is 'begin', can have zero or more expressions
	// R7RS allows (begin) with no expressions (returns unspecified value)
	var exprs []ValidatedExpr
	for i := 1; i < len(elements); i++ {
		expr := validateExpr(elements[i], result)
		if expr != nil {
			exprs = append(exprs, expr)
		}
	}

	// If any validation failed, return nil
	expectedCount := len(elements) - 1
	if len(exprs) != expectedCount && expectedCount > 0 {
		return nil
	}

	return &ValidatedBegin{
		source: source,
		Exprs:  exprs,
	}
}
