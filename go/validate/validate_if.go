package validate

import (
	"wile/syntax"
)

// validateIf validates (if test conseq [alt])
func validateIf(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice for easier validation
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "if", "if form must be a proper list")
		return nil
	}

	// elements[0] is 'if' symbol, actual args start at [1]
	argCount := len(elements) - 1

	if argCount < 2 {
		result.addErrorf(source, "if", "if requires at least 2 arguments, got %d", argCount)
		return nil
	}

	if argCount > 3 {
		result.addErrorf(source, "if", "if requires at most 3 arguments, got %d", argCount)
		return nil
	}

	// Validate sub-expressions (continue even if some fail to collect all errors)
	test := validateExpr(elements[1], result)
	conseq := validateExpr(elements[2], result)

	var alt ValidatedExpr
	if argCount == 3 {
		alt = validateExpr(elements[3], result)
	}

	// If any sub-validation failed, don't return a valid form
	if test == nil || conseq == nil || (argCount == 3 && alt == nil) {
		return nil
	}

	return &ValidatedIf{
		source: source,
		Test:   test,
		Conseq: conseq,
		Alt:    alt,
	}
}
