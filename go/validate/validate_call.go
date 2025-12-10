package validate

import (
	"wile/syntax"
)

// validateCall validates (proc arg...)
func validateCall(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "call", "function call must be a proper list")
		return nil
	}

	// Need at least the procedure
	if len(elements) < 1 {
		result.addError(source, "call", "empty application")
		return nil
	}

	// Validate the procedure expression
	proc := validateExpr(elements[0], result)

	// Validate all arguments
	var args []ValidatedExpr
	for i := 1; i < len(elements); i++ {
		arg := validateExpr(elements[i], result)
		if arg != nil {
			args = append(args, arg)
		}
	}

	// If proc or any arg validation failed, return nil
	expectedArgCount := len(elements) - 1
	if proc == nil || (len(args) != expectedArgCount && expectedArgCount > 0) {
		return nil
	}

	return &ValidatedCall{
		source: source,
		Proc:   proc,
		Args:   args,
	}
}
