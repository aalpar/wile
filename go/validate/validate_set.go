package validate

import (
	"skeme/syntax"
)

// validateSetBang validates (set! name expr)
func validateSetBang(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "set!", "set! form must be a proper list")
		return nil
	}

	// elements[0] is 'set!', need exactly name and value
	if len(elements) != 3 {
		result.addErrorf(source, "set!", "set! requires exactly 2 arguments, got %d", len(elements)-1)
		return nil
	}

	// Second element must be a symbol
	name, ok := asSyntaxSymbol(elements[1])
	if !ok {
		result.addErrorf(source, "set!", "expected symbol as first argument to set!, got %T", elements[1])
		return nil
	}

	// Validate the value expression
	value := validateExpr(elements[2], result)
	if value == nil {
		return nil
	}

	return &ValidatedSetBang{
		source: source,
		Name:   name,
		Value:  value,
	}
}
