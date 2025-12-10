package validate

import (
	"skeme/syntax"
)

// validateQuote validates (quote datum)
func validateQuote(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "quote", "quote form must be a proper list")
		return nil
	}

	// elements[0] is 'quote', need exactly one datum
	if len(elements) != 2 {
		result.addErrorf(source, "quote", "quote requires exactly 1 argument, got %d", len(elements)-1)
		return nil
	}

	// The datum can be any syntax value - no further validation needed
	return &ValidatedQuote{
		source: source,
		Datum:  elements[1],
	}
}

// validateQuasiquote validates (quasiquote template)
// Note: The template is not deeply validated here because quasiquote
// has complex runtime semantics with nested unquote/unquote-splicing
func validateQuasiquote(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "quasiquote", "quasiquote form must be a proper list")
		return nil
	}

	// elements[0] is 'quasiquote', need exactly one template
	if len(elements) != 2 {
		result.addErrorf(source, "quasiquote", "quasiquote requires exactly 1 argument, got %d", len(elements)-1)
		return nil
	}

	// The template is passed through - quasiquote has complex compile-time semantics
	return &ValidatedQuasiquote{
		source:   source,
		Template: elements[1],
	}
}
