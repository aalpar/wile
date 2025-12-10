package validate

import (
	"skeme/syntax"
	"skeme/values"
)

// validateDefineSyntax validates (define-syntax keyword transformer)
// Returns a ValidatedLiteral wrapping the original form since the compiler
// has specialized handling for this.
func validateDefineSyntax(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "define-syntax", "define-syntax form must be a proper list")
		return nil
	}

	// elements[0] is 'define-syntax', need keyword and transformer
	if len(elements) != 3 {
		result.addErrorf(source, "define-syntax", "define-syntax requires exactly 2 arguments (keyword and transformer), got %d", len(elements)-1)
		return nil
	}

	// Second element must be a symbol (the keyword to bind)
	if _, ok := asSyntaxSymbol(elements[1]); !ok {
		result.addError(getSourceContext(elements[1]), "define-syntax", "define-syntax keyword must be a symbol")
		return nil
	}

	// Third element is the transformer - don't validate deeply since it could be
	// syntax-rules, a variable reference, or any expression
	// The compiler/expander handles transformer validation

	// Return as literal - compiler handles the rest
	return &ValidatedLiteral{source: source, Value: pair}
}

// validateSyntaxRules validates (syntax-rules (literals...) clause...)
// Returns a ValidatedLiteral wrapping the original form.
func validateSyntaxRules(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "syntax-rules", "syntax-rules form must be a proper list")
		return nil
	}

	// elements[0] is 'syntax-rules', need at least literals list
	if len(elements) < 2 {
		result.addError(source, "syntax-rules", "syntax-rules requires at least a literals list")
		return nil
	}

	// Second element should be the literals list (could be empty)
	literalsExpr := elements[1]
	if literalsPair, ok := literalsExpr.(*syntax.SyntaxPair); ok {
		if !literalsPair.IsEmptyList() && !literalsPair.IsList() {
			result.addError(getSourceContext(literalsExpr), "syntax-rules", "syntax-rules literals must be a proper list")
			return nil
		}
		// Validate each literal is a symbol
		if !literalsPair.IsEmptyList() {
			_, err := syntax.SyntaxForEach(literalsPair, func(i int, hasNext bool, v syntax.SyntaxValue) error {
				if _, ok := asSyntaxSymbol(v); !ok {
					result.addErrorf(getSourceContext(v), "syntax-rules", "literal must be a symbol, got %T", v)
				}
				return nil
			})
			if err != nil {
				result.addError(source, "syntax-rules", "error iterating literals list")
				return nil
			}
		}
	} else {
		result.addError(getSourceContext(literalsExpr), "syntax-rules", "syntax-rules literals must be a list")
		return nil
	}

	// Validate each clause has pattern and template
	for i := 2; i < len(elements); i++ {
		clausePair, ok := elements[i].(*syntax.SyntaxPair)
		if !ok {
			result.addErrorf(getSourceContext(elements[i]), "syntax-rules", "clause %d must be a list", i-1)
			continue
		}
		clauseElements, improper := collectList(clausePair)
		if improper {
			result.addErrorf(getSourceContext(elements[i]), "syntax-rules", "clause %d must be a proper list", i-1)
			continue
		}
		if len(clauseElements) != 2 {
			result.addErrorf(getSourceContext(elements[i]), "syntax-rules", "clause %d must have exactly pattern and template, got %d elements", i-1, len(clauseElements))
		}
	}

	// Return as literal - compiler handles the rest
	return &ValidatedLiteral{source: source, Value: pair}
}

// validateImport validates (import import-set...)
// Returns a ValidatedLiteral wrapping the original form.
func validateImport(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "import", "import form must be a proper list")
		return nil
	}

	// elements[0] is 'import', need at least one import set
	if len(elements) < 2 {
		result.addError(source, "import", "import requires at least one import-set")
		return nil
	}

	// Basic validation - each import-set should be a list
	for i := 1; i < len(elements); i++ {
		if _, ok := elements[i].(*syntax.SyntaxPair); !ok {
			if _, ok := elements[i].(*syntax.SyntaxSymbol); !ok {
				result.addErrorf(getSourceContext(elements[i]), "import", "import-set %d must be a list or symbol", i)
			}
		}
	}

	return &ValidatedLiteral{source: source, Value: pair}
}

// validateExport validates (export export-spec...)
// Returns a ValidatedLiteral wrapping the original form.
func validateExport(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "export", "export form must be a proper list")
		return nil
	}

	// Export with no specs is valid (exports nothing)
	// Each export-spec should be a symbol or (rename ...)
	for i := 1; i < len(elements); i++ {
		spec := elements[i]
		if _, ok := asSyntaxSymbol(spec); ok {
			continue // Simple export
		}
		if specPair, ok := spec.(*syntax.SyntaxPair); ok {
			// Could be (rename internal external)
			if specPair.IsList() {
				continue
			}
		}
		result.addErrorf(getSourceContext(spec), "export", "export-spec %d must be a symbol or rename form", i)
	}

	return &ValidatedLiteral{source: source, Value: pair}
}

// validateDefineLibrary validates (define-library (name...) declaration...)
// Returns a ValidatedLiteral wrapping the original form.
func validateDefineLibrary(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "define-library", "define-library form must be a proper list")
		return nil
	}

	// elements[0] is 'define-library', need at least library name
	if len(elements) < 2 {
		result.addError(source, "define-library", "define-library requires a library name")
		return nil
	}

	// Library name must be a list
	namePair, ok := elements[1].(*syntax.SyntaxPair)
	if !ok {
		result.addError(getSourceContext(elements[1]), "define-library", "library name must be a list")
		return nil
	}
	if !namePair.IsList() {
		result.addError(getSourceContext(elements[1]), "define-library", "library name must be a proper list")
		return nil
	}

	// Validate library name components are symbols or integers
	_, err := syntax.SyntaxForEach(namePair, func(i int, hasNext bool, v syntax.SyntaxValue) error {
		if _, ok := asSyntaxSymbol(v); ok {
			return nil
		}
		if obj, ok := v.(*syntax.SyntaxObject); ok {
			if _, ok := obj.Unwrap().(*values.Integer); ok {
				return nil
			}
		}
		result.addErrorf(getSourceContext(v), "define-library", "library name component must be symbol or integer")
		return nil
	})
	if err != nil {
		result.addError(source, "define-library", "error validating library name")
		return nil
	}

	return &ValidatedLiteral{source: source, Value: pair}
}

// validateInclude validates (include filename...)
// Returns a ValidatedLiteral wrapping the original form.
func validateInclude(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "include", "include form must be a proper list")
		return nil
	}

	// elements[0] is 'include', need at least one filename
	if len(elements) < 2 {
		result.addError(source, "include", "include requires at least one filename")
		return nil
	}

	// Each argument should be a string
	for i := 1; i < len(elements); i++ {
		if obj, ok := elements[i].(*syntax.SyntaxObject); ok {
			if _, ok := obj.Unwrap().(*values.String); ok {
				continue
			}
		}
		result.addErrorf(getSourceContext(elements[i]), "include", "include argument %d must be a string", i)
	}

	return &ValidatedLiteral{source: source, Value: pair}
}

// validateCondExpand validates (cond-expand clause...)
// Returns a ValidatedLiteral wrapping the original form.
func validateCondExpand(pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "cond-expand", "cond-expand form must be a proper list")
		return nil
	}

	// cond-expand with no clauses is technically valid but useless
	// Each clause should be a list starting with a feature requirement
	for i := 1; i < len(elements); i++ {
		clausePair, ok := elements[i].(*syntax.SyntaxPair)
		if !ok {
			result.addErrorf(getSourceContext(elements[i]), "cond-expand", "clause %d must be a list", i)
			continue
		}
		if !clausePair.IsList() {
			result.addErrorf(getSourceContext(elements[i]), "cond-expand", "clause %d must be a proper list", i)
		}
	}

	return &ValidatedLiteral{source: source, Value: pair}
}
