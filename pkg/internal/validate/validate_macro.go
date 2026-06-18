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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// validateDefineSyntax validates (define-syntax keyword transformer)
// Returns a ValidatedLiteral wrapping the original form since the compiler
// has specialized handling for this.
func validateDefineSyntax(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "define-syntax", 2, 2, result)
	if !ok {
		return nil
	}

	// Second element must be a symbol (the keyword to bind)
	_, isSym := elements[1].(*syntax.SyntaxSymbol)
	if !isSym {
		result.addError(getSourceContext(elements[1]), "define-syntax", "define-syntax keyword must be a symbol")
		return nil
	}

	// Third element is the transformer - don't validate deeply since it could be
	// syntax-rules, a variable reference, or any expression
	// The compiler/expander handles transformer validation

	// Return as literal - compiler handles the rest
	return newLiteralExpr(source, pair)
}

// validateSyntaxRules validates (syntax-rules (literals...) clause...)
// Returns a ValidatedLiteral wrapping the original form.
func validateSyntaxRules(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "syntax-rules", 1, -1, result)
	if !ok {
		return nil
	}

	// Second element should be the literals list (could be empty)
	literalsExpr := elements[1]
	if syntax.IsSyntaxEmptyList(literalsExpr) { //nolint:revive // empty block: empty literals list is valid
	} else {
		literalsPair, ok := literalsExpr.(*syntax.SyntaxPair)
		if ok {
			if !literalsPair.IsEmptyList() && !literalsPair.IsList() {
				result.addError(getSourceContext(literalsExpr), "syntax-rules", "syntax-rules literals must be a proper list")
				return nil
			}
			// Validate each literal is a symbol
			if !literalsPair.IsEmptyList() {
				_, err := syntax.SyntaxForEach(ctx, literalsPair, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
					_, ok := v.(*syntax.SyntaxSymbol)
					if !ok {
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
	return newLiteralExpr(source, pair)
}

// validateImport validates (import import-set...)
// Returns a ValidatedLiteral wrapping the original form.
func validateImport(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "import", 1, -1, result)
	if !ok {
		return nil
	}

	// Basic validation - each import-set should be a list
	for i := 1; i < len(elements); i++ {
		_, ok := elements[i].(*syntax.SyntaxPair)
		if !ok {
			_, ok = elements[i].(*syntax.SyntaxSymbol)
			if !ok {
				result.addErrorf(getSourceContext(elements[i]), "import", "import-set %d must be a list or symbol", i)
			}
		}
	}

	return newLiteralExpr(source, pair)
}

// validateExport validates (export export-spec...)
// Returns a ValidatedLiteral wrapping the original form.
func validateExport(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "export", 0, -1, result)
	if !ok {
		return nil
	}

	// Export with no specs is valid (exports nothing)
	// Each export-spec should be a symbol or (rename ...)
	for i := 1; i < len(elements); i++ {
		spec := elements[i]
		_, ok := spec.(*syntax.SyntaxSymbol)
		if ok {
			continue // Simple export
		}
		specPair, ok := spec.(*syntax.SyntaxPair)
		if ok {
			// Could be (rename internal external)
			if specPair.IsList() {
				continue
			}
		}
		result.addErrorf(getSourceContext(spec), "export", "export-spec %d must be a symbol or rename form", i)
	}

	return newLiteralExpr(source, pair)
}

// validateDefineLibrary validates (define-library (name...) declaration...)
// Returns a ValidatedLiteral wrapping the original form.
func validateDefineLibrary(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "define-library", 1, -1, result)
	if !ok {
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
	_, err := syntax.SyntaxForEach(ctx, namePair, func(_ context.Context, _ int, _ bool, v syntax.SyntaxValue) error {
		_, ok := v.(*syntax.SyntaxSymbol)
		if ok {
			return nil
		}
		obj, ok := v.(*syntax.SyntaxObject)
		if ok {
			_, ok = obj.Unwrap().(*values.Integer)
			if ok {
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

	return newLiteralExpr(source, pair)
}

// validateInclude validates (include filename...)
// Returns a ValidatedLiteral wrapping the original form.
func validateInclude(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "include", 1, -1, result)
	if !ok {
		return nil
	}

	// Each argument should be a string
	for i := 1; i < len(elements); i++ {
		obj, ok := elements[i].(*syntax.SyntaxObject)
		if ok {
			_, ok = obj.Unwrap().(*values.String)
			if ok {
				continue
			}
		}
		result.addErrorf(getSourceContext(elements[i]), "include", "include argument %d must be a string", i)
	}

	return newLiteralExpr(source, pair)
}

// validateCondExpand validates (cond-expand clause...)
// Returns a ValidatedLiteral wrapping the original form.
func validateCondExpand(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "cond-expand", 0, -1, result)
	if !ok {
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

	return newLiteralExpr(source, pair)
}
