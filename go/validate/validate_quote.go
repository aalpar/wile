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
	"wile/syntax"
)

// validateQuote validates (quote datum)
func validateQuote(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
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
		formName: "quote",
		source:   source,
		Datum:    elements[1],
	}
}

// validateQuasiquote validates (quasiquote template)
// Note: The template is not deeply validated here because quasiquote
// has complex runtime semantics with nested unquote/unquote-splicing
func validateQuasiquote(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
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
		formName: "quasiquote",
		source:   source,
		Template: elements[1],
	}
}
