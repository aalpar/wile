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

// validateLambda validates (lambda (params...) body...)
func validateLambda(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
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
	params := validateParams(elements[1], result)

	// Validate body - must have at least one expression
	var body []ValidatedExpr
	for i := 2; i < len(elements); i++ {
		expr := validateExpr(ctx, elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}

	// If any validation failed, return nil
	if len(body) != len(elements)-2 {
		return nil
	}

	return &ValidatedLambda{
		formName: "lambda",
		source:   source,
		Params:   params,
		Body:     body,
	}
}
