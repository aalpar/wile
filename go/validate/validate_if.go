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

// validateIf validates (if test conseq [alt])
func validateIf(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
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
	test := validateExpr(ctx, elements[1], result)
	conseq := validateExpr(ctx, elements[2], result)

	var alt ValidatedExpr
	if argCount == 3 {
		alt = validateExpr(ctx, elements[3], result)
	}

	// If any sub-validation failed, don't return a valid form
	if test == nil || conseq == nil || (argCount == 3 && alt == nil) {
		return nil
	}

	return &ValidatedIf{
		formName: "if",
		source:   source,
		Test:     test,
		Conseq:   conseq,
		Alt:      alt,
	}
}
