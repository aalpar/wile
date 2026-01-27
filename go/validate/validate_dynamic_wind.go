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

	"wile/environment"
	"wile/syntax"
)

// validateDynamicWind validates (dynamic-wind before thunk after)
//
// R7RS §6.10: dynamic-wind calls thunk without arguments, returning the result(s).
// Before is called whenever execution enters the dynamic extent of the call to thunk,
// and after is called whenever it exits.
func validateDynamicWind(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source := pair.SourceContext()

	// Collect all elements into a slice for easier validation
	elements, improper := collectList(pair)
	if improper {
		result.addError(source, "dynamic-wind", "dynamic-wind form must be a proper list")
		return nil
	}

	// elements[0] is 'dynamic-wind' symbol, actual args start at [1]
	argCount := len(elements) - 1

	if argCount != 3 {
		result.addErrorf(source, "dynamic-wind", "dynamic-wind requires exactly 3 arguments, got %d", argCount)
		return nil
	}

	// Validate sub-expressions (continue even if some fail to collect all errors)
	before := validateExpr(ctx, env, elements[1], result)
	thunk := validateExpr(ctx, env, elements[2], result)
	after := validateExpr(ctx, env, elements[3], result)

	// If any sub-validation failed, don't return a valid form
	if before == nil || thunk == nil || after == nil {
		return nil
	}

	return &ValidatedDynamicWind{
		formName: "dynamic-wind",
		source:   source,
		Before:   before,
		Thunk:    thunk,
		After:    after,
	}
}
