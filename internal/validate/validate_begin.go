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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// validateBegin validates (begin expr...)
func validateBegin(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "begin", 0, -1, result)
	if !ok {
		return nil
	}

	// R7RS allows (begin) with no expressions (returns unspecified value)
	var body []ValidatedExpr
	for i := 1; i < len(elements); i++ {
		expr := validateExpr(ctx, env, elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}

	// If any validation failed, return nil
	expectedCount := len(elements) - 1
	if len(body) != expectedCount && expectedCount > 0 {
		return nil
	}

	return &ValidatedBegin{
		formName: "begin",
		source:   source,
		body:     body,
	}
}
