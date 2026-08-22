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
)

// validateIf validates (if test conseq [alt])
func validateIf(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "if", 2, 3, result)
	if !ok {
		return nil
	}

	// Validate sub-expressions (continue even if some fail to collect all errors)
	test := validateExpr(ctx, env, elements[1], result)
	conseq := validateExpr(ctx, env, elements[2], result)

	var alt ValidatedExpr
	if len(elements)-1 == 3 {
		alt = validateExpr(ctx, env, elements[3], result)
	}

	// If any sub-validation failed, don't return a valid form
	if test == nil || conseq == nil || (len(elements)-1 == 3 && alt == nil) {
		return nil
	}

	return &ValidatedIf{
		formName: "if", source: source,
		Test:   test,
		Conseq: conseq,
		Alt:    alt,
	}
}
