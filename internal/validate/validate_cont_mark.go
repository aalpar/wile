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

// validateWithContinuationMark validates (with-continuation-mark key val body)
func validateWithContinuationMark(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "with-continuation-mark", 3, 3, result)
	if !ok {
		return nil
	}

	key := validateExpr(ctx, env, elements[1], result)
	val := validateExpr(ctx, env, elements[2], result)
	body := validateExpr(ctx, env, elements[3], result)

	if key == nil || val == nil || body == nil {
		return nil
	}

	return &ValidatedWithContinuationMark{
		validatedBase: validatedBase{formName: "with-continuation-mark", source: source},
		Key:           key,
		Val:           val,
		Body:          body,
	}
}
