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

// validateSetBang validates (set! name expr)
func validateSetBang(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "set!", 2, 2, result)
	if !ok {
		return nil
	}

	// Second element must be a symbol
	name, ok := asSyntaxSymbol(elements[1])
	if !ok {
		result.addErrorf(source, "set!", "expected symbol as first argument to set!, got %T", elements[1])
		return nil
	}

	// Validate the value expression
	value := validateExpr(ctx, env, elements[2], result)
	if value == nil {
		return nil
	}

	return &ValidatedSetBang{
		formName: "set!",
		source:   source,
		Name:     name,
		subExp:   value,
	}
}
