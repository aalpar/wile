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

// validateLambda validates (lambda (params...) body...)
func validateLambda(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "lambda", 2, -1, result)
	if !ok {
		return nil
	}

	// Validate parameters
	params := validateParams(elements[1], result)

	// Create a child environment with parameters bound as local variables.
	// This enables proper shadowing detection: lambda parameters shadow
	// outer bindings including special forms (R7RS §4.2.2).
	childEnv := createLambdaValidationEnv(env, params)

	// Validate body - must have at least one expression
	var body []ValidatedExpr
	for i := 2; i < len(elements); i++ {
		expr := validateExpr(ctx, childEnv, elements[i], result)
		if expr != nil {
			body = append(body, expr)
		}
	}

	// If any validation failed, return nil
	if len(body) != len(elements)-2 {
		return nil
	}

	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda", source: source},
		params:        params,
		body:          body,
	}
}

// createLambdaValidationEnv creates a child environment with lambda parameters
// bound as local variables. This mirrors what the expander and compiler do,
// enabling validation to correctly detect when parameters shadow special forms.
func createLambdaValidationEnv(env *environment.EnvironmentFrame, params *ValidatedParams) *environment.EnvironmentFrame {
	if env == nil || params == nil {
		return env
	}

	// Create child environment with local bindings for parameters
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	// Bind required parameters
	for _, paramSym := range params.Required {
		// paramSym is already a *syntax.SyntaxSymbol
		childEnv.MaybeCreateLocalBindingWithScopes(
			paramSym.Sym,
			environment.BindingTypeVariable,
			paramSym.Scopes(),
		)
	}

	// Bind rest parameter if present
	if params.Rest != nil {
		childEnv.MaybeCreateLocalBindingWithScopes(
			params.Rest.Sym,
			environment.BindingTypeVariable,
			params.Rest.Scopes(),
		)
	}

	return childEnv
}
