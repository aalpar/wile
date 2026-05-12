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
	"slices"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
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

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	docstring, body := extractDocstring(body)

	return &ValidatedLambda{
		validatedBase:     validatedBase{formName: "lambda", source: source},
		validatedProcBase: validatedProcBase{params: params, body: body, docstring: docstring},
	}
}

// createLambdaValidationEnv creates a child environment with lambda parameters
// bound as local variables. This mirrors what the expander and compiler do,
// enabling validation to correctly detect when parameters shadow special forms.
//
// Always creates a fresh child frame, even when params has zero required and
// no rest. Uses createChildEnvWithSymbols (the always-frame primitive) rather
// than extendEnvWithSymbols (which short-circuits on empty input — wrong for
// lambda).
func createLambdaValidationEnv(env *environment.EnvironmentFrame, params *ValidatedParams) *environment.EnvironmentFrame {
	if env == nil || params == nil {
		return env
	}
	syms := params.Required
	if params.Rest != nil {
		syms = append(slices.Clone(syms), params.Rest)
	}
	return createChildEnvWithSymbols(env, syms)
}

// extractDocstring checks whether the first body expression is a string
// literal (Guile-style docstring). If so, returns the string and the
// remaining body. The string must not be the only expression — a body
// of just "hello" is a return value, not documentation.
func extractDocstring(body []ValidatedExpr) (string, []ValidatedExpr) {
	if len(body) < 2 {
		return "", body
	}
	lit, ok := body[0].(*ValidatedLiteral)
	if !ok {
		return "", body
	}
	str, ok := lit.Value.UnwrapAll().(*values.String)
	if !ok {
		return "", body
	}
	return str.Value, body[1:]
}
