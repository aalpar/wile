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

// validateQuote validates (quote datum)
func validateQuote(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "quote", 1, 1, result)
	if !ok {
		return nil
	}

	// The datum can be any syntax value - no further validation needed
	return &ValidatedQuote{
		validatedBase: validatedBase{formName: "quote", source: source},
		Datum:         elements[1],
	}
}

// validateQuasiquote validates (quasiquote template)
// Note: The template is not deeply validated here because quasiquote
// has complex runtime semantics with nested unquote/unquote-splicing
func validateQuasiquote(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "quasiquote", 1, 1, result)
	if !ok {
		return nil
	}

	// The template is passed through - quasiquote has complex compile-time semantics
	return &ValidatedQuasiquote{
		validatedBase: validatedBase{formName: "quasiquote", source: source},
		Template:      elements[1],
	}
}
