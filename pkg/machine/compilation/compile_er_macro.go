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

package compilation

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// compileERMacroTransformer compiles (er-macro-transformer <lambda-expr>) into
// an *ERMacroTransformer wrapping the 3-arg closure and definition-site environment.
func compileERMacroTransformer(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	erForm *syntax.SyntaxPair,
	evaluator machine.MacroEvaluator,
) (*ERMacroTransformer, error) {
	// erForm is (er-macro-transformer <lambda>) — exactly two elements counting
	// the keyword; parts[1] is the lambda expression.
	parts, err := syntax.FormParts(erForm, "er-macro-transformer", 2, 2)
	if err != nil {
		return nil, wrapSourcedError(erForm.SourceContext(), err)
	}
	lambdaExpr := parts[1]

	// Compile and evaluate the lambda to get a machine.MachineClosure.
	// compileAndEvalLambdaTransformer handles expansion, compilation, and evaluation.
	closure, err := compileAndEvalLambdaTransformer(ctx, env, lambdaExpr, evaluator)
	if err != nil {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			err,
			"er-macro-transformer: failed to compile lambda",
		))
	}

	// Validate arity: the lambda must be callable with 3 arguments (form, rename,
	// compare); a variadic lambda qualifies.
	if !closure.AcceptsArity(3) {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			werr.ErrWrongNumberOfArguments,
			"er-macro-transformer: lambda must accept exactly 3 arguments (form rename compare)",
		))
	}

	// Wrap in ERMacroTransformer with the definition-site expand environment
	defEnv := env.Expand()
	return NewERMacroTransformer(closure, defEnv), nil
}
