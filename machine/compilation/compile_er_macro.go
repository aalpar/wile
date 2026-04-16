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

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

// compileERMacroTransformer compiles (er-macro-transformer <lambda-expr>) into
// an *ERMacroTransformer wrapping the 3-arg closure and definition-site environment.
func compileERMacroTransformer(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	erForm *syntax.SyntaxPair,
	evaluator machine.MacroEvaluator,
) (*ERMacroTransformer, error) {
	// Extract the lambda expression from (er-macro-transformer <lambda>)
	cdr := erForm.SyntaxCdr()
	argsPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdr) {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			werr.ErrInvalidSyntax,
			"er-macro-transformer: expected a lambda expression",
		))
	}

	lambdaExpr := argsPair.SyntaxCar()
	if lambdaExpr == nil {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			werr.ErrUnexpectedNil,
			"er-macro-transformer: lambda expression is nil",
		))
	}

	// Reject extra arguments: (er-macro-transformer <lambda>) must have exactly one arg.
	rest := argsPair.SyntaxCdr()
	if !syntax.IsSyntaxEmptyList(rest) {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			werr.ErrInvalidSyntax,
			"er-macro-transformer: expected exactly one argument (lambda expression), got extra forms",
		))
	}

	// Compile and evaluate the lambda to get a machine.MachineClosure.
	// compileAndEvalLambdaTransformer handles expansion, compilation, and evaluation.
	closure, err := compileAndEvalLambdaTransformer(ctx, env, lambdaExpr, evaluator)
	if err != nil {
		return nil, wrapSourcedError(erForm.SourceContext(), werr.WrapForeignErrorf(
			err,
			"er-macro-transformer: failed to compile lambda",
		))
	}

	// Validate arity: the lambda must accept exactly 3 parameters (form, rename, compare)
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
