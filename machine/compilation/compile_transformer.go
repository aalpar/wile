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
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

const (
	// TransformerSyntaxRules is the leading symbol for (syntax-rules ...) transformers.
	TransformerSyntaxRules = "syntax-rules"
	// TransformerERMacro is the leading symbol for (er-macro-transformer ...) transformers.
	TransformerERMacro = "er-macro-transformer"
	// FormDefineSyntax is the form name for (define-syntax ...) definitions.
	FormDefineSyntax = "define-syntax"
)

// compileTransformerToMachineClosure compiles a define-syntax transformer expression
// into a values.Value for storage in the expand environment.
//
// Supports:
//   - (syntax-rules ...) - compiled directly via CompileSyntaxRules → *machine.MachineClosure
//   - (lambda (stx) ...) - compiled and evaluated to produce a *machine.MachineClosure
//   - (er-macro-transformer (lambda (form rename compare) ...)) → *ERMacroTransformer
//
// The env parameter is used for compilation (so transformers can see local bindings),
// while the resulting value is intended to be stored in env.Expand().
func compileTransformerToMachineClosure(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	transformerExpr syntax.SyntaxValue,
	libraryScope *syntax.Scope,
	evaluator machine.MacroEvaluator,
) (values.Value, error) {
	transformerPair, ok := transformerExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "define-syntax: transformer must be a list"))
	}

	car := transformerPair.SyntaxCar()
	if car == nil {
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: transformer has empty car"))
	}

	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol"))
	}

	symVal := sym.Unwrap()
	if symVal == nil {
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: transformer symbol is nil"))
	}

	symbol, ok := symVal.(*values.Symbol)
	if !ok {
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol"))
	}

	switch symbol.Key {
	case TransformerSyntaxRules:
		return CompileSyntaxRules(ctx, env, transformerPair, libraryScope)

	case "lambda":
		return compileAndEvalLambdaTransformer(ctx, env, transformerPair, evaluator)

	case TransformerERMacro:
		return compileERMacroTransformer(ctx, env, transformerPair, evaluator)

	default:
		return nil, wrapSourcedError(transformerExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: unsupported transformer type %q (expected syntax-rules, lambda, or er-macro-transformer)", symbol.Key))
	}
}

// compileAndEvalLambdaTransformer compiles a lambda expression and evaluates it at
// compile time to produce a closure that can be used as a syntax transformer.
//
// Unlike other ExpandAndCompile callers that operate on runtime environments,
// this function uses env.Expand() — transformers see expansion-time bindings,
// not runtime bindings. The extra EvalTemplate step (absent from ExpandAndCompile)
// executes the compiled lambda at compile time to produce the transformer closure.
func compileAndEvalLambdaTransformer(ctx context.Context, env *environment.EnvironmentFrame, lambdaExpr syntax.SyntaxValue, evaluator machine.MacroEvaluator) (*machine.MachineClosure, error) {
	expandEnv := env.Expand()

	tpl, err := ExpandAndCompile(ctx, expandEnv, lambdaExpr, nil, DefaultInlineThreshold)
	if err != nil {
		return nil, wrapSourcedError(lambdaExpr.SourceContext(), werr.WrapForeignErrorf(err, "transformer"))
	}

	result, err := evaluator.EvalTemplate(ctx, tpl, expandEnv)
	if err != nil {
		return nil, wrapSourcedError(lambdaExpr.SourceContext(), werr.WrapForeignErrorf(err, "error evaluating transformer"))
	}

	closure, ok := result.(*machine.MachineClosure)
	if !ok {
		return nil, wrapSourcedError(lambdaExpr.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotAProcedure, "define-syntax: transformer must evaluate to a procedure, got %T", result))
	}

	return closure, nil
}
