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

package machine

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// compileTransformerToMachineClosure compiles a define-syntax transformer expression
// into a values.Value for storage in the expand environment.
//
// Supports:
//   - (syntax-rules ...) - compiled directly via CompileSyntaxRules → *MachineClosure
//   - (lambda (stx) ...) - compiled and evaluated to produce a *MachineClosure
//   - (er-macro-transformer (lambda (form rename compare) ...)) → *ERMacroTransformer
//
// The env parameter is used for compilation (so transformers can see local bindings),
// while the resulting value is intended to be stored in env.Expand().
func compileTransformerToMachineClosure(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	transformerExpr syntax.SyntaxValue,
	libraryScope *syntax.Scope,
	evaluator MacroEvaluator,
) (values.Value, error) {
	transformerPair, ok := transformerExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "define-syntax: transformer must be a list")
	}

	car := transformerPair.SyntaxCar()
	if car == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: transformer has empty car")
	}

	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol")
	}

	symVal := sym.Unwrap()
	if symVal == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "define-syntax: transformer symbol is nil")
	}

	symbol, ok := symVal.(*values.Symbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol")
	}

	switch symbol.Key {
	case "syntax-rules":
		return CompileSyntaxRules(ctx, env, transformerPair, libraryScope)

	case "lambda":
		return compileAndEvalLambdaTransformer(ctx, env, transformerPair, evaluator)

	case "er-macro-transformer":
		return compileERMacroTransformer(ctx, env, transformerPair, evaluator)

	default:
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedTransformer, "define-syntax: unsupported transformer type %q (expected syntax-rules, lambda, or er-macro-transformer)", symbol.Key)
	}
}

// compileAndEvalLambdaTransformer compiles a lambda expression and evaluates it at
// compile time to produce a closure that can be used as a syntax transformer.
func compileAndEvalLambdaTransformer(ctx context.Context, env *environment.EnvironmentFrame, lambdaExpr syntax.SyntaxValue, evaluator MacroEvaluator) (*MachineClosure, error) {
	tpl := NewNativeTemplate(0, 0, false)

	expandEnv := env.Expand()

	expandedExpr, err := NewExpanderTimeContinuation(ctx, expandEnv, evaluator).ExpandExpression(lambdaExpr)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error expanding transformer")
	}

	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompiletimeContinuation(tpl, expandEnv, evaluator)
	err = compiler.CompileExpression(cctx, expandedExpr)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error compiling transformer")
	}

	result, err := evaluator.EvalTemplate(ctx, tpl, expandEnv)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "error evaluating transformer")
	}

	closure, ok := result.(*MachineClosure)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAProcedure, "define-syntax: transformer must evaluate to a procedure, got %T", result)
	}

	return closure, nil
}
