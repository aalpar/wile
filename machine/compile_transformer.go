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
)

// compileTransformerToMachineClosure compiles a define-syntax transformer expression
// into a MachineClosure for storage in the expand environment.
//
// Supports:
//   - (syntax-rules ...) - compiled directly via CompileSyntaxRules
//   - (lambda (stx) ...) - compiled and evaluated to produce a closure
//
// The env parameter is used for compilation (so transformers can see local bindings),
// while the resulting closure is intended to be stored in env.Expand().
func compileTransformerToMachineClosure(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	transformerExpr syntax.SyntaxValue,
) (*MachineClosure, error) {
	transformerPair, ok := transformerExpr.(*syntax.SyntaxPair)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotASyntaxPair, "define-syntax: transformer must be a list")
	}

	car := transformerPair.SyntaxCar()
	if car == nil {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: transformer has empty car")
	}

	sym, ok := car.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol")
	}

	symVal := sym.Unwrap()
	if symVal == nil {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedNil, "define-syntax: transformer symbol is nil")
	}

	symbol, ok := symVal.(*values.Symbol)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: transformer must start with a symbol")
	}

	switch symbol.Key {
	case "syntax-rules":
		return CompileSyntaxRules(ctx, env, transformerPair)

	case "lambda":
		return compileAndEvalLambdaTransformer(ctx, env, transformerPair)

	default:
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedTransformer, "define-syntax: unsupported transformer type %q (expected syntax-rules or lambda)", symbol.Key)
	}
}

// compileAndEvalLambdaTransformer compiles a lambda expression and evaluates it at
// compile time to produce a closure that can be used as a syntax transformer.
func compileAndEvalLambdaTransformer(ctx context.Context, env *environment.EnvironmentFrame, lambdaExpr syntax.SyntaxValue) (*MachineClosure, error) {
	tpl := NewNativeTemplate(0, 0, false)

	expandEnv := env.Expand()

	expandedExpr, err := NewExpanderTimeContinuation(ctx, expandEnv).ExpandExpression(lambdaExpr)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error expanding transformer")
	}

	cctx := NewCompileTimeCallContext(ctx, false, true)
	compiler := NewCompiletimeContinuation(tpl, expandEnv)
	err = compiler.CompileExpression(cctx, expandedExpr)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error compiling transformer")
	}

	cont := NewMachineContinuation(nil, tpl, expandEnv)
	mc := NewMachineContext(ctx, cont)
	err = mc.Run()
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error evaluating transformer")
	}

	result := mc.GetValue()
	closure, ok := result.(*MachineClosure)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAProcedure, "define-syntax: transformer must evaluate to a procedure, got %T", result)
	}

	return closure, nil
}
