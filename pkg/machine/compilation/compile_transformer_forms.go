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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileSyntaxRulesExpr compiles (syntax-rules …) in EXPRESSION position. The
// transformer closure is built at compile time in p.env — for a define-syntax
// right-hand side that is env.NextPhase() (compileTransformerValue), for
// (define t (syntax-rules …)) the phase-0 frame — and loaded as a literal.
//
// p.env has no lexical locals when it is a phase view (AtPhase), so the
// producer's definition-site local arm (compile_syntax_rules.go, the
// env.GetLocalIndex probe) no longer fires from a define-syntax / let-syntax /
// letrec-syntax right-hand side; design §2.1 item 3 and F2 in the impl plan. It
// still fires for (syntax-rules …) in expression position inside a phase-0 local
// frame — (let ((x 'def)) (define t (syntax-rules () ((_) x)))) — because that
// p.env is the lambda/let child frame, until P3 deletes the producer. The three
// guard rows in pkg/wile/syntax_forms_p0_test.go pin what still holds.
//
// Deleted in P3 with the Go syntax-rules.
func (p *CompileTimeContinuation) CompileSyntaxRulesExpr(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	src := expr.SourceContext()
	form := syntax.NewSyntaxCons(syntax.NewSyntaxSymbol(TransformerSyntaxRules, src), expr, src)
	closure, err := CompileSyntaxRules(ctctx.Context(), p.env, form, p.libraryScope)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "syntax-rules"))
	}
	litIdx := p.template.MaybeAppendLiteral(closure)
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}

// CompileERMacroTransformerExpr compiles (er-macro-transformer <lambda>) in
// EXPRESSION position: the lambda is compiled and evaluated in p.env at compile
// time and the *ERMacroTransformer is loaded as a literal. The definition
// environment handed to the rename closure is p.env, the frame the right-hand
// side compiles in; the head switch used env.Expand() of the DEFINING frame,
// which is the same frame for every phase-0 definition site and differs only
// for an ER macro defined inside another transformer's body.
//
// Deleted in P3 with the Go ER path.
func (p *CompileTimeContinuation) CompileERMacroTransformerExpr(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	lambdaExpr, err := formSingleArg(expr, TransformerERMacro)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	tpl, err := expandAndCompileScoped(ctctx.Context(), p.env, lambdaExpr, nil, DefaultInlineThreshold, DefaultMaxExpandDepth, p.libraryScope)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "er-macro-transformer: failed to compile lambda"))
	}
	result, err := p.evaluator.EvalTemplate(ctctx.Context(), tpl, p.env)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "er-macro-transformer: error evaluating lambda"))
	}
	closure, ok := result.(*machine.MachineClosure)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"er-macro-transformer: expected a procedure, got %T", result))
	}
	if !closure.AcceptsArity(3) {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
			"er-macro-transformer: lambda must accept exactly 3 arguments (form rename compare)"))
	}
	litIdx := p.template.MaybeAppendLiteral(NewERMacroTransformer(closure, p.env))
	p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
	return nil
}
