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
	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/werr"
)

func init() {
	// Register compilers for core forms (Tier 1 - typed ValidatedExpr)
	registerTypedCompiler("if", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedIf) error {
		return ctc.CompileValidatedIf(ctctx, v)
	})
	registerTypedCompiler("define", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
		return ctc.CompileValidatedDefine(ctctx, v)
	})
	registerTypedCompiler("lambda", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLambda) error {
		return ctc.CompileValidatedLambda(ctctx, v)
	})
	registerTypedCompiler("case-lambda", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedCaseLambda) error {
		return ctc.CompileValidatedCaseLambda(ctctx, v)
	})
	registerTypedCompiler("set!", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedSetBang) error {
		return ctc.CompileValidatedSetBang(ctctx, v)
	})
	registerTypedCompiler("quote", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedQuote) error {
		return ctc.CompileValidatedQuote(ctctx, v)
	})
	registerTypedCompiler("begin", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedBegin) error {
		return ctc.CompileValidatedBegin(ctctx, v)
	})
	registerTypedCompiler("quasiquote", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedQuasiquote) error {
		return ctc.CompileValidatedQuasiquote(ctctx, v)
	})
	registerTypedCompiler("dynamic-wind", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedDynamicWind) error {
		return ctc.CompileValidatedDynamicWind(ctctx, v)
	})
	registerTypedCompiler("apply", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedApply) error {
		return ctc.CompileValidatedApply(ctctx, v)
	})
	registerTypedCompiler("with-continuation-mark", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedWithContinuationMark) error {
		return ctc.CompileValidatedWithContinuationMark(ctctx, v)
	})
	registerTypedCompiler("let", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLet) error {
		return ctc.CompileValidatedLet(ctctx, v)
	})
	registerTypedCompiler("let*", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLet) error {
		return ctc.CompileValidatedLet(ctctx, v)
	})
	registerTypedCompiler("letrec", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLet) error {
		return ctc.CompileValidatedLet(ctctx, v)
	})
	registerTypedCompiler("letrec*", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLet) error {
		return ctc.CompileValidatedLet(ctctx, v)
	})

	// Register compilers for extension forms (Tier 2 - syntax passthrough)
	// These extract syntax from ValidatedLiteral and compile it
	registerSyntaxCompiler("syntax", (*CompileTimeContinuation).CompileSyntax)
	registerSyntaxCompiler("syntax-case", (*CompileTimeContinuation).CompileSyntaxCase)
	registerSyntaxCompiler("meta", (*CompileTimeContinuation).CompileMeta)
	registerSyntaxCompiler("include", (*CompileTimeContinuation).CompileInclude)
	registerSyntaxCompiler("include-ci", (*CompileTimeContinuation).CompileIncludeCi)
	registerSyntaxCompiler("define-syntax", (*CompileTimeContinuation).CompileDefineSyntax)
	// syntax-rules is handled by define-syntax, not registered separately
	registerSyntaxCompiler("define-library", (*CompileTimeContinuation).CompileDefineLibrary)
	registerSyntaxCompiler("library", (*CompileTimeContinuation).CompileDefineLibrary) // R6RS alias
	registerSyntaxCompiler("import", (*CompileTimeContinuation).CompileImport)
	registerSyntaxCompiler("export", (*CompileTimeContinuation).CompileExport)
	registerSyntaxCompiler("unquote", (*CompileTimeContinuation).CompileUnquote)
	registerSyntaxCompiler("unquote-splicing", (*CompileTimeContinuation).CompileUnquoteSplicing)
	registerSyntaxCompiler("quasisyntax", (*CompileTimeContinuation).CompileQuasisyntax)
	registerSyntaxCompiler("unsyntax", (*CompileTimeContinuation).CompileUnsyntax)
	registerSyntaxCompiler("unsyntax-splicing", (*CompileTimeContinuation).CompileUnsyntaxSplicing)
	registerSyntaxCompiler("with-syntax", (*CompileTimeContinuation).CompileWithSyntax)
	registerSyntaxCompiler("cond-expand", (*CompileTimeContinuation).CompileCondExpand)
	registerSyntaxCompiler("define-for-syntax", (*CompileTimeContinuation).CompileDefineForSyntax)
	registerSyntaxCompiler("begin-for-syntax", (*CompileTimeContinuation).CompileBeginForSyntax)
	registerSyntaxCompiler("eval-when", (*CompileTimeContinuation).CompileEvalWhen)
}

// registerTypedCompiler registers a compiler that handles a typed ValidatedExpr.
func registerTypedCompiler[T validate.ValidatedExpr](name string, fn func(*CompileTimeContinuation, CompileTimeCallContext, T) error) {
	forms.RegisterCompiler(name, func(ctc any, ctctx any, expr forms.ValidatedExpr) error {
		return fn(
			ctc.(*CompileTimeContinuation),
			ctctx.(CompileTimeCallContext),
			expr.(T),
		)
	})
}

// registerSyntaxCompiler registers a compiler that handles syntax directly.
// For forms that pass through validation as ValidatedLiteral.
func registerSyntaxCompiler(name string, fn SyntaxCompilerFunc) {
	forms.RegisterCompiler(name, func(ctc any, ctctx any, expr forms.ValidatedExpr) error {
		lit, ok := expr.(*validate.ValidatedLiteral)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"registerSyntaxCompiler(%s): expected ValidatedLiteral, got %T", name, expr)
		}
		pair, ok := lit.Value.(*syntax.SyntaxPair)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"registerSyntaxCompiler(%s): expected SyntaxPair, got %T", name, lit.Value)
		}
		args, ok := pair.Cdr().(syntax.SyntaxValue)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"registerSyntaxCompiler(%s): CDR is not SyntaxValue: %T", name, pair.Cdr())
		}
		return fn(
			ctc.(*CompileTimeContinuation),
			ctctx.(CompileTimeCallContext),
			args,
		)
	})
}
