// Copyright 2025 Aaron Alpar
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
	"wile/forms"
	"wile/syntax"
	"wile/validate"
)

func init() {
	// Register compilers for core forms (Tier 1 - typed ValidatedExpr)
	registerTypedCompiler("if", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedIf) error {
		return ctc.compileValidatedIf(ctctx, formName, v)
	})
	registerTypedCompiler("define", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
		return ctc.compileValidatedDefine(ctctx, formName, v)
	})
	registerTypedCompiler("lambda", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedLambda) error {
		return ctc.compileValidatedLambda(ctctx, formName, v)
	})
	registerTypedCompiler("case-lambda", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedCaseLambda) error {
		return ctc.compileValidatedCaseLambda(ctctx, formName, v)
	})
	registerTypedCompiler("set!", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedSetBang) error {
		return ctc.compileValidatedSetBang(ctctx, formName, v)
	})
	registerTypedCompiler("quote", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedQuote) error {
		return ctc.compileValidatedQuote(ctctx, formName, v)
	})
	registerTypedCompiler("begin", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedBegin) error {
		return ctc.compileValidatedBegin(ctctx, formName, v)
	})
	registerTypedCompiler("quasiquote", func(ctc *CompileTimeContinuation, formName string, ctctx CompileTimeCallContext, v *validate.ValidatedQuasiquote) error {
		return ctc.compileValidatedQuasiquote(ctctx, formName, v)
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
func registerTypedCompiler[T validate.ValidatedExpr](name string, fn func(*CompileTimeContinuation, string, CompileTimeCallContext, T) error) {
	forms.RegisterCompiler(name, func(ctc any, ctctx any, expr any) error {
		return fn(
			ctc.(*CompileTimeContinuation),
			name,
			ctctx.(CompileTimeCallContext),
			expr.(T),
		)
	})
}

// registerSyntaxCompiler registers a compiler that handles syntax directly.
// For forms that pass through validation as ValidatedLiteral.
func registerSyntaxCompiler(name string, fn SyntaxCompilerFunc) {
	forms.RegisterCompiler(name, func(ctc any, ctctx any, expr any) error {
		// Extract syntax from ValidatedLiteral
		lit := expr.(*validate.ValidatedLiteral)
		pair := lit.Value.(*syntax.SyntaxPair)
		// Skip the keyword, get the arguments
		args := pair.Cdr().(syntax.SyntaxValue)
		return fn(
			ctc.(*CompileTimeContinuation),
			ctctx.(CompileTimeCallContext),
			args,
		)
	})
}
