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
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/werr"
)

func init() {
	// Tier 2: syntax passthrough compilers — syntaxCompiler unwraps
	// ValidatedLiteral → SyntaxPair → CDR before calling the method.
	//
	// Tier 1 forms (if, define, lambda, etc.) are dispatched by type switch
	// in compileValidated (compile_validated.go) — no registry entry needed.
	registerCompiler("syntax", syntaxCompiler((*CompileTimeContinuation).CompileSyntax))
	registerCompiler("syntax-case", syntaxCompiler((*CompileTimeContinuation).CompileSyntaxCase))
	registerCompiler("meta", syntaxCompiler((*CompileTimeContinuation).CompileMeta))
	registerCompiler("include", syntaxCompiler((*CompileTimeContinuation).CompileInclude))
	registerCompiler("include-ci", syntaxCompiler((*CompileTimeContinuation).CompileIncludeCi))
	registerCompiler("define-syntax", syntaxCompiler((*CompileTimeContinuation).CompileDefineSyntax))
	// syntax-rules is handled by define-syntax, not registered separately
	registerCompiler("define-library", syntaxCompiler((*CompileTimeContinuation).CompileDefineLibrary))
	registerCompiler("library", syntaxCompiler((*CompileTimeContinuation).CompileDefineLibrary)) // R6RS alias
	registerCompiler("import", syntaxCompiler((*CompileTimeContinuation).CompileImport))
	registerCompiler("export", syntaxCompiler((*CompileTimeContinuation).CompileExport))
	registerCompiler("unquote", syntaxCompiler((*CompileTimeContinuation).CompileUnquote))
	registerCompiler("unquote-splicing", syntaxCompiler((*CompileTimeContinuation).CompileUnquoteSplicing))
	registerCompiler("quasisyntax", syntaxCompiler((*CompileTimeContinuation).CompileQuasisyntax))
	registerCompiler("unsyntax", syntaxCompiler((*CompileTimeContinuation).CompileUnsyntax))
	registerCompiler("unsyntax-splicing", syntaxCompiler((*CompileTimeContinuation).CompileUnsyntaxSplicing))
	registerCompiler("with-syntax", syntaxCompiler((*CompileTimeContinuation).CompileWithSyntax))
	registerCompiler("cond-expand", syntaxCompiler((*CompileTimeContinuation).CompileCondExpand))
	registerCompiler("define-for-syntax", syntaxCompiler((*CompileTimeContinuation).CompileDefineForSyntax))
	registerCompiler("begin-for-syntax", syntaxCompiler((*CompileTimeContinuation).CompileBeginForSyntax))
	registerCompiler("eval-when", syntaxCompiler((*CompileTimeContinuation).CompileEvalWhen))
}

// syntaxCompiler adapts a SyntaxCompilerFunc into a CompilerFunc by unwrapping
// ValidatedLiteral → SyntaxPair → CDR.
func syntaxCompiler(fn SyntaxCompilerFunc) CompilerFunc {
	return func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr *validate.ValidatedLiteral) error {
		pair, ok := expr.Value.(*syntax.SyntaxPair)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: expected SyntaxPair, got %T", expr.Value)
		}
		args, ok := pair.Cdr().(syntax.SyntaxValue)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"syntaxCompiler: CDR is not SyntaxValue: %T", pair.Cdr())
		}
		return fn(ctc, ctctx, args)
	}
}
