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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/syntax"
)

// ADDING A NEW SPECIAL FORM requires updates in these locations:
//
//  1. internal/validate/register.go    — register validator (this file)
//  2. internal/validate/validate_<name>.go — implement validator function
//  3. machine/register.go              — register compiler
//  4. machine/compile_validated.go     — implement compilation (Tier 1)
//     OR machine/compile_<name>.go     — implement compilation (Tier 2)
//  5. machine/primitive_expanders_registry.go — register expander (if needed)
//  6. machine/expander_*.go            — implement expander (if needed)
//  7. registry/core/specialforms.go    — register compile-time binding
func init() {
	// Register validators for core forms
	registerValidator("if", validateIf)
	registerValidator("define", validateDefine)
	registerValidator("lambda", validateLambda)
	registerValidator("case-lambda", validateCaseLambda)
	registerValidator("set!", validateSetBang)
	registerValidator("quote", validateQuote)
	registerValidator("begin", validateBegin)
	registerValidator("quasiquote", validateQuasiquote)
	registerValidator("dynamic-wind", validateDynamicWind)
	registerValidator("with-continuation-mark", validateWithContinuationMark)
	registerValidator("apply", validateApply)
	registerValidator("let", validateLetCommon(LetKindLet, "let"))
	registerValidator("let*", validateLetCommon(LetKindLetStar, "let*"))
	registerValidator("letrec", validateLetCommon(LetKindLetrec, "letrec"))
	registerValidator("letrec*", validateLetCommon(LetKindLetrecStar, "letrec*"))

	// Macro and library forms with structural validation
	registerValidator("define-syntax", validateDefineSyntax)
	registerValidator("syntax-rules", validateSyntaxRules)
	registerValidator("import", validateImport)
	registerValidator("export", validateExport)
	registerValidator("define-library", validateDefineLibrary)
	registerValidator("library", validateDefineLibrary) // R6RS alias
	registerValidator("include", validateInclude)
	registerValidator("include-ci", validateInclude)
	registerValidator("cond-expand", validateCondExpand)

	// Forms that pass through to compiler (no validation needed)
	registerPassthrough("let-syntax")
	registerPassthrough("letrec-syntax")
	registerPassthrough("meta")
	registerPassthrough("syntax")
	registerPassthrough("syntax-case")
	registerPassthrough("quasisyntax")
	registerPassthrough("unsyntax")
	registerPassthrough("unsyntax-splicing")
	registerPassthrough("with-syntax")
	registerPassthrough("define-for-syntax")
	registerPassthrough("begin-for-syntax")
	registerPassthrough("eval-when")
	registerPassthrough("unquote")
	registerPassthrough("unquote-splicing")
}

// validatorFunc is the internal type for validation functions.
// The env parameter provides the environment for checking local variable shadowing.
type validatorFunc func(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr

// registerValidator wraps a validation function and registers it with the forms package.
func registerValidator(name string, fn validatorFunc) {
	forms.RegisterValidator(name, func(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result any) forms.ValidatedExpr {
		return fn(ctx, env, pair, result.(*ValidationResult))
	})
}

// registerPassthrough registers a form that passes through as ValidatedLiteral.
//
// The form's body is code this package never validates — it is compiled later, in
// its own unit — so every name it mentions is recorded as a possible set! target
// before the form is parked. Discarding env and result here (which this function
// used to do) is what let a set! or a capture inside a passthrough body go unseen
// by escape, mutable and StableInUnit marking. See opaque_subtree.go.
func registerPassthrough(name string) {
	forms.RegisterValidator(name, func(_ context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result any) forms.ValidatedExpr {
		res, ok := result.(*ValidationResult)
		if ok {
			markOpaqueSubtree(env, pair, res)
		}
		return newLiteralExpr(pair.SourceContext(), pair)
	})
}
