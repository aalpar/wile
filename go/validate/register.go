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

package validate

import (
	"context"
	"wile/forms"
	"wile/syntax"
)

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
type validatorFunc func(ctx context.Context, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr

// registerValidator wraps a validation function and registers it with the forms package.
func registerValidator(name string, fn validatorFunc) {
	forms.RegisterValidator(name, func(ctx context.Context, pair any, result any) any {
		return fn(ctx, pair.(*syntax.SyntaxPair), result.(*ValidationResult))
	})
}

// registerPassthrough registers a form that passes through as ValidatedLiteral.
func registerPassthrough(name string) {
	forms.RegisterValidator(name, func(ctx context.Context, pair any, result any) any {
		p := pair.(*syntax.SyntaxPair)
		return &ValidatedLiteral{source: p.SourceContext(), formName: "@literal", Value: p}
	})
}
