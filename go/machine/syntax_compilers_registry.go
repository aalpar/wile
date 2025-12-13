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
	"wile/environment"
	"wile/syntax"
	"wile/values"
)

// RegisterSyntaxCompilers binds all syntax compilers in the compile-time
// environment (env.Compile()). These are looked up by CompileSyntaxPrimitive()
// when the compiler encounters a special form.
//
// The compiler uses a two-tier dispatch system:
//
// Tier 1 (Validated Forms): Core forms like if, define, lambda, set!, quote,
// quasiquote, begin go through the validation layer which produces type-safe
// ValidatedExpr types, then compile via compileValidated* methods. These are
// NOT registered here.
//
// Tier 2 (Registry Forms): Extension forms like syntax-case, import, define-syntax
// pass through validation as ValidatedLiteral, then are dispatched via this
// registry. This is the single source of truth for these forms.
//
// The syntax compilers are bound with BindingTypePrimitive to distinguish them
// from syntax transformers (BindingTypeSyntax) and regular variables.
func RegisterSyntaxCompilers(env *environment.EnvironmentFrame) error {
	compileEnv := env.Compile()

	// All syntax compilers for Tier 2 forms.
	// Each entry maps a keyword to its compile function.
	compilers := []struct {
		name string
		fn   SyntaxCompilerFunc
	}{
		{"syntax", (*CompileTimeContinuation).CompileSyntax},
		{"syntax-case", (*CompileTimeContinuation).CompileSyntaxCase},
		{"meta", (*CompileTimeContinuation).CompileMeta},
		{"include", (*CompileTimeContinuation).CompileInclude},
		{"include-ci", (*CompileTimeContinuation).CompileIncludeCi},
		{"define-syntax", (*CompileTimeContinuation).CompileDefineSyntax},
		{"define-library", (*CompileTimeContinuation).CompileDefineLibrary},
		{"library", (*CompileTimeContinuation).CompileDefineLibrary}, // R6RS alias
		{"import", (*CompileTimeContinuation).CompileImport},
		{"export", (*CompileTimeContinuation).CompileExport},
		{"unquote", (*CompileTimeContinuation).CompileUnquote},
		{"unquote-splicing", (*CompileTimeContinuation).CompileUnquoteSplicing},
		{"quasisyntax", (*CompileTimeContinuation).CompileQuasisyntax},
		{"unsyntax", (*CompileTimeContinuation).CompileUnsyntax},
		{"unsyntax-splicing", (*CompileTimeContinuation).CompileUnsyntaxSplicing},
		{"with-syntax", (*CompileTimeContinuation).CompileWithSyntax},
		{"cond-expand", (*CompileTimeContinuation).CompileCondExpand},
		{"define-for-syntax", (*CompileTimeContinuation).CompileDefineForSyntax},
		{"begin-for-syntax", (*CompileTimeContinuation).CompileBeginForSyntax},
		{"eval-when", (*CompileTimeContinuation).CompileEvalWhen},
	}

	for _, comp := range compilers {
		sym := env.InternSymbol(values.NewSymbol(comp.name))
		compiler := NewSyntaxCompiler(comp.name, comp.fn)

		// Always set the value - the binding may already exist from runtime primitives
		// registration, but we still need to set the SyntaxCompiler value
		idx, _ := compileEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive)
		compileEnv.SetOwnGlobalValue(idx, compiler) //nolint:errcheck
	}

	return nil
}

// LookupSyntaxCompiler looks up a syntax compiler by symbol in the compile
// environment. Returns the SyntaxCompiler if found, or nil if the symbol does
// not name a syntax compiler.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupSyntaxCompiler(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *SyntaxCompiler {
	compileEnv := env.Compile()

	// Look up with scopes for hygiene
	bnd := compileEnv.GetBindingWithScopes(sym, scopes)
	if bnd == nil {
		return nil
	}

	// Check if it's a syntax compiler binding
	if bnd.BindingType() != environment.BindingTypePrimitive {
		return nil
	}

	// Get the value and check if it's a SyntaxCompiler
	val := bnd.Value()
	if pc, ok := val.(*SyntaxCompiler); ok {
		return pc
	}

	return nil
}
