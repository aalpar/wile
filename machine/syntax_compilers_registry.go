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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// RegisterSyntaxCompilers binds all syntax compilers in the compile-time
// environment (env.Compile()). These bindings serve two purposes:
//
//  1. Library export/import: findLibraryBinding in library_bindings.go searches
//     the compile environment to locate syntax compilers when exporting or
//     importing forms like syntax-case, define-syntax, etc.
//  2. Scope-aware lookup via LookupSyntaxCompiler for hygiene resolution.
//
// Compilation dispatch itself goes through the forms registry (register.go),
// not through these bindings. The forms registry and the compile environment
// are populated from the same source list to stay in sync.
//
// The syntax compilers are bound with BindingTypePrimitive to distinguish them
// from syntax transformers (BindingTypeSyntax) and regular variables.
func RegisterSyntaxCompilers(env *environment.EnvironmentFrame) error {
	compilers := []PhaseEntry[SyntaxCompilerFunc]{
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

	return RegisterPhaseBindings(env, env.Compile, compilers,
		func(name string, fn SyntaxCompilerFunc) values.Value {
			return NewSyntaxCompiler(name, fn)
		})
}

// LookupSyntaxCompiler looks up a syntax compiler by symbol in the compile
// environment. Returns the SyntaxCompiler if found, or nil if the symbol does
// not name a syntax compiler.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupSyntaxCompiler(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *SyntaxCompiler {
	return LookupPhaseBinding[*SyntaxCompiler](env.Compile(), sym, scopes)
}
