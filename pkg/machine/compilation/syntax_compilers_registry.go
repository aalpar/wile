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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// syntaxCompilerEntries is the single source of truth for all Tier 2 syntax
// compiler registrations. Both init() (which attaches each entry onto its FormSpec
// via forms.RegisterCompiler) and RegisterSyntaxCompilers (compile-time environment
// for library export/hygiene) derive from this slice.
//
// ADDING A NEW SYNTAX COMPILER: add one entry here. Both registration paths
// pick it up automatically.
var syntaxCompilerEntries = []PhaseEntry[SyntaxCompilerFunc]{
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

// RegisterSyntaxCompilers binds all syntax compilers in the runtime frame's
// sealed-base target — the frozen taproot for a layered main namespace, or the
// flat frame itself for a NewChildRuntime library frame (SealedBaseTarget()
// picks the right one, mirroring how applyBaseEnvironment routes runtime
// primitives). Because every phase frame now parents to that taproot (see the
// reparent in environment/phase_registry.go createPhaseEnv), a binding placed
// here is ambient: reachable uniformly from the runtime, expand, and compile
// phases via the parent chain, instead of being pinned to the phase-2 (compile)
// frame. These bindings serve two purposes:
//
//  1. Library export/import: findLibraryBinding in library_bindings.go searches
//     a library's own runtime/expand/compile frames to locate syntax compilers
//     when exporting or importing forms like syntax-case, define-syntax, etc.
//     A NewChildRuntime library frame is a flat island (parent nil), so it does
//     not reach the engine root's taproot — special forms stay ambient-only,
//     unchanged by this relocation.
//  2. Scope-aware lookup via LookupSyntaxCompiler for hygiene resolution.
//
// Compilation dispatch itself goes through the forms registry (register.go),
// not through these bindings. Both paths are populated from
// syntaxCompilerEntries to stay in sync.
//
// The syntax compilers are bound with BindingTypePrimitive to distinguish them
// from syntax transformers (BindingTypeSyntax) and regular variables.
func RegisterSyntaxCompilers(env *environment.EnvironmentFrame) error {
	taproot := func() *environment.EnvironmentFrame {
		return env.SealedBaseTarget()
	}
	return RegisterPhaseBindings(env, taproot, syntaxCompilerEntries,
		func(name string, fn SyntaxCompilerFunc) values.Value {
			return NewSyntaxCompiler(name, fn)
		})
}

// LookupSyntaxCompiler looks up a syntax compiler by symbol, entering through
// the compile phase frame. Returns the SyntaxCompiler if found, or nil if the
// symbol does not name a syntax compiler. The compilers live in the ambient
// taproot (see RegisterSyntaxCompilers); the compile frame reaches them through
// its parent chain, so a phase-2 shadow still takes precedence.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupSyntaxCompiler(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *SyntaxCompiler {
	return LookupPhaseBinding[*SyntaxCompiler](env.Compile(), sym, scopes)
}
