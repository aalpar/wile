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

// RegisterSyntaxCompilers binds all syntax compilers through the level-0
// SEALED-WRITE view, which every owner of a sealed axis has: the main
// namespace's, or a NewChildRuntime library env's own. Unlike the primitive
// expanders, these WANT the level-0 seal, because that is the one write whose
// coordinate is the ambient (AnyPhase, sealed) one — every other write, mutable
// at any level or sealed above 0, lands at an exact level
// (EnvironmentFrame.writeCoordinates). A binding placed here is therefore
// reachable from a frame at any level as the ranked probe's T3 tier, instead of
// being pinned to the PhaseCompile view.
//
// This is a write COORDINATE, not a topology: phase views have no lexical parent,
// and hermeticity is key disjointness in the one store. Comments here once said
// "every phase frame parents to that taproot"; createPhaseEnv stopped reparenting
// when the store was flattened.
//
// These bindings serve two purposes:
//
//  1. Library export/import: findLibraryBinding in library_bindings.go searches
//     the levels the library's own registry has instantiated (PresentPhases) to
//     locate syntax compilers when exporting or importing forms like syntax-case,
//     define-syntax, etc. A NewChildRuntime library env is an island — it owns its
//     own store, so the engine root's ambient tier is not reachable from it at any
//     level — and special forms stay ambient-only, unchanged by this relocation.
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
		return env.SealedWriteViewAt(environment.PhaseRuntime)
	}
	return RegisterPhaseBindings(env, taproot, syntaxCompilerEntries,
		func(name string, fn SyntaxCompilerFunc) values.Value {
			return NewSyntaxCompiler(name, fn)
		})
}

// LookupSyntaxCompiler looks up a syntax compiler by symbol, entering through the
// PhaseCompile view. Returns the SyntaxCompiler if found, or nil if the symbol
// does not name a syntax compiler. The compilers live in the ambient tier (see
// RegisterSyntaxCompilers), which that view's ranked probe reaches as T3, so a
// PhaseCompile shadow at T1 or T2 still takes precedence.
//
// Naming PhaseCompile by constant is correct here, unlike a climb: it is a fixed
// registry coordinate rather than a rung of the macro tower, so it does not vary
// with the level of the frame asking. See environment.Phase.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupSyntaxCompiler(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *SyntaxCompiler {
	return LookupPhaseBinding[*SyntaxCompiler](env.Compile(), sym, scopes)
}
