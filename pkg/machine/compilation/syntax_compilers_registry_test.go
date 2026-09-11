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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxCompilersRegistry(t *testing.T) {
	// RegisterSyntaxCompilers binds SyntaxCompiler values into the taproot
	// (the phase-0 seal). After registration, the hygiene-aware read
	// LookupPhaseBinding[*SyntaxCompiler] should find them by symbol with nil
	// scopes — the compile frame reaches the taproot through its parent chain.
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		{name: "syntax", formName: "syntax"},
		{name: "syntax-case", formName: "syntax-case"},
		{name: "meta", formName: "meta"},
		{name: "include", formName: "include"},
		{name: "include-ci", formName: "include-ci"},
		{name: "define-syntax", formName: "define-syntax"},
		{name: "define-library", formName: "define-library"},
		{name: "library (R6RS alias)", formName: "library"},
		{name: "import", formName: "import"},
		{name: "export", formName: "export"},
		{name: "unquote", formName: "unquote"},
		{name: "unquote-splicing", formName: "unquote-splicing"},
		{name: "quasisyntax", formName: "quasisyntax"},
		{name: "unsyntax", formName: "unsyntax"},
		{name: "unsyntax-splicing", formName: "unsyntax-splicing"},
		{name: "with-syntax", formName: "with-syntax"},
		{name: "cond-expand", formName: "cond-expand"},
		{name: "define-for-syntax", formName: "define-for-syntax"},
		{name: "begin-for-syntax", formName: "begin-for-syntax"},
		{name: "eval-when", formName: "eval-when"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			sc := LookupPhaseBinding[*SyntaxCompiler](env, sym, nil)
			qt.Assert(t, sc, qt.IsNotNil, qt.Commentf("LookupPhaseBinding[*SyntaxCompiler](%q) returned nil", tc.formName))
			qt.Assert(t, sc.Name(), qt.Equals, tc.formName)
		})
	}
}

// Syntax compilers reach the phase they were REGISTERED at, plus whatever a bulk
// row supplies — not every phase. The ambient (ANY, sealed) tier they used to
// occupy is gone: RegisterSyntaxCompilers writes through the level-0 sealed-write
// view, and EnvironmentFrame.writeCoordinates now lands every write at
// (phase = view's phase, view's sealed), so the compilers sit at
// (phase 0, sealed) and phase 1 is a foreign coordinate to them.
//
// Measured on a bare namespace, symbol "syntax-case": reachable at phase 0 only;
// phases -1, 1, 2 and 3 all resolve nil. Installing the row the default dialect
// declares for phase 1 (PhasedImport{BaseSourceName, PhaseExpand}, engine.go) adds
// phase 1 and nothing else — phase 2 stays nil, because the dialect declares the
// base at phases 0 and 1 and no higher.
//
// The pre-relocation leak this test was written for is still checked: the
// compilers must not be at (2, mutable).
func TestSyntaxCompilersReachTheirOwnPhasePlusBulkRowPhases(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	ns := env.Namespace()
	sym := values.NewSymbol("syntax-case")

	// The compiler binding sits at the level-0 SEALED coordinate — an exact
	// phase now, not the ambient one.
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
	qt.Assert(t, sealedRoot.OwnGlobalIndex(sym, values.EmptyScopes()), qt.IsNotNil)

	// It is NOT at (2, mutable) — the leak the relocation closes.
	qt.Assert(t, ns.AtPhase(environment.Phase(2)).OwnGlobalIndex(sym, values.EmptyScopes()), qt.IsNil)

	// Exact phase 0, so a read at phase 0 answers and a read at any other phase
	// does not. Phase 1 going nil here IS the user-visible break: a procedural
	// transformer body reaches the base only through a declared import.
	qt.Assert(t, ns.Runtime().GetBinding(sym, values.AllScopes()), qt.IsNotNil)
	qt.Assert(t, ns.Expand().GetBinding(sym, values.AllScopes()), qt.IsNil)
	qt.Assert(t, ns.AtPhase(environment.Phase(2)).GetBinding(sym, values.AllScopes()), qt.IsNil)

	// A bulk row is what restores phase-1 reach: one resolution candidate at
	// phase 1 standing for every name the store supplies at phase 0. This is the
	// shape wile.NewEngine installs for each of the default dialect's declared
	// initial imports.
	store := ns.Store()
	src := environment.NewSealedStoreBulkSource(store, environment.PhaseRuntime, environment.BaseSourceName())
	store.InstallBulkRow(src, nil, environment.PhaseExpand, true)

	qt.Assert(t, ns.Expand().GetBinding(sym, values.AllScopes()), qt.IsNotNil,
		qt.Commentf("the phase-1 row supplies what the ambient tier used to"))
	qt.Assert(t, store.BulkResolutionCount() > 0, qt.IsTrue,
		qt.Commentf("the answer must come THROUGH the row, not from a per-symbol slot"))

	// The row is declared at phase 1 alone, so it widens nothing above it.
	qt.Assert(t, ns.AtPhase(environment.Phase(2)).GetBinding(sym, values.AllScopes()), qt.IsNil)
}

func TestSyntaxCompilersRegistryLookupMiss(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("not-a-syntax-compiler")
	sc := LookupPhaseBinding[*SyntaxCompiler](env, sym, nil)
	qt.Assert(t, sc, qt.IsNil)
}

func TestSyntaxCompilersRegistryCoreFormsNotRegistered(t *testing.T) {
	// Core forms (if, define, lambda, etc.) are handled by Tier 1 validated
	// compilers registered in the forms package, NOT as SyntaxCompiler
	// bindings in the compile environment. Verify they do NOT appear here.
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		{name: "if not in syntax compilers", formName: "if"},
		{name: "define not in syntax compilers", formName: "define"},
		{name: "lambda not in syntax compilers", formName: "lambda"},
		{name: "set! not in syntax compilers", formName: "set!"},
		{name: "begin not in syntax compilers", formName: "begin"},
		{name: "quote not in syntax compilers", formName: "quote"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			sc := LookupPhaseBinding[*SyntaxCompiler](env, sym, nil)
			qt.Assert(t, sc, qt.IsNil, qt.Commentf("%q should not be a SyntaxCompiler", tc.formName))
		})
	}
}
