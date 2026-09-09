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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestPrimitiveExpandersRegistry(t *testing.T) {
	// RegisterPrimitiveExpanders binds PrimitiveExpander values into
	// env.Expand(). After registration, LookupPrimitiveExpander should
	// find them by symbol with nil scopes.
	env := environment.NewNamespace().Runtime()
	err := RegisterPrimitiveExpanders(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		// Unchanged forms
		{name: "quote", formName: "quote"},
		{name: "define-syntax", formName: "define-syntax"},
		{name: "quasiquote", formName: "quasiquote"},
		{name: "unquote", formName: "unquote"},
		{name: "unquote-splicing", formName: "unquote-splicing"},
		{name: "include", formName: "include"},
		{name: "include-ci", formName: "include-ci"},
		{name: "define-library", formName: "define-library"},
		{name: "cond-expand", formName: "cond-expand"},
		{name: "syntax", formName: "syntax"},
		{name: "syntax-case", formName: "syntax-case"},
		{name: "er-macro-transformer", formName: "er-macro-transformer"},
		{name: "quasisyntax", formName: "quasisyntax"},
		{name: "unsyntax", formName: "unsyntax"},
		{name: "unsyntax-splicing", formName: "unsyntax-splicing"},
		{name: "with-syntax", formName: "with-syntax"},
		{name: "let-syntax", formName: "let-syntax"},
		{name: "letrec-syntax", formName: "letrec-syntax"},
		{name: "with-binding-scope", formName: "with-binding-scope"},
		{name: "syntax-error", formName: "syntax-error"},

		// Forms that expand subexpressions
		{name: "if", formName: "if"},
		{name: "begin", formName: "begin"},
		{name: "set!", formName: "set!"},
		{name: "define", formName: "define"},
		{name: "lambda", formName: "lambda"},
		{name: "case-lambda", formName: "case-lambda"},
		{name: "with-continuation-mark", formName: "with-continuation-mark"},
		{name: "import", formName: "import"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			pe := LookupPrimitiveExpander(env, sym, nil)
			qt.Assert(t, pe, qt.IsNotNil, qt.Commentf("LookupPrimitiveExpander(%q) returned nil", tc.formName))
			qt.Assert(t, pe.Name(), qt.Equals, tc.formName)
		})
	}
}

func TestPrimitiveExpandersRegistryLookupMiss(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterPrimitiveExpanders(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("not-a-primitive-expander")
	pe := LookupPrimitiveExpander(env, sym, nil)
	qt.Assert(t, pe, qt.IsNil)
}

// TestPrimitiveExpandersLandAtExactPhaseOne pins WHERE registerPrimitiveExpandersWithout
// writes: (ExactPhase(1), sealed), the ranked probe's T2, never the ambient T3 set.
//
// This is an ANSWER pin, not a change pin: it passes today and is here because the
// function's own comment asserted the opposite for as long as it did without anything
// contradicting it. It is also a ratchet for the Flatt Stage A work, which deletes the
// ambient tier — under A the expected answer here does not move (these slots are already
// exact-phase), and a diff that makes it move means A routed expanders somewhere new.
//
// Deliberately uses a BARE namespace: a full engine adds keyword slots for some of these
// names from a different source. pkg/wile/binding_tier_census_test.go covers that.
func TestPrimitiveExpandersLandAtExactPhaseOne(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterPrimitiveExpanders(env)
	qt.Assert(t, err, qt.IsNil)

	g := env.GlobalEnvironment()
	// let-syntax and when-adjacent forms have no specialforms.go keyword row, so they
	// are the discriminating cases; syntax-rules and quote do, and are included to show
	// the bare path alone still mints no ambient slot for them.
	for _, name := range []string{"syntax-rules", "quote", "let-syntax", "import", "lambda"} {
		t.Run(name, func(t *testing.T) {
			sym := values.NewSymbol(name)
			amb, ambTie := g.AmbientBinding(sym, syntax.EmptyScopes())
			qt.Assert(t, ambTie, qt.IsFalse)
			qt.Assert(t, amb, qt.IsNil,
				qt.Commentf("%q must not occupy the ambient tier", name))

			ex1, ex1Tie := g.ExactBindingAt(sym, syntax.EmptyScopes(), environment.PhaseExpand)
			qt.Assert(t, ex1Tie, qt.IsFalse)
			qt.Assert(t, ex1, qt.IsNotNil,
				qt.Commentf("%q must resolve at (ExactPhase(1), sealed)", name))

			ex0, _ := g.ExactBindingAt(sym, syntax.EmptyScopes(), environment.PhaseRuntime)
			qt.Assert(t, ex0, qt.IsNil,
				qt.Commentf("%q must not resolve at exact phase 0", name))
		})
	}
}
