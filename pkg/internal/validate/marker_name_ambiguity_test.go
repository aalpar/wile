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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestW2MarkerNameDoesNotRaiseOnAmbiguity is the gate on I024, at the exact
// consumer the item is about.
//
// The head of a quasiquote form is a DATUM until an unquote brings it back to
// code, and this walk asks only what it denotes. Two bindings of "foo" at the
// incomparable equal-cardinality scope sets {A,B} and {A,C} tie for a reference
// carrying {A,B,C}; resolution refuses the tie, and refusing it by RAISING
// turns a hygiene curiosity in a position nothing evaluates into a compile-time
// crash. The refusal is an answer here: "denotes no form", so the spelling
// stands, exactly as for an unbound head.
//
// Red on master, where markerName resolves through the raising GetBinding and
// this panics with
// `GetBinding: identifier "foo" resolves ambiguously among incomparable
// hygienic scope sets: ambiguous binding`.
func TestW2MarkerNameDoesNotRaiseOnAmbiguity(t *testing.T) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	env := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	sym := values.NewSymbol("foo")
	env.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable,
		syntax.Scopes{}.Add(scopeA).Add(scopeB), nil)
	env.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable,
		syntax.Scopes{}.Add(scopeA).Add(scopeC), nil)

	stx := syntax.NewSyntaxSymbol("foo", &syntax.SourceContext{
		Scopes: syntax.Scopes{}.Add(scopeA).Add(scopeB).Add(scopeC),
	})

	got := ""
	raised := func() (r any) {
		defer func() {
			r = recover()
		}()
		got = markerName(env, stx)
		return nil
	}()

	qt.Assert(t, raised, qt.IsNil,
		qt.Commentf("markerName raised on an ambiguous DATUM head: %v", raised))
	qt.Assert(t, got, qt.Equals, "foo",
		qt.Commentf("a refused tie denotes no form, so the spelling stands"))
}

// TestW2MarkerNameStillDenotesUnderNoAmbiguity is the non-vacuity guard, and it
// has to be a RENAMED marker to be one: a spelling that matches its own
// denotation is the answer a markerName that resolved nothing at all would also
// give, so the guard only bites when the two differ.
func TestW2MarkerNameStillDenotesUnderNoAmbiguity(t *testing.T) {
	scope := syntax.NewScope()
	scopes := syntax.Scopes{}.Add(scope)

	env := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	sym := values.NewSymbol("otherunquote")
	env.MaybeCreateLocalBinding(sym, environment.BindingTypeSyntax, scopes, nil)
	env.GetBinding(sym, syntax.ScopesOf(scopes)).SetValue(environment.NewFormKeyword(unquoteKey))

	stx := syntax.NewSyntaxSymbol("otherunquote", &syntax.SourceContext{Scopes: scopes})

	qt.Assert(t, markerName(env, stx), qt.Equals, unquoteKey,
		qt.Commentf("a renamed marker is still a marker; spelling-only reading would answer otherunquote"))
}
