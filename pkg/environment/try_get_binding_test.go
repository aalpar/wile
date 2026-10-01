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

package environment

import (
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// ambiguousLocalFrame returns a frame holding two bindings of name at the
// incomparable equal-cardinality scope sets {A,B} and {A,C}, together with the
// query {A,B,C} that ties them: no single binding is the maximal subset, which
// is Racket's "ambiguous binding".
func ambiguousLocalFrame(name string) (*EnvironmentFrame, *values.Symbol, syntax.ScopeSet) {
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	scopeC := syntax.NewScope()

	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), NewNamespaceFrame())
	sym := values.NewSymbol(name)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable,
		syntax.Scopes{}.Add(scopeA).Add(scopeB), nil)
	env.MaybeCreateLocalBinding(sym, BindingTypeVariable,
		syntax.Scopes{}.Add(scopeA).Add(scopeC), nil)

	return env, sym, syntax.ScopesOf(syntax.Scopes{}.Add(scopeA).Add(scopeB).Add(scopeC))
}

// TestTryGetBindingReportsTheLocalTie is the contract the datum-side readers
// need: the tie GetBinding raises comes back as (nil, true), with no panic.
func TestTryGetBindingReportsTheLocalTie(t *testing.T) {
	env, sym, q := ambiguousLocalFrame("foo")

	raised := capturePanic(func() {
		bnd, ambiguous := env.TryGetBinding(sym, q)
		qt.Assert(t, bnd, qt.IsNil, qt.Commentf("an ambiguous tie resolves to nothing"))
		qt.Assert(t, ambiguous, qt.IsTrue, qt.Commentf("and says so"))
	})
	qt.Assert(t, raised, qt.IsNil, qt.Commentf("TryGetBinding must not raise: got %v", raised))

	// The sibling still refuses, so this adds a reader rather than relaxing one.
	qt.Assert(t, capturePanic(func() {
		env.GetBinding(sym, q)
	}), qt.IsNotNil, qt.Commentf("GetBinding keeps raising on the same tie"))
}

// TestTryGetBindingReportsTheStoreTie is the same contract one layer down: the
// local chain is clean and the tie is between two sealed slots in the store, so
// the refusal comes from tryResolveRankedLocked rather than from the lexical
// walk.
func TestTryGetBindingReportsTheStoreTie(t *testing.T) {
	sc1 := syntax.NewScope()
	sc2 := syntax.NewScope()
	sym := values.NewSymbol("v")

	env := NewNamespaceFrame()
	g := env.GlobalEnvironment()
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, syntax.Scopes{}.Add(sc1), 0, true)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, syntax.Scopes{}.Add(sc2), 0, true)

	q := syntax.ScopesOf(syntax.Scopes{}.Add(sc1).Add(sc2))
	raised := capturePanic(func() {
		bnd, ambiguous := env.TryGetBinding(sym, q)
		qt.Assert(t, bnd, qt.IsNil)
		qt.Assert(t, ambiguous, qt.IsTrue)
	})
	qt.Assert(t, raised, qt.IsNil, qt.Commentf("TryGetBinding must not raise on a store tie: got %v", raised))

	qt.Assert(t, capturePanic(func() {
		env.GetBinding(sym, q)
	}), qt.IsNotNil, qt.Commentf("GetBinding keeps raising on the store tie"))
}

// TestTryGetBindingAgreesWithGetBindingWhenUnambiguous is the non-vacuity half:
// the ordinary path must be the SAME answer, not merely a non-panicking one.
// Every resolution in the tree goes through this body now.
func TestTryGetBindingAgreesWithGetBindingWhenUnambiguous(t *testing.T) {
	scope := syntax.NewScope()
	local := values.NewSymbol("local")
	global := values.NewSymbol("global")
	absent := values.NewSymbol("absent")

	top := NewNamespaceFrame()
	top.GlobalEnvironment().CreateGlobalBindingAt(global, BindingTypeVariable, syntax.Scopes{}, 0, false)
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), top)
	env.MaybeCreateLocalBinding(local, BindingTypeVariable,
		syntax.Scopes{}.Add(scope), nil)

	scoped := syntax.ScopesOf(syntax.Scopes{}.Add(scope))
	for _, row := range []struct {
		name string
		sym  *values.Symbol
		q    syntax.ScopeSet
	}{
		{"local hit, scoped query", local, scoped},
		{"local miss, empty query", local, syntax.EmptyScopes()},
		{"global hit, empty query", global, syntax.EmptyScopes()},
		{"global hit, wildcard query", global, syntax.AllScopes()},
		{"unbound", absent, scoped},
	} {
		t.Run(row.name, func(t *testing.T) {
			want := env.GetBinding(row.sym, row.q)
			got, ambiguous := env.TryGetBinding(row.sym, row.q)
			qt.Assert(t, ambiguous, qt.IsFalse)
			qt.Assert(t, got, qt.Equals, want)
		})
	}
}
