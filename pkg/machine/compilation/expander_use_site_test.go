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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// newPrunerFixture builds a bare expander carrying a registry that already holds
// `registered`. Nothing else on the expander is reachable from the pruner, so
// this deliberately does not go through NewExpanderTimeContinuation, which would
// need a namespace and an evaluator.
func newPrunerFixture(registered ...*syntax.Scope) *ExpanderTimeContinuation {
	log := &useSiteScopeLog{scopes: values.NewMapSet[*syntax.Scope](len(registered))}
	for _, s := range registered {
		log.scopes.Set(s)
	}
	return &ExpanderTimeContinuation{useSiteScopes: log}
}

func scopedSym(name string, scopes ...*syntax.Scope) *syntax.SyntaxSymbol {
	return syntax.NewSyntaxSymbol(name, &syntax.SourceContext{Scopes: scopes})
}

// binderScopesOf reads the scope set off a form's binder, following both define
// shapes, so a test can assert on the result without re-implementing the walk.
func binderScopesOf(t *testing.T, form syntax.SyntaxValue) []*syntax.Scope {
	t.Helper()
	pair, ok := form.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	cdr, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	target := cdr.SyntaxCar()
	sym, ok := target.(*syntax.SyntaxSymbol)
	if ok {
		return sym.Scopes()
	}
	head, ok := target.(*syntax.SyntaxPair)
	qt.Assert(t, ok, qt.IsTrue)
	sym, ok = head.SyntaxCar().(*syntax.SyntaxSymbol)
	qt.Assert(t, ok, qt.IsTrue)
	return sym.Scopes()
}

// TestPruneUseSiteScopes covers the pruner's whole decision surface. The two
// rows that matter most are the pair "strips a registered scope" and "leaves an
// unregistered scope": the scope sets are the same shape and the verdicts are
// opposite, decided by the registry alone. That is the property the design rests
// on — Racket's own use-site pruning matches registered object identity and not
// the scope's kind tag, so a scope tagged 'use-site but never registered is
// never pruned.
func TestPruneUseSiteScopes(t *testing.T) {
	t.Run("strips a registered scope from (define name value)", func(t *testing.T) {
		us := syntax.NewScope()
		keep := syntax.NewScope()
		p := newPrunerFixture(us)
		form := syntaxList(nil, scopedSym("define"), scopedSym("foo", us, keep), scopedSym("v"))

		out := p.pruneUseSiteScopes(form)

		scopes := binderScopesOf(t, out)
		qt.Assert(t, scopes, qt.HasLen, 1)
		qt.Assert(t, scopes[0], qt.Equals, keep)
	})

	t.Run("leaves an unregistered scope alone", func(t *testing.T) {
		unregistered := syntax.NewScope()
		p := newPrunerFixture(syntax.NewScope())
		form := syntaxList(nil, scopedSym("define"), scopedSym("foo", unregistered), scopedSym("v"))

		out := p.pruneUseSiteScopes(form)

		// Identity, not merely equality: nothing changed, so nothing was rebuilt.
		qt.Assert(t, out, qt.Equals, form)
		scopes := binderScopesOf(t, out)
		qt.Assert(t, scopes, qt.HasLen, 1)
		qt.Assert(t, scopes[0], qt.Equals, unregistered)
	})

	t.Run("strips from the curried (define (name args) body) shape", func(t *testing.T) {
		us := syntax.NewScope()
		p := newPrunerFixture(us)
		form := syntaxList(nil,
			scopedSym("define"),
			syntaxList(nil, scopedSym("foo", us), scopedSym("a", us)),
			scopedSym("a", us),
		)

		out := p.pruneUseSiteScopes(form)

		qt.Assert(t, binderScopesOf(t, out), qt.HasLen, 0)
	})

	t.Run("strips from define-syntax, which extractDefineName excludes", func(t *testing.T) {
		us := syntax.NewScope()
		p := newPrunerFixture(us)
		form := syntaxList(nil, scopedSym("define-syntax"), scopedSym("inner", us), scopedSym("tx"))

		out := p.pruneUseSiteScopes(form)

		qt.Assert(t, binderScopesOf(t, out), qt.HasLen, 0)
	})

	t.Run("recurses through begin", func(t *testing.T) {
		us := syntax.NewScope()
		p := newPrunerFixture(us)
		inner := syntaxList(nil, scopedSym("define"), scopedSym("foo", us), scopedSym("v"))
		form := syntaxList(nil, scopedSym("begin"), scopedSym("other"), inner)

		out := p.pruneUseSiteScopes(form)

		outPair, ok := out.(*syntax.SyntaxPair)
		qt.Assert(t, ok, qt.IsTrue)
		rest, ok := outPair.SyntaxCdr().(*syntax.SyntaxPair)
		qt.Assert(t, ok, qt.IsTrue)
		tail, ok := rest.SyntaxCdr().(*syntax.SyntaxPair)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, binderScopesOf(t, tail.SyntaxCar()), qt.HasLen, 0)
	})

	t.Run("leaves a let binder alone: its references wear the scope too", func(t *testing.T) {
		us := syntax.NewScope()
		p := newPrunerFixture(us)
		form := syntaxList(nil,
			scopedSym("let"),
			syntaxList(nil, syntaxList(nil, scopedSym("v", us), scopedSym("7"))),
			scopedSym("v", us),
		)

		out := p.pruneUseSiteScopes(form)

		qt.Assert(t, out, qt.Equals, form)
	})

	t.Run("an empty registry is identity", func(t *testing.T) {
		us := syntax.NewScope()
		p := newPrunerFixture()
		form := syntaxList(nil, scopedSym("define"), scopedSym("foo", us), scopedSym("v"))

		qt.Assert(t, p.pruneUseSiteScopes(form), qt.Equals, form)
	})

	t.Run("a nil registry is identity", func(t *testing.T) {
		us := syntax.NewScope()
		p := &ExpanderTimeContinuation{}
		form := syntaxList(nil, scopedSym("define"), scopedSym("foo", us), scopedSym("v"))

		qt.Assert(t, p.pruneUseSiteScopes(form), qt.Equals, form)
	})
}
