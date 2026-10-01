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

// TestW2HeadFormNameDoesNotRaiseOnAmbiguity is validate.markerName's twin on
// this side of the validate -> machine edge, and it is a gate for the same
// reason: headFormName is asked what a head LOOKS LIKE before anything commits
// to compiling it, so an incomparable scope-set tie must come back as "denotes
// no form" and leave the spelling standing. The reference's own resolution
// still raises later if the program really does reference it.
//
// The pair must agree — quasiHeadDepth's doc states that as a requirement — so
// a one-sided change here would make the inliner's opaque scan permissive where
// the compiler's walk says live. Red on master, where this panics with
// `GetBinding: identifier "foo" resolves ambiguously among incomparable
// hygienic scope sets: ambiguous binding`.
func TestW2HeadFormNameDoesNotRaiseOnAmbiguity(t *testing.T) {
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
		got = headFormName(env, stx)
		return nil
	}()

	qt.Assert(t, raised, qt.IsNil,
		qt.Commentf("headFormName raised on an ambiguous head: %v", raised))
	qt.Assert(t, got, qt.Equals, "foo",
		qt.Commentf("a refused tie denotes no form, so the spelling stands"))
}

// TestW2HeadFormNameStillDenotesUnderNoAmbiguity is the non-vacuity guard, and
// like its validate twin it has to use a RENAMED keyword: a spelling equal to
// its own denotation is also what a headFormName that resolved nothing would
// answer.
func TestW2HeadFormNameStillDenotesUnderNoAmbiguity(t *testing.T) {
	scope := syntax.NewScope()
	scopes := syntax.Scopes{}.Add(scope)

	env := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	sym := values.NewSymbol("mylambda")
	env.MaybeCreateLocalBinding(sym, environment.BindingTypeSyntax, scopes, nil)
	env.GetBinding(sym, syntax.ScopesOf(scopes)).SetValue(environment.NewFormKeyword("lambda"))

	stx := syntax.NewSyntaxSymbol("mylambda", &syntax.SourceContext{Scopes: scopes})

	qt.Assert(t, headFormName(env, stx), qt.Equals, "lambda",
		qt.Commentf("a renamed keyword still denotes its form; spelling-only would answer mylambda"))
}
