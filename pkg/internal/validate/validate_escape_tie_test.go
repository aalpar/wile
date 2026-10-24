// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

func withScope(s *syntax.SyntaxSymbol, sc *syntax.Scope) *syntax.SyntaxSymbol {
	return s.AddScope(sc).(*syntax.SyntaxSymbol)
}

// markEscapedBindings resolves every body reference against the let's own frame,
// including references nested under a lambda whose parameter shadows them. With
// two incomparable let binders the nested reference ties there, though the real
// frame chain resolves it to the lambda's parameter. The analysis is best-effort
// and over-approximating, so a tie must not become a compile error.
func TestMarkEscapedBindingsNestedShadowTieDoesNotRaise(t *testing.T) {
	a, b, c := syntax.NewScope(), syntax.NewScope(), syntax.NewScope()
	childEnv := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	x := syntax.NewSyntaxSymbol("x", nil)
	ab := withScope(withScope(x, a), b)
	ac := withScope(withScope(x, a), c)
	abc := withScope(ab, c)
	bindLocalSymbol(childEnv, ab)
	bindLocalSymbol(childEnv, ac)
	bindings := []ValidatedLetBinding{{Name: ab, Init: lit()}, {Name: ac, Init: lit()}}
	body := []ValidatedExpr{&ValidatedLambda{
		formName: "lambda",
		params:   &ValidatedParams{Required: []*syntax.SyntaxSymbol{abc}},
		body:     []ValidatedExpr{&ValidatedSymbol{formName: "@symbol", Symbol: abc}},
	}}

	markEscapedBindings(childEnv, bindings, body, false)

	// Parity with the pre-fix over-approximation: one tied binder is marked.
	qt.Assert(t, bindings[0].Escapes || bindings[1].Escapes, qt.IsTrue)
}
