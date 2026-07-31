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

package syntax_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestSyntaxBoxUnwrapAll checks the container behaviour: a SyntaxBox unwraps to
// a *values.Box holding the unwrapped content, recursively.
func TestSyntaxBoxUnwrapAll(t *testing.T) {
	c := qt.New(t)

	inner := syntax.NewSyntaxObject(values.NewInteger(5), nil)
	bx := syntax.NewSyntaxBox(inner, nil)

	got, ok := bx.UnwrapAll().(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", bx.UnwrapAll()))
	c.Assert(values.Equal(got.Unbox(), values.NewInteger(5)), qt.IsTrue)

	// Nesting is preserved, unlike the widening introducers (#z#z5 is 5).
	outer := syntax.NewSyntaxBox(bx, nil)
	nested, ok := outer.UnwrapAll().(*values.Box)
	c.Assert(ok, qt.IsTrue)
	_, ok = nested.Unbox().(*values.Box)
	c.Assert(ok, qt.IsTrue, qt.Commentf("a box in a box must stay two deep, got %T", nested.Unbox()))
}

// TestSyntaxBoxUnwrapAllTerminatesOnCycle is why the UnwrapAllShared arm
// pre-registers its placeholder before recursing, exactly as the pair spine and
// the vector do. #0=#&#0# is the written form of a box that holds itself, and
// Wile's own writer emits it, so this is reachable from a round trip rather than
// only from a hand-built structure.
func TestSyntaxBoxUnwrapAllTerminatesOnCycle(t *testing.T) {
	c := qt.New(t)

	bx := syntax.NewSyntaxBox(nil, nil)
	bx.Value = bx

	got, ok := bx.UnwrapAll().(*values.Box)
	c.Assert(ok, qt.IsTrue)
	c.Assert(got.Unbox() == values.Value(got), qt.IsTrue,
		qt.Commentf("the unwrapped box must hold itself, not a copy"))
}

// TestSyntaxBoxAddScopePropagates is the provenance half of choosing a real
// SyntaxBox over a SyntaxObject holding a pre-unwrapped box: the content stays
// syntax, so a box in a macro template propagates scopes into what it holds.
func TestSyntaxBoxAddScopePropagates(t *testing.T) {
	c := qt.New(t)

	sym := syntax.NewSyntaxSymbol("x", nil)
	c.Assert(len(sym.Scopes()), qt.Equals, 0)

	bx := syntax.NewSyntaxBox(sym, nil)
	scope := syntax.NewScope()

	scoped, ok := bx.AddScope(scope).(*syntax.SyntaxBox)
	c.Assert(ok, qt.IsTrue)
	inner, ok := scoped.Value.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", scoped.Value))
	c.Assert(values.HasScope(inner.Scopes(), scope), qt.IsTrue,
		qt.Commentf("the scope must reach the boxed symbol"))

	// The original is untouched: every scope operation returns a new object.
	c.Assert(len(sym.Scopes()), qt.Equals, 0)
}
