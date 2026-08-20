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

package match

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// A `#&` box is a container, like a vector: the compiler descends into it, the
// matcher descends into the input's box, and the expander expands its content.
// Review wave 5, 2.2.13.

func testSyntaxBox(v syntax.SyntaxValue) *syntax.SyntaxBox {
	return syntax.NewSyntaxBox(v, nil)
}

func TestVisitCarAsBoxString(t *testing.T) {
	c := qt.New(t)
	c.Assert(ByteCodeVisitCarAsBox{}.String(), qt.Equals, "VisitCarAsBox")
}

func TestCompileAndMatchBoxPattern(t *testing.T) {
	c := qt.New(t)

	// Pattern: (m #&y)
	pattern := testSyntaxList(testSyntaxSym("m"), testSyntaxBox(testSyntaxSym("y")))
	variables := values.StringSet{"y": {}}

	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	c.Run("box input binds the content", func(c *qt.C) {
		m := NewMatcher(variables, compiled.Codes)
		input := testSyntaxList(testSyntaxSym("m"), testSyntaxBox(testSyntaxInt(7)))
		c.Assert(m.MatchSyntax(context.Background(), input), qt.IsNil)
		c.Assert(syntaxValuesEqualForMatch(m.GetBindings()["y"], testSyntaxInt(7)), qt.IsTrue)
	})

	c.Run("bare input does not match a box pattern", func(c *qt.C) {
		m := NewMatcher(variables, compiled.Codes)
		input := testSyntaxList(testSyntaxSym("m"), testSyntaxInt(7))
		c.Assert(m.MatchSyntax(context.Background(), input), qt.ErrorIs, ErrNotAMatch)
	})

	c.Run("exhausted position does not match a box pattern", func(c *qt.C) {
		m := NewMatcher(variables, compiled.Codes)
		input := testSyntaxList(testSyntaxSym("m"))
		c.Assert(m.MatchSyntax(context.Background(), input), qt.ErrorIs, ErrNotAMatch)
	})
}

// A box sub-pattern under an ellipsis has to repeat like any other container,
// which is what the pattern analyzer's box arm is for: the names have to travel
// up to the enclosing pair for compileEllipsis to claim them.
func TestCompileAndMatchBoxUnderEllipsis(t *testing.T) {
	c := qt.New(t)

	// Pattern: (m #&y ...)
	pattern := testSyntaxList(
		testSyntaxSym("m"),
		testSyntaxBox(testSyntaxSym("y")),
		testSyntaxSym("..."),
	)
	variables := values.StringSet{"y": {}}

	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	m := NewMatcher(variables, compiled.Codes, WithEllipsisVars(compiled.EllipsisVars))
	input := testSyntaxList(
		testSyntaxSym("m"),
		testSyntaxBox(testSyntaxInt(1)),
		testSyntaxBox(testSyntaxInt(2)),
	)
	c.Assert(m.MatchSyntax(context.Background(), input), qt.IsNil)
	c.Assert(ellipsisCaptureCount(m, "y"), qt.Equals, 2)
}

func TestExpandBoxTemplate(t *testing.T) {
	c := qt.New(t)

	// Pattern (m x), so `x` is captured from the input.
	pattern := testSyntaxList(testSyntaxSym("m"), testSyntaxSym("x"))
	variables := values.StringSet{"x": {}}
	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	newMatched := func(c *qt.C) *SyntaxMatcher {
		sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
		input := testSyntaxList(testSyntaxSym("m"), testSyntaxInt(5))
		c.Assert(sm.Match(context.Background(), input), qt.IsNil)
		return sm
	}

	c.Run("box content is substituted", func(c *qt.C) {
		sm := newMatched(c)
		expanded, err := sm.Expand(testSyntaxBox(testSyntaxSym("x")), ExpandOptions{})
		c.Assert(err, qt.IsNil)
		box, ok := expanded.(*syntax.SyntaxBox)
		c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", expanded))
		c.Assert(syntaxValuesEqualForMatch(box.Value, testSyntaxInt(5)), qt.IsTrue,
			qt.Commentf("box content = %v", box.Value))
	})

	c.Run("a void box expands to itself", func(c *qt.C) {
		sm := newMatched(c)
		empty := testSyntaxBox(nil)
		expanded, err := sm.Expand(empty, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(expanded, qt.Equals, syntax.SyntaxValue(empty))
	})

	c.Run("the ellipsis escape descends into a box", func(c *qt.C) {
		sm := newMatched(c)
		// (... #&x) — the escape suppresses the ellipsis's meaning, not
		// pattern-variable substitution.
		template := testSyntaxList(testSyntaxSym("..."), testSyntaxBox(testSyntaxSym("x")))
		expanded, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		box, ok := expanded.(*syntax.SyntaxBox)
		c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", expanded))
		c.Assert(syntaxValuesEqualForMatch(box.Value, testSyntaxInt(5)), qt.IsTrue)
	})

	c.Run("the ellipsis escape leaves a void box alone", func(c *qt.C) {
		sm := newMatched(c)
		empty := testSyntaxBox(nil)
		template := testSyntaxList(testSyntaxSym("..."), empty)
		expanded, err := sm.Expand(template, ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(expanded, qt.Equals, syntax.SyntaxValue(empty))
	})

	c.Run("a pattern variable inside a box is found", func(c *qt.C) {
		sm := newMatched(c)
		vars := sm.findSyntaxPatternVariables(testSyntaxBox(testSyntaxSym("x")))
		ok := vars.Get("x")
		c.Assert(ok, qt.IsTrue, qt.Commentf("vars = %v", vars))
	})
}
