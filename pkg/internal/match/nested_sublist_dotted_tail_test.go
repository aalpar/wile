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

	qt "github.com/frankban/quicktest"
)

// TestNestedSublistDottedTail covers C12/C13: a pattern with a nested sub-list
// in NON-FINAL position followed by an improper `. rest` tail must match a
// multi-element input. Per R7RS §4.3.2, `(P1 ... Pn . Px)` matches an input of
// n-or-more elements where the first n elements match P1..Pn and the nth cdr
// matches Px.
//
// Pattern: `((x) . body)`  — element 0 is the nested sublist `(x)`; the improper
// tail `body` is a pattern variable that must capture the rest of the input.
// Input:   `((1) 9)`       — `(1)` matches `(x)` (binding x=1); `body` = `(9)`.
func TestNestedSublistDottedTail(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}, "body": {}}

	// Build pattern ((x) . body): a pair whose car is the sublist (x) and
	// whose cdr (improper tail) is the bare symbol body.
	pattern := syntax.NewSyntaxCons(
		testSyntaxList(testSyntaxSym("x")), // car: (x)
		testSyntaxSym("body"),              // improper tail: body
		nil,
	)

	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	// Input ((1) 9): proper list whose first element is (1) and whose rest is (9).
	target := testSyntaxList(
		testSyntaxList(testSyntaxInt(1)),
		testSyntaxInt(9),
	)

	matcher := NewMatcher(variables, compiler.codes)
	err = matcher.MatchSyntax(context.Background(), target)
	c.Assert(err, qt.IsNil, qt.Commentf("pattern ((x) . body) must match input ((1) 9)"))

	bindings := matcher.GetBindings()
	// x binds to 1 (the element inside the sublist).
	c.Assert(syntaxValuesEqualForMatch(bindings["x"], testSyntaxInt(1)), qt.IsTrue,
		qt.Commentf("x = %v, want 1", bindings["x"]))
	// body binds to the rest of the input after the first element: (9).
	wantBody := testSyntaxList(testSyntaxInt(9))
	c.Assert(syntaxValuesEqualForMatch(bindings["body"], wantBody), qt.IsTrue,
		qt.Commentf("body = %v, want (9)", bindings["body"]))
}

// TestEllipsisThenImproperTail covers the companion matcher bug that also
// blocks (chibi optional): an ellipsis followed by an improper `. rest` tail,
// e.g. `(a ... . rest)`. The ellipsis greedily consumes the leading elements of
// a PROPER-list input and `rest` binds to `()` (the cdr of the exhausted list).
// The chibi `let-optionals` macro relies on exactly this over a proper argument
// list: `(var&default ... . rest)`.
//
// KNOWN LIMITATION (pinned by the "improper input" subtest below): a genuinely
// improper input like `(1 2 . 5)` does NOT match — the ellipsis loop's VisitCdr
// cannot traverse a dotted input tail (verified identical on master). This is
// outside C12/C13's scope (nested-sublist + improper-PATTERN-tail), and chibi
// feeds only proper lists, so it does not block the libraries.
//
// Before the fix, after the ellipsis consumed every element the matcher
// position was the empty list and ByteCodeCaptureCdr rejected it as no-match,
// so even a proper-list input never matched.
func TestEllipsisThenImproperTail(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"a": {}, "rest": {}}

	// Pattern (a ... . rest):
	//   (a . (... . rest))  — a, then ellipsis, then improper tail rest.
	pattern := syntax.NewSyntaxCons(
		testSyntaxSym("a"),
		syntax.NewSyntaxCons(
			testSyntaxSym("..."),
			testSyntaxSym("rest"),
			nil,
		),
		nil,
	)

	compileAndMatch := func(target *syntax.SyntaxPair) (*Matcher, error) {
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		err := compiler.Compile(context.TODO(), pattern)
		c.Assert(err, qt.IsNil)
		m := NewMatcher(variables, compiler.codes, WithEllipsisVars(compiler.ellipsisVars))
		matchErr := m.MatchSyntax(context.Background(), target)
		return m, matchErr
	}

	// Proper list (1 2 3): ellipsis consumes 1 2 3; rest = ().
	c.Run("proper list", func(c *qt.C) {
		target := testSyntaxList(testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3))
		m, err := compileAndMatch(target)
		c.Assert(err, qt.IsNil, qt.Commentf("(a ... . rest) must match (1 2 3)"))
		rest := m.GetBindings()["rest"]
		c.Assert(syntax.IsSyntaxEmptyList(rest), qt.IsTrue,
			qt.Commentf("rest = %v, want ()", rest))
	})

	// Single element (7): ellipsis consumes 7; rest = ().
	c.Run("single element", func(c *qt.C) {
		target := testSyntaxList(testSyntaxInt(7))
		m, err := compileAndMatch(target)
		c.Assert(err, qt.IsNil, qt.Commentf("(a ... . rest) must match (7)"))
		rest := m.GetBindings()["rest"]
		c.Assert(syntax.IsSyntaxEmptyList(rest), qt.IsTrue,
			qt.Commentf("rest = %v, want ()", rest))
	})

	// Improper input (1 2 . 5): does NOT match — known limitation, pinned so
	// the docstring's claim stays honest. The ellipsis loop cannot traverse a
	// dotted input tail; this is verified identical on master and is outside
	// C12/C13's scope (chibi feeds only proper lists).
	c.Run("improper input not matched", func(c *qt.C) {
		target := syntax.NewSyntaxCons(
			testSyntaxInt(1),
			syntax.NewSyntaxCons(testSyntaxInt(2), testSyntaxInt(5), nil),
			nil,
		)
		_, err := compileAndMatch(target)
		c.Assert(err, qt.IsNotNil,
			qt.Commentf("(a ... . rest) does not match improper input (1 2 . 5)"))
	})
}
