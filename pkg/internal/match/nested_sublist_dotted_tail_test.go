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

	variables := syntax.PatternVarSymbols{"x": {}, "body": {}}

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
// A genuinely improper input like `(1 2 . 5)` matches too, binding rest to the
// dotted tail (review wave 5, 2.3.4). ByteCodeVisitCdr used to refuse a dotted
// input tail outright, which killed every improper input; it now exhausts the
// position and carries the tail, so the ellipsis loop halts AFTER the last
// capture rather than before it. Halting before it would silently drop the last
// element — `(1 2 . 5)` would yield a=(1) rest=5 instead of a=(1 2) rest=5 —
// which is why the subtest below asserts the captured VALUES, not just a match.
//
// Before the C12/C13 fix, after the ellipsis consumed every element the matcher
// position was the empty list and ByteCodeCaptureCdr rejected it as no-match,
// so even a proper-list input never matched.
func TestEllipsisThenImproperTail(t *testing.T) {
	c := qt.New(t)

	variables := syntax.PatternVarSymbols{"a": {}, "rest": {}}

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

	// Improper input (1 2 . 5): ellipsis consumes 1 and 2; rest = 5. Racket
	// gives ((1 2) 5) for the same pattern and input.
	c.Run("improper input", func(c *qt.C) {
		target := syntax.NewSyntaxCons(
			testSyntaxInt(1),
			syntax.NewSyntaxCons(testSyntaxInt(2), testSyntaxInt(5), nil),
			nil,
		)
		m, err := compileAndMatch(target)
		c.Assert(err, qt.IsNil,
			qt.Commentf("(a ... . rest) must match improper input (1 2 . 5)"))
		rest := m.GetBindings()["rest"]
		c.Assert(syntaxValuesEqualForMatch(rest, testSyntaxInt(5)), qt.IsTrue,
			qt.Commentf("rest = %v, want 5", rest))
		// The loop must halt with the position on the LAST pair, so both 1 and
		// 2 are captured. An exit test at the loop head captures only 1 and
		// still satisfies the rest assertion above.
		c.Assert(ellipsisCaptureCount(m, "a"), qt.Equals, 2)
	})

	// A dotted input tail that NO pattern element consumes is still a
	// mismatch: `(a ...)` is a proper-list pattern.
	c.Run("improper input against a proper pattern", func(c *qt.C) {
		compiler := NewSyntaxCompiler()
		properVars := syntax.PatternVarSymbols{"a": {}}
		compiler.variables = properVars
		properPattern := syntax.NewSyntaxCons(
			testSyntaxSym("a"),
			testSyntaxList(testSyntaxSym("...")),
			nil,
		)
		err := compiler.Compile(context.TODO(), properPattern)
		c.Assert(err, qt.IsNil)
		m := NewMatcher(properVars, compiler.codes, WithEllipsisVars(compiler.ellipsisVars))
		target := syntax.NewSyntaxCons(
			testSyntaxInt(1),
			syntax.NewSyntaxCons(testSyntaxInt(2), testSyntaxInt(5), nil),
			nil,
		)
		matchErr := m.MatchSyntax(context.Background(), target)
		c.Assert(matchErr, qt.ErrorIs, ErrNotAMatch,
			qt.Commentf("(a ...) must not match improper input (1 2 . 5)"))
	})
}

// ellipsisCaptureCount counts the per-iteration capture contexts that bound
// name, across every ellipsis group. The root context holds no ellipsis
// captures, so this is how many repetitions the loop actually ran.
func ellipsisCaptureCount(m *Matcher, name string) int {
	count := 0
	for _, children := range m.captureStack[0].children {
		for _, child := range children {
			_, ok := child.bindings[name]
			if ok {
				count++
			}
		}
	}
	return count
}
