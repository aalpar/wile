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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

// bytecodeEqual compares two bytecode slices by their string representation.
// This avoids issues with qt.DeepEquals not being able to compare unexported fields.
func bytecodeEqual(a, b []SyntaxCommand) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if fmt.Sprintf("%v", a[i]) != fmt.Sprintf("%v", b[i]) {
			return false
		}
	}
	return true
}

func TestUtilsMatcherSuites(t *testing.T) {
	c := qt.New(t)
	qtsuite.Run(c, UtilsMatcherSuite{})
}

type UtilsMatcherSuite struct{}

func (UtilsMatcherSuite) TestMatchCompile(c *qt.C) {
	tcs := []struct {
		variables map[string]struct{}
		in        *syntax.SyntaxPair
		out       []SyntaxCommand
	}{
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: testSyntaxList(testSyntaxInt(10), testSyntaxSym("a")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeVisitCdr{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in:        testSyntaxList(testSyntaxList(testSyntaxInt(10)), testSyntaxInt(20)),
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxInt(20)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: testSyntaxList(testSyntaxInt(10), testSyntaxList(testSyntaxSym("a"), testSyntaxSym("b")), testSyntaxInt(40)),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxInt(40)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in:        testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30)),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxInt(20)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxInt(30)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b")), testSyntaxSym("...")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxInt(20)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("a")},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxSym("...")},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b")), testSyntaxSym("...")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxInt(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxInt(20)},
				ByteCodeVisitCdr{},
				// SkipIfEmpty checks for empty list before executing loop body
				ByteCodeSkipIfEmpty{Offset: 9},
				// mark
				ByteCodePushContext{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeDone{},
				ByteCodePopContext{},
				ByteCodeJump{Offset: -8},
				ByteCodeDone{},
			},
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d", i), func(c *qt.C) {
			vst := NewSyntaxCompiler()
			vst.variables = tc.variables
			vst.Compile(context.TODO(), tc.in) //nolint:errcheck
			c.Assert(bytecodeEqual(vst.codes, tc.out), qt.IsTrue,
				qt.Commentf("got %v, want %v", vst.codes, tc.out))
		})
	}
}

func (UtilsMatcherSuite) TestMatchExecute(c *qt.C) {
	tcs := []struct {
		variables map[string]struct{}
		in        *syntax.SyntaxPair
		target    *syntax.SyntaxPair
		matches   bool
	}{
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in:      testSyntaxList(testSyntaxInt(10), testSyntaxSym("a")),
			target:  testSyntaxList(testSyntaxInt(10), testSyntaxInt(20)),
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			in:        testSyntaxList(testSyntaxList(testSyntaxInt(10)), testSyntaxInt(20)),
			target:    testSyntaxList(testSyntaxList(testSyntaxInt(10)), testSyntaxInt(20)),
			matches:   true,
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: testSyntaxList(
				testSyntaxInt(10), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b"),
				), testSyntaxInt(40),
			),
			target: testSyntaxList(
				testSyntaxInt(10), testSyntaxList(
					testSyntaxInt(20), testSyntaxSym("b"),
				), testSyntaxInt(40),
			),
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			in:        testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30)),
			target:    testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30)),
			matches:   true,
		},
		{
			variables: map[string]struct{}{},
			in: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b")), testSyntaxSym("...")),
			target: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b")), testSyntaxSym("...")),
			matches: true,
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxSym("a"), testSyntaxSym("b"),
				), testSyntaxSym("..."),
			),
			target: testSyntaxList(
				testSyntaxInt(10), testSyntaxInt(20), testSyntaxList(
					testSyntaxInt(30), testSyntaxSym("b"),
				),
				testSyntaxList(
					testSyntaxInt(40), testSyntaxSym("b"),
				),
			),
			matches: true,
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d", i), func(c *qt.C) {
			vst := NewSyntaxCompiler()
			vst.variables = tc.variables
			err := vst.Compile(context.TODO(), tc.in)
			c.Assert(err, qt.IsNil)
			mtc := NewMatcher(vst.variables, vst.codes)
			err = mtc.MatchSyntax(context.Background(), tc.target)
			if tc.matches {
				c.Assert(err, qt.IsNil, qt.Commentf("expected match"))
			} else {
				c.Assert(err, qt.ErrorIs, ErrNotAMatch, qt.Commentf("expected no match"))
			}
		})
	}
}

func (UtilsMatcherSuite) TestInsert(c *qt.C) {
	tcs := []struct {
		in  []SyntaxCommand
		out []SyntaxCommand
		i   int
	}{
		{
			i: 0,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 1,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 2,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 3,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -3},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 4,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeDone{},
				ByteCodeJump{Offset: -3},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 5,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeDone{},
				ByteCodeVisitCar{},
			},
		},
		{
			i: 6,
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSym("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
				ByteCodeDone{},
			},
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %q", i, tc.in), func(c *qt.C) {
			q := insert(tc.i, tc.in, []SyntaxCommand{ByteCodeDone{}})
			c.Assert(bytecodeEqual(q, tc.out), qt.IsTrue,
				qt.Commentf("got %v, want %v", q, tc.out))
		})
	}
}

// testSyntaxVec creates a SyntaxVector with the given elements for tests.
func testSyntaxVec(elems ...syntax.SyntaxValue) *syntax.SyntaxVector {
	return syntax.NewSyntaxVector(nil, elems...)
}

func TestCompileVectorPattern(t *testing.T) {
	c := qt.New(t)

	c.Run("vector with two captures", func(c *qt.C) {
		// Pattern: (foo #(x y))
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		expected := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxSym("foo")},
			ByteCodeVisitCdr{},
			ByteCodeVisitCarAsVector{},
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeVisitCdr{},
			ByteCodeCaptureCar{Binding: "y"},
			ByteCodeDone{},
			ByteCodeDone{},
		}
		c.Assert(bytecodeEqual(compiler.codes, expected), qt.IsTrue,
			qt.Commentf("got %v, want %v", compiler.codes, expected))
	})

	c.Run("empty vector", func(c *qt.C) {
		// Pattern: (foo #())
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(),
		)
		compiler := NewSyntaxCompiler()
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		expected := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxSym("foo")},
			ByteCodeVisitCdr{},
			ByteCodeRequireCarEmptyVector{},
			ByteCodeDone{},
		}
		c.Assert(bytecodeEqual(compiler.codes, expected), qt.IsTrue,
			qt.Commentf("got %v, want %v", compiler.codes, expected))
	})

	c.Run("vector with literal and capture", func(c *qt.C) {
		// Pattern: (foo #(10 x))
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxInt(10), testSyntaxSym("x")),
		)
		variables := map[string]struct{}{"x": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		expected := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxSym("foo")},
			ByteCodeVisitCdr{},
			ByteCodeVisitCarAsVector{},
			ByteCodeCompareCar{Value: testSyntaxInt(10)},
			ByteCodeVisitCdr{},
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeDone{},
			ByteCodeDone{},
		}
		c.Assert(bytecodeEqual(compiler.codes, expected), qt.IsTrue,
			qt.Commentf("got %v, want %v", compiler.codes, expected))
	})

	c.Run("ellipsis applied to vector subpattern", func(c *qt.C) {
		// Pattern: (foo #(x y) ...)
		// Ellipsis repeats the entire vector subpattern.
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
			testSyntaxSym("..."),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		expected := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxSym("foo")}, // 0
			ByteCodeVisitCdr{},                 // 1
			ByteCodeSkipIfEmpty{Offset: 9},     // 2 → 11
			ByteCodePushContext{EllipsisID: 0}, // 3
			ByteCodeVisitCarAsVector{},         // 4
			ByteCodeCaptureCar{Binding: "x"},   // 5
			ByteCodeVisitCdr{},                 // 6
			ByteCodeCaptureCar{Binding: "y"},   // 7
			ByteCodeDone{},                     // 8
			ByteCodePopContext{EllipsisID: 0},  // 9
			ByteCodeJump{Offset: -8},           // 10 → 2
			ByteCodeDone{},                     // 11
		}
		c.Assert(bytecodeEqual(compiler.codes, expected), qt.IsTrue,
			qt.Commentf("got %v, want %v", compiler.codes, expected))

		// Verify ellipsis captures both variables
		c.Assert(compiler.ellipsisVars[0], qt.ContentEquals, map[string]struct{}{
			"x": {},
			"y": {},
		})
	})
}

func TestExecuteVectorPattern(t *testing.T) {
	c := qt.New(t)

	c.Run("compile and match vector", func(c *qt.C) {
		// Pattern: (foo #(x y)), Input: (foo #(1 2))
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxInt(1), testSyntaxInt(2)),
		)
		matcher := NewMatcher(variables, compiler.codes)
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)

		bindings := matcher.GetBindings()
		c.Assert(syntaxValuesEqualForMatch(bindings["x"], testSyntaxInt(1)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(bindings["y"], testSyntaxInt(2)), qt.IsTrue)
	})

	c.Run("vector mismatch - list input", func(c *qt.C) {
		// Pattern: (foo #(x)), Input: (foo (1)) — list, not vector
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x")),
		)
		variables := map[string]struct{}{"x": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxList(testSyntaxInt(1)),
		)
		matcher := NewMatcher(variables, compiler.codes)
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("vector mismatch - wrong length", func(c *qt.C) {
		// Pattern: (foo #(x y)), Input: (foo #(1)) — too few elements
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxInt(1)),
		)
		matcher := NewMatcher(variables, compiler.codes)
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("empty vector match and mismatch", func(c *qt.C) {
		// Pattern: (foo #()), Input: (foo #()) — match
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(),
		)
		compiler := NewSyntaxCompiler()
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(),
		)
		matcher := NewMatcher(map[string]struct{}{}, compiler.codes)
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)

		// Input: (foo #(1)) — non-empty, should not match
		target2 := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxInt(1)),
		)
		matcher2 := NewMatcher(map[string]struct{}{}, compiler.codes)
		err = matcher2.MatchSyntax(context.Background(), target2)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("ellipsis applied to vector subpattern", func(c *qt.C) {
		// Pattern: (foo #(x y) ...), Input: (foo #(1 2) #(3 4))
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
			testSyntaxSym("..."),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxInt(1), testSyntaxInt(2)),
			testSyntaxVec(testSyntaxInt(3), testSyntaxInt(4)),
		)
		matcher := NewMatcher(variables, compiler.codes, WithEllipsisVars(compiler.ellipsisVars))
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)

		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 2)
		c.Assert(syntaxValuesEqualForMatch(children[0].bindings["x"], testSyntaxInt(1)), qt.IsTrue,
			qt.Commentf("iter 0: x = %v", children[0].bindings["x"]))
		c.Assert(syntaxValuesEqualForMatch(children[0].bindings["y"], testSyntaxInt(2)), qt.IsTrue,
			qt.Commentf("iter 0: y = %v", children[0].bindings["y"]))
		c.Assert(syntaxValuesEqualForMatch(children[1].bindings["x"], testSyntaxInt(3)), qt.IsTrue,
			qt.Commentf("iter 1: x = %v", children[1].bindings["x"]))
		c.Assert(syntaxValuesEqualForMatch(children[1].bindings["y"], testSyntaxInt(4)), qt.IsTrue,
			qt.Commentf("iter 1: y = %v", children[1].bindings["y"]))
	})

	c.Run("ellipsis applied to vector - zero repetitions", func(c *qt.C) {
		// Pattern: (foo #(x y) ...), Input: (foo) — zero vectors
		pattern := testSyntaxList(
			testSyntaxSym("foo"),
			testSyntaxVec(testSyntaxSym("x"), testSyntaxSym("y")),
			testSyntaxSym("..."),
		)
		variables := map[string]struct{}{"x": {}, "y": {}}
		compiler := NewSyntaxCompiler()
		compiler.variables = variables
		compiler.Compile(context.TODO(), pattern) //nolint:errcheck

		target := testSyntaxList(testSyntaxSym("foo"))
		matcher := NewMatcher(variables, compiler.codes, WithEllipsisVars(compiler.ellipsisVars))
		err := matcher.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)

		// Zero repetitions: no children
		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 0)
	})
}

// TestCompileDottedWildcardTail pins R7RS §4.3.2: `_` is the wildcard in a
// dotted tail as well as in an element position — it matches the rest of the
// input but must never bind, so a template `_` stays a free identifier. Before
// the fix the tail compiled to CaptureCdr and `_` was substituted.
func TestCompileDottedWildcardTail(t *testing.T) {
	c := qt.New(t)

	// Pattern: (m a . _) — build the improper tail explicitly.
	pattern := syntax.NewSyntaxCons(
		testSyntaxSym("m"),
		syntax.NewSyntaxCons(
			testSyntaxSym("a"),
			testSyntaxSym("_"),
			nil,
		),
		nil,
	)

	// `_` is a pattern variable as far as the caller's variable set is concerned;
	// the wildcard rule must win regardless.
	variables := map[string]struct{}{"a": {}, "_": {}}

	compiled, err := CompileSyntaxPattern(context.TODO(), pattern, variables, nil)
	c.Assert(err, qt.IsNil)

	codes := fmt.Sprintf("%v", compiled.Codes)
	c.Assert(codes, qt.Contains, "DiscardCdr")
	c.Assert(codes, qt.Not(qt.Contains), "CaptureCdr")

	tcs := []struct {
		name  string
		input *syntax.SyntaxPair
	}{
		{
			name:  "non-empty tail",
			input: testSyntaxList(testSyntaxSym("m"), testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3)),
		},
		{
			name:  "empty tail",
			input: testSyntaxList(testSyntaxSym("m"), testSyntaxInt(1)),
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			sm := NewSyntaxMatcher(variables, compiled.Codes, &SyntaxMatcherOpts{EllipsisVars: compiled.EllipsisVars})
			err := sm.Match(context.Background(), tc.input)
			c.Assert(err, qt.IsNil)

			bindings := sm.GetBindings()
			_, bound := bindings["_"]
			c.Assert(bound, qt.IsFalse, qt.Commentf("wildcard `_` must not bind, bindings: %v", bindings))
			_, bound = bindings["a"]
			c.Assert(bound, qt.IsTrue)
		})
	}
}
