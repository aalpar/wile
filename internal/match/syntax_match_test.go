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
	"testing"

	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

// testSyntaxList builds a proper syntax list from the given elements.
func testSyntaxList(elems ...syntax.SyntaxValue) *syntax.SyntaxPair {
	var q syntax.SyntaxValue = syntax.NewSyntaxEmptyList(nil)
	for i := len(elems) - 1; i >= 0; i-- {
		q = syntax.NewSyntaxCons(elems[i], q, nil)
	}
	return q.(*syntax.SyntaxPair)
}

func TestMatchSyntax(t *testing.T) {
	c := qt.New(t)

	c.Run("flat list match", func(c *qt.C) {
		// Pattern: (10 20 30)
		// Bytecode: CompareCar(10), VisitCdr, CompareCar(20), VisitCdr, CompareCar(30), Done
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxInt(10)},
			ByteCodeVisitCdr{},
			ByteCodeCompareCar{Value: testSyntaxInt(20)},
			ByteCodeVisitCdr{},
			ByteCodeCompareCar{Value: testSyntaxInt(30)},
			ByteCodeDone{},
		}
		target := testSyntaxList(testSyntaxInt(10), testSyntaxInt(20), testSyntaxInt(30))
		matcher := NewMatcher(map[string]struct{}{}, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)
	})

	c.Run("nested list match", func(c *qt.C) {
		// Pattern: ((10) 20)
		// Bytecode: VisitCar, CompareCar(10), Done, CompareCar(20), Done
		codes := []SyntaxCommand{
			ByteCodeVisitCar{},
			ByteCodeCompareCar{Value: testSyntaxInt(10)},
			ByteCodeDone{},
			ByteCodeCompareCar{Value: testSyntaxInt(20)},
			ByteCodeDone{},
		}
		inner := testSyntaxList(testSyntaxInt(10))
		target := testSyntaxList(inner, testSyntaxInt(20))
		matcher := NewMatcher(map[string]struct{}{}, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)
	})

	c.Run("capture with ellipsis", func(c *qt.C) {
		// Pattern: (x ...) where x is a pattern variable
		// Bytecode:
		//   0: SkipIfEmpty{Offset: 5}   -> jumps to index 5 (0+5=5, but -1 for i++ => 4, then i++ => 5)
		//   1: PushContext{EllipsisID: 0}
		//   2: CaptureCar{Binding: "x"}
		//   3: VisitCdr
		//   4: PopContext{EllipsisID: 0}
		//   5: Jump{Offset: -5}         -> jumps back to SkipIfEmpty (5 + (-5) - 1 + 1 = 0)
		//   6: Done
		codes := []SyntaxCommand{
			ByteCodeSkipIfEmpty{Offset: 6},     // 0: skip to Done (index 6) if empty
			ByteCodePushContext{EllipsisID: 0}, // 1
			ByteCodeCaptureCar{Binding: "x"},   // 2
			ByteCodeVisitCdr{},                 // 3
			ByteCodePopContext{EllipsisID: 0},  // 4
			ByteCodeJump{Offset: -5},           // 5: jump back to index 0
			ByteCodeDone{},                     // 6
		}
		target := testSyntaxList(testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3))
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)

		bindings := matcher.GetBindings()
		c.Assert(bindings, qt.IsNotNil)
		// Bindings for ellipsis variables are in child contexts, not the root.
		// The root should have children with the captured values.
		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 3)
		c.Assert(syntaxValuesEqualForMatch(children[0].bindings["x"], testSyntaxInt(1)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(children[1].bindings["x"], testSyntaxInt(2)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(children[2].bindings["x"], testSyntaxInt(3)), qt.IsTrue)
	})

	c.Run("mismatch", func(c *qt.C) {
		// Pattern: (10 20) but target is (10 30)
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: testSyntaxInt(10)},
			ByteCodeVisitCdr{},
			ByteCodeCompareCar{Value: testSyntaxInt(20)},
			ByteCodeDone{},
		}
		target := testSyntaxList(testSyntaxInt(10), testSyntaxInt(30))
		matcher := NewMatcher(map[string]struct{}{}, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("improper list capture", func(c *qt.C) {
		// Pattern: (a . rest) where a and rest are pattern variables
		// Bytecode: CaptureCar("a"), CaptureCdr("rest"), Done
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "a"},
			ByteCodeCaptureCdr{Binding: "rest"},
			ByteCodeDone{},
		}
		// Create improper pair (1 . 2)
		target := syntax.NewSyntaxCons(testSyntaxInt(1), testSyntaxInt(2), nil)
		variables := map[string]struct{}{"a": {}, "rest": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)

		bindings := matcher.GetBindings()
		c.Assert(syntaxValuesEqualForMatch(bindings["a"], testSyntaxInt(1)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(bindings["rest"], testSyntaxInt(2)), qt.IsTrue)
	})
}

func TestMatchSyntaxWithLiterals(t *testing.T) {
	c := qt.New(t)

	c.Run("literal match success", func(c *qt.C) {
		// Pattern: (if x) where "if" is a literal
		// Bytecode: CompareCar("if"), VisitCdr, CaptureCar("x"), Done
		ifSym := syntax.NewSyntaxSymbol("if", nil)
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: ifSym},
			ByteCodeVisitCdr{},
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeDone{},
		}
		literalSyntax := map[string]*syntax.SyntaxSymbol{
			"if": ifSym,
		}
		literalMatcher := func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool {
			return true // Always match for this test
		}
		target := testSyntaxList(syntax.NewSyntaxSymbol("if", nil), testSyntaxInt(42))
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntaxWithLiterals(target, literalSyntax, literalMatcher)
		c.Assert(err, qt.IsNil)

		bindings := matcher.GetBindings()
		c.Assert(syntaxValuesEqualForMatch(bindings["x"], testSyntaxInt(42)), qt.IsTrue)
	})

	c.Run("literal mismatch via matcher callback", func(c *qt.C) {
		// Pattern: (if x) where "if" is a literal, but literalMatcher returns false
		ifSym := syntax.NewSyntaxSymbol("if", nil)
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: ifSym},
			ByteCodeVisitCdr{},
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeDone{},
		}
		literalSyntax := map[string]*syntax.SyntaxSymbol{
			"if": ifSym,
		}
		literalMatcher := func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool {
			return false // Scope mismatch simulation
		}
		target := testSyntaxList(syntax.NewSyntaxSymbol("if", nil), testSyntaxInt(42))
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntaxWithLiterals(target, literalSyntax, literalMatcher)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("non-symbol at literal position", func(c *qt.C) {
		// Pattern expects literal "if" but target has integer 42 at that position
		ifSym := syntax.NewSyntaxSymbol("if", nil)
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: ifSym},
			ByteCodeDone{},
		}
		literalSyntax := map[string]*syntax.SyntaxSymbol{
			"if": ifSym,
		}
		literalMatcher := func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool {
			return true
		}
		target := testSyntaxList(testSyntaxInt(42))
		matcher := NewMatcher(map[string]struct{}{}, codes)

		err := matcher.MatchSyntaxWithLiterals(target, literalSyntax, literalMatcher)
		c.Assert(err, qt.Equals, ErrNotAMatch)
	})

	c.Run("non-literal symbol comparison", func(c *qt.C) {
		// Pattern: (foo 10) where "foo" is NOT a literal (empty literal set)
		// Normal symbol comparison should succeed
		fooSym := syntax.NewSyntaxSymbol("foo", nil)
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: fooSym},
			ByteCodeVisitCdr{},
			ByteCodeCompareCar{Value: testSyntaxInt(10)},
			ByteCodeDone{},
		}
		literalSyntax := map[string]*syntax.SyntaxSymbol{} // No literals
		literalMatcher := func(inputSym *syntax.SyntaxSymbol, patternLiteralKey string) bool {
			return true
		}
		target := testSyntaxList(syntax.NewSyntaxSymbol("foo", nil), testSyntaxInt(10))
		matcher := NewMatcher(map[string]struct{}{}, codes)

		err := matcher.MatchSyntaxWithLiterals(target, literalSyntax, literalMatcher)
		c.Assert(err, qt.IsNil)
	})
}

func TestMatchSyntax_EllipsisInMiddle(t *testing.T) {
	c := qt.New(t)

	c.Run("zero ellipsis repetitions", func(c *qt.C) {
		// Pattern: (x ... b c) with target (b c) - zero repetitions of x
		// x captures nothing, then b and c are matched literally.
		// Bytecode:
		//   0: SkipIfTailCount{Offset: 6, Count: 2}  -> skip to index 6 if 2 remain
		//   1: PushContext{EllipsisID: 0}
		//   2: CaptureCar{Binding: "x"}
		//   3: VisitCdr
		//   4: PopContext{EllipsisID: 0}
		//   5: Jump{Offset: -5}                       -> back to index 0
		//   6: CompareCar("b")
		//   7: VisitCdr
		//   8: CompareCar("c")
		//   9: Done
		codes := []SyntaxCommand{
			ByteCodeSkipIfTailCount{Offset: 6, Count: 2},  // 0
			ByteCodePushContext{EllipsisID: 0},            // 1
			ByteCodeCaptureCar{Binding: "x"},              // 2
			ByteCodeVisitCdr{},                            // 3
			ByteCodePopContext{EllipsisID: 0},             // 4
			ByteCodeJump{Offset: -5},                      // 5
			ByteCodeCompareCar{Value: testSyntaxSym("b")}, // 6
			ByteCodeVisitCdr{},                            // 7
			ByteCodeCompareCar{Value: testSyntaxSym("c")}, // 8
			ByteCodeDone{},                                // 9
		}
		target := testSyntaxList(testSyntaxSym("b"), testSyntaxSym("c"))
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)

		// x should have zero captures (no children for ellipsis 0)
		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 0)
	})

	c.Run("multiple ellipsis repetitions before tail", func(c *qt.C) {
		// Pattern: (x ... b c) with target (1 2 3 b c) - three repetitions of x
		codes := []SyntaxCommand{
			ByteCodeSkipIfTailCount{Offset: 6, Count: 2},  // 0
			ByteCodePushContext{EllipsisID: 0},            // 1
			ByteCodeCaptureCar{Binding: "x"},              // 2
			ByteCodeVisitCdr{},                            // 3
			ByteCodePopContext{EllipsisID: 0},             // 4
			ByteCodeJump{Offset: -5},                      // 5
			ByteCodeCompareCar{Value: testSyntaxSym("b")}, // 6
			ByteCodeVisitCdr{},                            // 7
			ByteCodeCompareCar{Value: testSyntaxSym("c")}, // 8
			ByteCodeDone{},                                // 9
		}
		target := testSyntaxList(
			testSyntaxInt(1), testSyntaxInt(2), testSyntaxInt(3),
			testSyntaxSym("b"), testSyntaxSym("c"),
		)
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)

		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 3)
		c.Assert(syntaxValuesEqualForMatch(children[0].bindings["x"], testSyntaxInt(1)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(children[1].bindings["x"], testSyntaxInt(2)), qt.IsTrue)
		c.Assert(syntaxValuesEqualForMatch(children[2].bindings["x"], testSyntaxInt(3)), qt.IsTrue)
	})

	c.Run("single element before tail", func(c *qt.C) {
		// Pattern: (x ... b) with target (1 b) - one repetition of x
		codes := []SyntaxCommand{
			ByteCodeSkipIfTailCount{Offset: 6, Count: 1},  // 0
			ByteCodePushContext{EllipsisID: 0},            // 1
			ByteCodeCaptureCar{Binding: "x"},              // 2
			ByteCodeVisitCdr{},                            // 3
			ByteCodePopContext{EllipsisID: 0},             // 4
			ByteCodeJump{Offset: -5},                      // 5
			ByteCodeCompareCar{Value: testSyntaxSym("b")}, // 6
			ByteCodeDone{},                                // 7
		}
		target := testSyntaxList(testSyntaxInt(1), testSyntaxSym("b"))
		variables := map[string]struct{}{"x": {}}
		matcher := NewMatcher(variables, codes)

		err := matcher.MatchSyntax(target)
		c.Assert(err, qt.IsNil)

		children := matcher.captureStack[0].children[0]
		c.Assert(len(children), qt.Equals, 1)
		c.Assert(syntaxValuesEqualForMatch(children[0].bindings["x"], testSyntaxInt(1)), qt.IsTrue)
	})
}
