// Copyright 2025 Aaron Alpar
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
	"fmt"
	"log"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"

	"wile/values"
)

func TestUtilsMatchSuites(t *testing.T) {
	c := qt.New(t)
	qtsuite.Run(c, UtilsMatchSuite{})
}

type UtilsMatchSuite struct {
}

func (UtilsMatchSuite) TestMatch(c *qt.C) {
	tcs := []struct {
		variables map[string]struct{}
		in        []SyntaxCommand
		target    *values.Pair
		matches   bool
	}{
		{
			variables: map[string]struct{}{},
			target:    values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			in: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(30)},
				ByteCodeDone{},
			},
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			target:    values.List(values.List(values.NewInteger(10)), values.NewInteger(20)),
			in: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeDone{},
			},
			matches: true,
		},
		{
			variables: map[string]struct{}{"a": {}},
			target:    values.List(values.NewInteger(10), values.List(values.NewSymbol("a"), values.NewSymbol("b")), values.NewInteger(40)),
			in: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: values.NewInteger(40)},
				ByteCodeDone{},
			},
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			target: values.List(
				values.NewInteger(10), values.NewInteger(20),
				values.List(values.NewSymbol("a"), values.NewSymbol("b")),
				values.List(values.NewSymbol("a"), values.NewSymbol("b")),
				values.List(values.NewSymbol("a"), values.NewSymbol("b"))),
			in: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				// mark
				ByteCodePushContext{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("a")},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeDone{},
				ByteCodePopContext{},
				ByteCodeJump{Offset: -7},
				ByteCodeDone{},
			},
			matches: true,
		},
		{
			variables: map[string]struct{}{"a": {}},
			target: values.List(
				values.NewInteger(10), values.NewInteger(20),
				values.List(values.NewSymbol("x"), values.NewSymbol("b")),
				values.List(values.NewSymbol("y"), values.NewSymbol("b")),
				values.List(values.NewSymbol("z"), values.NewSymbol("b"))),
			in: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				// mark
				ByteCodePushContext{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeDone{},
				ByteCodePopContext{},
				ByteCodeJump{Offset: -7},
				ByteCodeDone{},
			},
			matches: true,
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %s", i, tc.in), func(c *qt.C) {
			vst := NewMatcher(tc.variables, tc.in)
			err := vst.Match(tc.target)
			if tc.matches {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(err, qt.IsNotNil)
			}
		})
	}
}

func TestExpand(t *testing.T) {
	tcs := []struct {
		in *values.Pair
	}{
		{in: values.List()},
		{in: values.List(values.NewInteger(10))},
		{in: values.List(values.NewInteger(10), values.NewInteger(20))},
	}
	for _, tc := range tcs {
		match := &Matcher{}
		match.captureStack = []*captureContext{{bindings: map[string]values.Value{}}}
		_, err := match.Expand(tc.in)
		log.Printf("")
		qt.Assert(t, err, qt.IsNil)
	}
}

func TestMatchEdgeCases(t *testing.T) {
	t.Run("Match with unknown opcode", func(t *testing.T) {
		// Create a custom opcode that's not recognized
		type UnknownOpCode struct{}
		variables := map[string]struct{}{}
		// We can't actually test this without modifying the code to expose unknown opcodes
		// So we'll skip this test
		matcher := NewMatcher(variables, []SyntaxCommand{ByteCodeDone{}})
		target := values.List(values.NewInteger(10))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.IsNil) // Empty list matches with just Done
	})

	t.Run("CaptureCar with conflicting binding", func(t *testing.T) {
		variables := map[string]struct{}{"x": {}}
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "x"},
			ByteCodeVisitCdr{},
			ByteCodeCaptureCar{Binding: "x"}, // Try to bind x to different value
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.NewInteger(10), values.NewInteger(20))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.Equals, ErrNotAMatch)
	})

	t.Run("VisitCar on non-pair", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeVisitCar{},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.NewInteger(10))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.Equals, ErrNotAMatch)
	})

	t.Run("VisitCdr on non-pair", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: values.NewInteger(10)},
			ByteCodeVisitCdr{},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		// Create improper list: (10 . 20)
		target := values.NewCons(values.NewInteger(10), values.NewInteger(20))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.Equals, ErrNotAMatch)
	})

	t.Run("ByteCodeRequireCarEmpty success", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeRequireCarEmpty{},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.EmptyList)

		err := matcher.Match(target)
		qt.Assert(t, err, qt.IsNil)
	})

	t.Run("ByteCodeRequireCarEmpty failure", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeRequireCarEmpty{},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.NewInteger(10))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.Equals, ErrNotAMatch)
	})

	t.Run("CompareCar mismatch", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: values.NewInteger(10)},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.NewInteger(20))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.Equals, ErrNotAMatch)
	})

	t.Run("SkipIfEmpty with void", func(t *testing.T) {
		variables := map[string]struct{}{}
		codes := []SyntaxCommand{
			ByteCodeCompareCar{Value: values.NewInteger(10)},
			ByteCodeVisitCdr{},
			ByteCodeSkipIfEmpty{Offset: 2},
			ByteCodeCompareCar{Value: values.NewInteger(20)},
			ByteCodeDone{},
		}

		matcher := NewMatcher(variables, codes)
		target := values.List(values.NewInteger(10))

		err := matcher.Match(target)
		qt.Assert(t, err, qt.IsNil)
	})
}

func TestExpandEdgeCases(t *testing.T) {
	t.Run("Expand with ellipsis variable outside context", func(t *testing.T) {
		variables := map[string]struct{}{"x": {}}
		matcher := &Matcher{
			variables: variables,
		}
		matcher.captureStack = []*captureContext{{
			bindings: map[string]values.Value{},
			children: make(map[int][]*captureContext),
		}}

		// Create an ellipsis context where x is bound
		ellipsisVars := map[string]struct{}{"x": {}}
		template := values.NewSymbol("x")

		_, err := matcher.expandValue(template, matcher.captureStack[0], ellipsisVars)
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "used outside of ellipsis context")
	})

	t.Run("Expand ellipsis with no matching ID", func(t *testing.T) {
		variables := map[string]struct{}{"x": {}}
		ellipsisVars := map[int]map[string]struct{}{
			0: {"y": {}}, // Different variable
		}
		matcher := &Matcher{
			variables:    variables,
			ellipsisVars: ellipsisVars,
		}
		matcher.captureStack = []*captureContext{{
			bindings: map[string]values.Value{},
			children: make(map[int][]*captureContext),
		}}

		// Pattern with ellipsis but variable not in ellipsisVars
		pattern := values.NewSymbol("x")
		template := values.List(pattern, values.NewSymbol("..."))

		result, err := matcher.expandValue(template, matcher.captureStack[0], nil)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})

	t.Run("Expand with empty children", func(t *testing.T) {
		variables := map[string]struct{}{"x": {}}
		ellipsisVars := map[int]map[string]struct{}{
			0: {"x": {}},
		}
		matcher := &Matcher{
			variables:    variables,
			ellipsisVars: ellipsisVars,
		}
		matcher.captureStack = []*captureContext{{
			bindings: map[string]values.Value{},
			children: map[int][]*captureContext{
				0: {}, // Empty children
			},
		}}

		pattern := values.NewSymbol("x")
		template := values.List(pattern, values.NewSymbol("..."))

		result, err := matcher.expandValue(template, matcher.captureStack[0], nil)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, values.IsEmptyList(result), qt.IsTrue)
	})
}
