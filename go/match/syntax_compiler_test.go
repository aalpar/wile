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
	"context"
	"fmt"
	"testing"

	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

// testSyntaxIntC creates a syntax-wrapped integer for test bytecode in compiler tests.
func testSyntaxIntC(v int64) syntax.SyntaxValue {
	return syntax.NewSyntaxObject(values.NewInteger(v), nil)
}

// testSyntaxSymC creates a syntax-wrapped symbol for test bytecode in compiler tests.
func testSyntaxSymC(key string) syntax.SyntaxValue {
	return syntax.NewSyntaxSymbol(key, nil)
}

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
		in        *values.Pair
		out       []SyntaxCommand
	}{
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(values.NewInteger(10), values.NewSymbol("a")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeVisitCdr{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in:        values.List(values.List(values.NewInteger(10)), values.NewInteger(20)),
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxIntC(20)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(values.NewInteger(10), values.List(values.NewSymbol("a"), values.NewSymbol("b")), values.NewInteger(40)),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxIntC(40)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in:        values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxIntC(20)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxIntC(30)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("...")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxIntC(20)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("a")},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxSymC("...")},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("...")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: testSyntaxIntC(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxIntC(20)},
				ByteCodeVisitCdr{},
				// SkipIfEmpty checks for empty list before executing loop body
				ByteCodeSkipIfEmpty{Offset: 9},
				// mark
				ByteCodePushContext{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeDone{},
				ByteCodePopContext{},
				ByteCodeJump{Offset: -8},
				ByteCodeDone{},
			},
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %s", i, tc.in.SchemeString()), func(c *qt.C) {
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
		in        *values.Pair
		target    *values.Pair
		matches   bool
		result    values.Value
	}{
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in:      values.List(values.NewInteger(10), values.NewSymbol("a")),
			target:  values.List(values.NewInteger(10), values.NewInteger(20)),
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			in:        values.List(values.List(values.NewInteger(10)), values.NewInteger(20)),
			target:    values.List(values.List(values.NewInteger(10)), values.NewInteger(20)),
			matches:   true,
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(
				values.NewInteger(10), values.List(
					values.NewSymbol("a"), values.NewSymbol("b"),
				), values.NewInteger(40),
			),
			target: values.List(
				values.NewInteger(10), values.List(
					values.NewInteger(20), values.NewSymbol("b"),
				), values.NewInteger(40),
			),
			matches: true,
		},
		{
			variables: map[string]struct{}{},
			in:        values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			target:    values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			matches:   true,
		},
		{
			variables: map[string]struct{}{},
			in: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("...")),
			target: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("...")),
			matches: true,
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b"),
				), values.NewSymbol("..."),
			),
			target: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewInteger(30), values.NewSymbol("b"),
				),
				values.List(
					values.NewInteger(40), values.NewSymbol("b"),
				),
			),
			matches: true,
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %s", i, tc.in.SchemeString()), func(c *qt.C) {
			vst := NewSyntaxCompiler()
			vst.variables = tc.variables
			err := vst.Compile(context.TODO(), tc.in)
			c.Assert(err, qt.IsNil)
			mtc := NewMatcher(vst.variables, vst.codes)
			err = mtc.Match(tc.target)
			if tc.matches {
				c.Assert(err, qt.IsNil, qt.Commentf("expected match for %s", tc.in.SchemeString()))
			} else {
				c.Assert(err, qt.ErrorIs, ErrNotAMatch, qt.Commentf("expected no match for %s", tc.in.SchemeString()))
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: testSyntaxSymC("b")},
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
