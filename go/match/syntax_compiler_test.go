package match

import (
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"

	"wile/values"
)

func TestUtilsMatcherSuites(t *testing.T) {
	c := qt.New(t)
	qtsuite.Run(c, UtilsMatcherSuite{})
}

type UtilsMatcherSuite struct {
}

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
				ByteCodeCompareCar{Value: values.NewInteger(10)},
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
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{
				"a": {},
			},
			in: values.List(values.NewInteger(10), values.List(values.NewSymbol("a"), values.NewSymbol("b")), values.NewInteger(40)),
			out: []SyntaxCommand{
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
		},
		{
			variables: map[string]struct{}{},
			in:        values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(30)},
				ByteCodeDone{},
			},
		},
		{
			variables: map[string]struct{}{},
			in: values.List(
				values.NewInteger(10), values.NewInteger(20), values.List(
					values.NewSymbol("a"), values.NewSymbol("b")), values.NewSymbol("...")),
			out: []SyntaxCommand{
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("a")},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: values.NewSymbol("...")},
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
				ByteCodeCompareCar{Value: values.NewInteger(10)},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewInteger(20)},
				ByteCodeVisitCdr{},
				// SkipIfEmpty checks for empty list before executing loop body
				ByteCodeSkipIfEmpty{Offset: 9},
				// mark
				ByteCodePushContext{},
				ByteCodeVisitCar{},
				ByteCodeCaptureCar{Binding: "a"},
				ByteCodeVisitCdr{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
			vst.Compile(tc.in)
			c.Assert(vst.codes, qt.DeepEquals, tc.out)
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
			err := vst.Compile(tc.in)
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeDone{},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +3},
				ByteCodeVisitCar{},
				ByteCodeDone{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
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
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
			},
			out: []SyntaxCommand{
				ByteCodeVisitCar{},
				ByteCodeJump{Offset: +2},
				ByteCodeVisitCar{},
				ByteCodeCompareCar{Value: values.NewSymbol("b")},
				ByteCodeJump{Offset: -2},
				ByteCodeVisitCar{},
				ByteCodeDone{},
			},
		},
	}
	for i, tc := range tcs {
		c.Run(fmt.Sprintf("%d: %q", i, tc.in), func(c *qt.C) {
			q := insert(tc.i, tc.in, []SyntaxCommand{ByteCodeDone{}})
			c.Assert(q, qt.DeepEquals, tc.out)
		})
	}

}
