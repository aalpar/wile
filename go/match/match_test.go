package match

import (
	"fmt"
	"log"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"

	"skeme/values"
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
