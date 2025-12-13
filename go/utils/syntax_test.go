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

package utils

import (
	"wile/syntax"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

func makeSourceContext() *syntax.SourceContext {
	sidx0 := syntax.NewSourceIndexes(0, 0, 0)
	sidx1 := syntax.NewSourceIndexes(0, 0, 0)
	return syntax.NewSourceContext("test", "test.scm", sidx0, sidx1)
}

// SyntaxValueToDatumSuite tests the SyntaxValueToDatum function
type SyntaxValueToDatumSuite struct {
	sctx *syntax.SourceContext
}

func (s *SyntaxValueToDatumSuite) Init(c *qt.C) {
	s.sctx = makeSourceContext()
}

func (s *SyntaxValueToDatumSuite) TestVoid(c *qt.C) {
	result := SyntaxValueToDatum(syntax.SyntaxVoid)
	c.Assert(result, qt.Equals, values.Void)
}

func (s *SyntaxValueToDatumSuite) TestEmptyList(c *qt.C) {
	emptyList := syntax.NewSyntaxEmptyList(s.sctx)
	result := SyntaxValueToDatum(emptyList)
	c.Assert(values.IsEmptyList(result), qt.IsTrue)
}

func (s *SyntaxValueToDatumSuite) TestSymbol(c *qt.C) {
	sym := syntax.NewSyntaxSymbol("foo", s.sctx)
	result := SyntaxValueToDatum(sym)
	expected := values.NewSymbol("foo")
	c.Assert(result, values.SchemeEquals, expected)
}

func (s *SyntaxValueToDatumSuite) TestProperList(c *qt.C) {
	// Build (1 2 3)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), s.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), s.sctx)
	s3 := syntax.NewSyntaxObject(values.NewInteger(3), s.sctx)
	list := syntax.NewSyntaxCons(s1,
		syntax.NewSyntaxCons(s2,
			syntax.NewSyntaxCons(s3,
				syntax.NewSyntaxEmptyList(s.sctx), s.sctx), s.sctx), s.sctx)

	result := SyntaxValueToDatum(list)
	expected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	c.Assert(result, values.SchemeEquals, expected)
}

func (s *SyntaxValueToDatumSuite) TestImproperList(c *qt.C) {
	// Build (1 . 2)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), s.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), s.sctx)
	pair := syntax.NewSyntaxCons(s1, s2, s.sctx)

	result := SyntaxValueToDatum(pair)
	expected := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	c.Assert(result, values.SchemeEquals, expected)
}

func (s *SyntaxValueToDatumSuite) TestImproperListLonger(c *qt.C) {
	// Build (1 2 . 3)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), s.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), s.sctx)
	s3 := syntax.NewSyntaxObject(values.NewInteger(3), s.sctx)
	list := syntax.NewSyntaxCons(s1,
		syntax.NewSyntaxCons(s2, s3, s.sctx), s.sctx)

	result := SyntaxValueToDatum(list)
	expected := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	c.Assert(result, values.SchemeEquals, expected)
}

func (s *SyntaxValueToDatumSuite) TestVector(c *qt.C) {
	// Build #(1 2 3)
	vec := syntax.NewSyntaxVector(s.sctx)
	vec.Values = append(vec.Values,
		syntax.NewSyntaxObject(values.NewInteger(1), s.sctx),
		syntax.NewSyntaxObject(values.NewInteger(2), s.sctx),
		syntax.NewSyntaxObject(values.NewInteger(3), s.sctx))

	result := SyntaxValueToDatum(vec)
	expected := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	c.Assert(result, values.SchemeEquals, expected)
}

func (s *SyntaxValueToDatumSuite) TestSyntaxObject(c *qt.C) {
	obj := syntax.NewSyntaxObject(values.NewInteger(42), s.sctx)
	result := SyntaxValueToDatum(obj)
	c.Assert(result, values.SchemeEquals, values.NewInteger(42))
}

func (s *SyntaxValueToDatumSuite) TestSyntaxObjectWithBox(c *qt.C) {
	inner := values.NewInteger(42)
	box := values.NewBox(inner)
	obj := syntax.NewSyntaxObject(box, s.sctx)

	result := SyntaxValueToDatum(obj)
	resultBox, ok := result.(*values.Box)
	c.Assert(ok, qt.IsTrue)
	c.Assert(resultBox.Unbox(), values.SchemeEquals, values.NewInteger(42))
}

func (s *SyntaxValueToDatumSuite) TestPlainValue(c *qt.C) {
	// A plain value that's not a syntax type should be returned as-is
	v := values.NewInteger(123)
	result := SyntaxValueToDatum(v)
	c.Assert(result, qt.Equals, v)
}

func (s *SyntaxValueToDatumSuite) TestNestedList(c *qt.C) {
	// Build ((1 2) (3 4))
	inner1 := syntax.NewSyntaxCons(
		syntax.NewSyntaxObject(values.NewInteger(1), s.sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(2), s.sctx),
			syntax.NewSyntaxEmptyList(s.sctx), s.sctx), s.sctx)
	inner2 := syntax.NewSyntaxCons(
		syntax.NewSyntaxObject(values.NewInteger(3), s.sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(4), s.sctx),
			syntax.NewSyntaxEmptyList(s.sctx), s.sctx), s.sctx)
	outer := syntax.NewSyntaxCons(inner1,
		syntax.NewSyntaxCons(inner2,
			syntax.NewSyntaxEmptyList(s.sctx), s.sctx), s.sctx)

	result := SyntaxValueToDatum(outer)
	expected := values.List(
		values.List(values.NewInteger(1), values.NewInteger(2)),
		values.List(values.NewInteger(3), values.NewInteger(4)))
	c.Assert(result, values.SchemeEquals, expected)
}

func TestSyntaxValueToDatum(t *testing.T) {
	qtsuite.Run(qt.New(t), &SyntaxValueToDatumSuite{})
}

// DatumToSyntaxValueSuite tests the DatumToSyntaxValue function
type DatumToSyntaxValueSuite struct {
	sctx *syntax.SourceContext
}

func (s *DatumToSyntaxValueSuite) Init(c *qt.C) {
	s.sctx = makeSourceContext()
}

func (s *DatumToSyntaxValueSuite) TestVoid(c *qt.C) {
	result := DatumToSyntaxValue(s.sctx, values.Void)
	c.Assert(result, qt.Equals, syntax.SyntaxVoid)
}

func (s *DatumToSyntaxValueSuite) TestEmptyList(c *qt.C) {
	result := DatumToSyntaxValue(s.sctx, values.EmptyList)
	c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
}

func (s *DatumToSyntaxValueSuite) TestSymbol(c *qt.C) {
	sym := values.NewSymbol("foo")
	result := DatumToSyntaxValue(s.sctx, sym)
	synSym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(synSym.Key, qt.Equals, "foo")
}

func (s *DatumToSyntaxValueSuite) TestProperList(c *qt.C) {
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(s.sctx, list)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, list)
}

func (s *DatumToSyntaxValueSuite) TestImproperList(c *qt.C) {
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := DatumToSyntaxValue(s.sctx, pair)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, pair)
}

func (s *DatumToSyntaxValueSuite) TestImproperListLonger(c *qt.C) {
	// (1 2 . 3)
	pair := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	result := DatumToSyntaxValue(s.sctx, pair)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, pair)
}

func (s *DatumToSyntaxValueSuite) TestVector(c *qt.C) {
	vec := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(s.sctx, vec)

	synVec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(synVec.Values), qt.Equals, 3)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, vec)
}

func (s *DatumToSyntaxValueSuite) TestBox(c *qt.C) {
	box := values.NewBox(values.NewInteger(42))
	result := DatumToSyntaxValue(s.sctx, box)

	synObj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	resultBox, ok := synObj.Datum.(*values.Box)
	c.Assert(ok, qt.IsTrue)
	// The box contents should be wrapped in syntax
	_, ok = resultBox.Unbox().(syntax.SyntaxValue)
	c.Assert(ok, qt.IsTrue)
}

func (s *DatumToSyntaxValueSuite) TestAlreadySyntax(c *qt.C) {
	original := syntax.NewSyntaxSymbol("foo", s.sctx)
	result := DatumToSyntaxValue(s.sctx, original)
	c.Assert(result, qt.Equals, original)
}

func (s *DatumToSyntaxValueSuite) TestInteger(c *qt.C) {
	num := values.NewInteger(42)
	result := DatumToSyntaxValue(s.sctx, num)

	synObj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	c.Assert(synObj.Datum, values.SchemeEquals, num)
}

func (s *DatumToSyntaxValueSuite) TestNestedList(c *qt.C) {
	// ((1 2) (3 4))
	list := values.List(
		values.List(values.NewInteger(1), values.NewInteger(2)),
		values.List(values.NewInteger(3), values.NewInteger(4)))
	result := DatumToSyntaxValue(s.sctx, list)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, list)
}

func TestDatumToSyntaxValue(t *testing.T) {
	qtsuite.Run(qt.New(t), &DatumToSyntaxValueSuite{})
}

// RoundTripSuite tests round-trip conversion between datum and syntax
type RoundTripSuite struct {
	sctx *syntax.SourceContext
}

func (s *RoundTripSuite) Init(c *qt.C) {
	s.sctx = makeSourceContext()
}

func (s *RoundTripSuite) TestProperList(c *qt.C) {
	original := values.List(
		values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewInteger(42))

	syntaxVal := DatumToSyntaxValue(s.sctx, original)
	result := SyntaxValueToDatum(syntaxVal)
	c.Assert(result, values.SchemeEquals, original)
}

func (s *RoundTripSuite) TestComplexStructure(c *qt.C) {
	// (lambda (x y) (+ x y))
	original := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("y")),
		values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y")))

	syntaxVal := DatumToSyntaxValue(s.sctx, original)
	result := SyntaxValueToDatum(syntaxVal)
	c.Assert(result, values.SchemeEquals, original)
}

func TestRoundTrip(t *testing.T) {
	qtsuite.Run(qt.New(t), &RoundTripSuite{})
}
