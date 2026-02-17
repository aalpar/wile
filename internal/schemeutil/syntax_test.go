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

package schemeutil

import (
	"context"
	"testing"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

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

func (p *SyntaxValueToDatumSuite) Init(_ *qt.C) {
	p.sctx = makeSourceContext()
}

func (p *SyntaxValueToDatumSuite) TestVoid(c *qt.C) {
	result := SyntaxValueToDatum(syntax.SyntaxVoid)
	c.Assert(result, qt.Equals, values.Void)
}

func (p *SyntaxValueToDatumSuite) TestEmptyList(c *qt.C) {
	emptyList := syntax.SyntaxEmptyList
	result := SyntaxValueToDatum(emptyList)
	c.Assert(values.IsEmptyList(result), qt.IsTrue)
}

func (p *SyntaxValueToDatumSuite) TestSymbol(c *qt.C) {
	sym := syntax.NewSyntaxSymbol("foo", p.sctx)
	result := SyntaxValueToDatum(sym)
	expected := values.NewSymbol("foo")
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxValueToDatumSuite) TestProperList(c *qt.C) {
	// Build (1 2 3)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), p.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), p.sctx)
	s3 := syntax.NewSyntaxObject(values.NewInteger(3), p.sctx)
	list := syntax.NewSyntaxCons(s1,
		syntax.NewSyntaxCons(s2,
			syntax.NewSyntaxCons(s3,
				syntax.SyntaxEmptyList, p.sctx), p.sctx), p.sctx)

	result := SyntaxValueToDatum(list)
	expected := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxValueToDatumSuite) TestImproperList(c *qt.C) {
	// Build (1 . 2)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), p.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), p.sctx)
	pair := syntax.NewSyntaxCons(s1, s2, p.sctx)

	result := SyntaxValueToDatum(pair)
	expected := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxValueToDatumSuite) TestImproperListLonger(c *qt.C) {
	// Build (1 2 . 3)
	s1 := syntax.NewSyntaxObject(values.NewInteger(1), p.sctx)
	s2 := syntax.NewSyntaxObject(values.NewInteger(2), p.sctx)
	s3 := syntax.NewSyntaxObject(values.NewInteger(3), p.sctx)
	list := syntax.NewSyntaxCons(s1,
		syntax.NewSyntaxCons(s2, s3, p.sctx), p.sctx)

	result := SyntaxValueToDatum(list)
	expected := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxValueToDatumSuite) TestVector(c *qt.C) {
	// Build #(1 2 3)
	vec := syntax.NewSyntaxVector(p.sctx)
	vec.Values = append(vec.Values,
		syntax.NewSyntaxObject(values.NewInteger(1), p.sctx),
		syntax.NewSyntaxObject(values.NewInteger(2), p.sctx),
		syntax.NewSyntaxObject(values.NewInteger(3), p.sctx))

	result := SyntaxValueToDatum(vec)
	expected := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxValueToDatumSuite) TestSyntaxObject(c *qt.C) {
	obj := syntax.NewSyntaxObject(values.NewInteger(42), p.sctx)
	result := SyntaxValueToDatum(obj)
	c.Assert(result, values.SchemeEquals, values.NewInteger(42))
}

func (p *SyntaxValueToDatumSuite) TestSyntaxObjectWithBox(c *qt.C) {
	inner := values.NewInteger(42)
	box := values.NewBox(inner)
	obj := syntax.NewSyntaxObject(box, p.sctx)

	result := SyntaxValueToDatum(obj)
	resultBox, ok := result.(*values.Box)
	c.Assert(ok, qt.IsTrue)
	c.Assert(resultBox.Unbox(), values.SchemeEquals, values.NewInteger(42))
}

func (p *SyntaxValueToDatumSuite) TestPlainValue(c *qt.C) {
	// A plain value that's not a syntax type should be returned as-is
	v := values.NewInteger(123)
	result := SyntaxValueToDatum(v)
	c.Assert(result, qt.Equals, v)
}

func (p *SyntaxValueToDatumSuite) TestNestedList(c *qt.C) {
	// Build ((1 2) (3 4))
	inner1 := syntax.NewSyntaxCons(
		syntax.NewSyntaxObject(values.NewInteger(1), p.sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(2), p.sctx),
			syntax.SyntaxEmptyList, p.sctx), p.sctx)
	inner2 := syntax.NewSyntaxCons(
		syntax.NewSyntaxObject(values.NewInteger(3), p.sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxObject(values.NewInteger(4), p.sctx),
			syntax.SyntaxEmptyList, p.sctx), p.sctx)
	outer := syntax.NewSyntaxCons(inner1,
		syntax.NewSyntaxCons(inner2,
			syntax.SyntaxEmptyList, p.sctx), p.sctx)

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

func (p *DatumToSyntaxValueSuite) Init(_ *qt.C) {
	p.sctx = makeSourceContext()
}

func (p *DatumToSyntaxValueSuite) TestVoid(c *qt.C) {
	result := DatumToSyntaxValue(context.Background(), p.sctx, values.Void)
	c.Assert(result, qt.Equals, syntax.SyntaxVoid)
}

func (p *DatumToSyntaxValueSuite) TestEmptyList(c *qt.C) {
	result := DatumToSyntaxValue(context.Background(), p.sctx, values.EmptyList)
	c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
}

func (p *DatumToSyntaxValueSuite) TestSymbol(c *qt.C) {
	sym := values.NewSymbol("foo")
	result := DatumToSyntaxValue(context.Background(), p.sctx, sym)
	synSym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(synSym.Sym.Key, qt.Equals, "foo")
}

func (p *DatumToSyntaxValueSuite) TestProperList(c *qt.C) {
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(context.Background(), p.sctx, list)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, list)
}

func (p *DatumToSyntaxValueSuite) TestImproperList(c *qt.C) {
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := DatumToSyntaxValue(context.Background(), p.sctx, pair)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, pair)
}

func (p *DatumToSyntaxValueSuite) TestImproperListLonger(c *qt.C) {
	// (1 2 . 3)
	pair := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	result := DatumToSyntaxValue(context.Background(), p.sctx, pair)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, pair)
}

func (p *DatumToSyntaxValueSuite) TestVector(c *qt.C) {
	vec := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(context.Background(), p.sctx, vec)

	synVec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(synVec.Values), qt.Equals, 3)

	// Convert back to datum and compare
	datum := SyntaxValueToDatum(result)
	c.Assert(datum, values.SchemeEquals, vec)
}

func (p *DatumToSyntaxValueSuite) TestBox(c *qt.C) {
	box := values.NewBox(values.NewInteger(42))
	result := DatumToSyntaxValue(context.Background(), p.sctx, box)

	synObj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	resultBox, ok := synObj.Datum().(*values.Box)
	c.Assert(ok, qt.IsTrue)
	// The box contents should be wrapped in syntax
	_, ok = resultBox.Unbox().(syntax.SyntaxValue)
	c.Assert(ok, qt.IsTrue)
}

func (p *DatumToSyntaxValueSuite) TestAlreadySyntax(c *qt.C) {
	original := syntax.NewSyntaxSymbol("foo", p.sctx)
	result := DatumToSyntaxValue(context.Background(), p.sctx, original)
	c.Assert(result, qt.Equals, original)
}

func (p *DatumToSyntaxValueSuite) TestInteger(c *qt.C) {
	num := values.NewInteger(42)
	result := DatumToSyntaxValue(context.Background(), p.sctx, num)

	synObj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	c.Assert(synObj.Datum(), values.SchemeEquals, num)
}

func (p *DatumToSyntaxValueSuite) TestNestedList(c *qt.C) {
	// ((1 2) (3 4))
	list := values.List(
		values.List(values.NewInteger(1), values.NewInteger(2)),
		values.List(values.NewInteger(3), values.NewInteger(4)))
	result := DatumToSyntaxValue(context.Background(), p.sctx, list)

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

func (p *RoundTripSuite) Init(_ *qt.C) {
	p.sctx = makeSourceContext()
}

func (p *RoundTripSuite) TestProperList(c *qt.C) {
	original := values.List(
		values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewInteger(42))

	syntaxVal := DatumToSyntaxValue(context.Background(), p.sctx, original)
	result := SyntaxValueToDatum(syntaxVal)
	c.Assert(result, values.SchemeEquals, original)
}

func (p *RoundTripSuite) TestComplexStructure(c *qt.C) {
	// (lambda (x y) (+ x y))
	original := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("y")),
		values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y")))

	syntaxVal := DatumToSyntaxValue(context.Background(), p.sctx, original)
	result := SyntaxValueToDatum(syntaxVal)
	c.Assert(result, values.SchemeEquals, original)
}

func TestRoundTrip(t *testing.T) {
	qtsuite.Run(qt.New(t), &RoundTripSuite{})
}
