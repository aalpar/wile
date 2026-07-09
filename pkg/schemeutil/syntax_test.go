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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

func makeSourceContext() *syntax.SourceContext {
	sidx0 := syntax.NewSourceIndexes(0, 0, 0)
	sidx1 := syntax.NewSourceIndexes(0, 0, 0)
	return syntax.NewSourceContext("test", "test.scm", sidx0, sidx1)
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
	c.Assert(synSym.Key(), qt.Equals, "foo")
}

func (p *DatumToSyntaxValueSuite) TestProperList(c *qt.C) {
	list := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(context.Background(), p.sctx, list)

	// Convert back to datum and compare
	datum := result.UnwrapAll()
	c.Assert(datum, valuestest.SchemeEquals, list)
}

func (p *DatumToSyntaxValueSuite) TestImproperList(c *qt.C) {
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := DatumToSyntaxValue(context.Background(), p.sctx, pair)

	// Convert back to datum and compare
	datum := result.UnwrapAll()
	c.Assert(datum, valuestest.SchemeEquals, pair)
}

func (p *DatumToSyntaxValueSuite) TestImproperListLonger(c *qt.C) {
	// (1 2 . 3)
	pair := values.NewCons(values.NewInteger(1),
		values.NewCons(values.NewInteger(2), values.NewInteger(3)))
	result := DatumToSyntaxValue(context.Background(), p.sctx, pair)

	// Convert back to datum and compare
	datum := result.UnwrapAll()
	c.Assert(datum, valuestest.SchemeEquals, pair)
}

func (p *DatumToSyntaxValueSuite) TestVector(c *qt.C) {
	vec := values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	result := DatumToSyntaxValue(context.Background(), p.sctx, vec)

	synVec, ok := result.(*syntax.SyntaxVector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(synVec.Values), qt.Equals, 3)

	// Convert back to datum and compare
	datum := result.UnwrapAll()
	c.Assert(datum, valuestest.SchemeEquals, vec)
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

func (p *DatumToSyntaxValueSuite) TestAlreadySyntaxPair(c *qt.C) {
	// A *SyntaxPair satisfies values.Tuple, so a naive type switch matches the
	// Tuple arm before the SyntaxValue arm and rebuilds the pair with the
	// *caller's* sctx, silently discarding the pair's own source location and
	// scope set. An already-wrapped syntax pair must pass through unchanged
	// (identity), exactly like the SyntaxSymbol case above.
	origCtx := syntax.NewSourceContext("orig", "orig.scm",
		syntax.NewSourceIndexes(1, 2, 3), syntax.NewSourceIndexes(1, 4, 5))
	original := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("a", origCtx),
		syntax.SyntaxEmptyList,
		origCtx)

	result := DatumToSyntaxValue(context.Background(), p.sctx, original)

	// Same pointer: passed through, not rebuilt.
	c.Assert(result, qt.Equals, original)
	// Its own source context survives rather than being overwritten by p.sctx.
	c.Assert(result.SourceContext(), qt.Equals, origCtx)
}

func (p *DatumToSyntaxValueSuite) TestAlreadySyntaxVector(c *qt.C) {
	// The switch's `case *values.Vector` sits BELOW the SyntaxValue arm; a
	// *SyntaxVector is not a *values.Vector, so it can only be matched by the
	// SyntaxValue arm and must pass through unchanged. This pins the second half
	// of the pass-through contract the comment in syntax.go promises (the first
	// half — *SyntaxPair — is TestAlreadySyntaxPair). Guards a reorder that would
	// route already-wrapped syntax vectors through a rebuild and drop their sctx.
	origCtx := syntax.NewSourceContext("orig", "orig.scm",
		syntax.NewSourceIndexes(1, 2, 3), syntax.NewSourceIndexes(1, 4, 5))
	original := syntax.NewSyntaxVector(origCtx)
	original.Values = append(original.Values, syntax.NewSyntaxSymbol("a", origCtx))

	result := DatumToSyntaxValue(context.Background(), p.sctx, original)

	// Same pointer: passed through, not rebuilt.
	c.Assert(result, qt.Equals, original)
	// Its own source context survives rather than being overwritten by p.sctx.
	c.Assert(result.SourceContext(), qt.Equals, origCtx)
}

func (p *DatumToSyntaxValueSuite) TestInteger(c *qt.C) {
	num := values.NewInteger(42)
	result := DatumToSyntaxValue(context.Background(), p.sctx, num)

	synObj, ok := result.(*syntax.SyntaxObject)
	c.Assert(ok, qt.IsTrue)
	c.Assert(synObj.Datum(), valuestest.SchemeEquals, num)
}

func (p *DatumToSyntaxValueSuite) TestNestedList(c *qt.C) {
	// ((1 2) (3 4))
	list := values.List(
		values.List(values.NewInteger(1), values.NewInteger(2)),
		values.List(values.NewInteger(3), values.NewInteger(4)))
	result := DatumToSyntaxValue(context.Background(), p.sctx, list)

	// Convert back to datum and compare
	datum := result.UnwrapAll()
	c.Assert(datum, valuestest.SchemeEquals, list)
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
	result := syntaxVal.UnwrapAll()
	c.Assert(result, valuestest.SchemeEquals, original)
}

func (p *RoundTripSuite) TestComplexStructure(c *qt.C) {
	// (lambda (x y) (+ x y))
	original := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("y")),
		values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y")))

	syntaxVal := DatumToSyntaxValue(context.Background(), p.sctx, original)
	result := syntaxVal.UnwrapAll()
	c.Assert(result, valuestest.SchemeEquals, original)
}

func TestRoundTrip(t *testing.T) {
	qtsuite.Run(qt.New(t), &RoundTripSuite{})
}
