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

package syntax

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

type SyntaxVectorSuite struct {
	sctx *SourceContext
}

func (p *SyntaxVectorSuite) Init(c *qt.C) {
	sidx0 := NewSourceIndexes(0, 0, 0)
	sidx1 := NewSourceIndexes(0, 0, 0)
	p.sctx = NewSourceContext("test", "test.scm", sidx0, sidx1)
}

func (p *SyntaxVectorSuite) TestNewSyntaxVector_Empty(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	c.Assert(vec, qt.IsNotNil)
	c.Assert(len(vec.Values), qt.Equals, 0)
	c.Assert(vec.SourceContext(), qt.Equals, p.sctx)
}

func (p *SyntaxVectorSuite) TestNewSyntaxVector_WithValues(c *qt.C) {
	v1 := NewSyntaxObject(values.NewInteger(1), p.sctx)
	v2 := NewSyntaxObject(values.NewInteger(2), p.sctx)
	v3 := NewSyntaxObject(values.NewInteger(3), p.sctx)

	vec := NewSyntaxVector(p.sctx, v1, v2, v3)
	c.Assert(len(vec.Values), qt.Equals, 3)
	c.Assert(vec.Values[0], valuestest.SchemeEquals, v1)
	c.Assert(vec.Values[1], valuestest.SchemeEquals, v2)
	c.Assert(vec.Values[2], valuestest.SchemeEquals, v3)
}

func (p *SyntaxVectorSuite) TestSourceContext(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	c.Assert(vec.SourceContext(), qt.Equals, p.sctx)
}

func (p *SyntaxVectorSuite) TestIsVoid_NotNil(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	c.Assert(vec.IsVoid(), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestIsVoid_Nil(c *qt.C) {
	var vec *SyntaxVector
	c.Assert(vec.IsVoid(), qt.IsTrue)
}

func (p *SyntaxVectorSuite) TestUnwrap_Empty(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	result := vec.Unwrap()

	resultVec, ok := result.(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*resultVec), qt.Equals, 0)
}

func (p *SyntaxVectorSuite) TestUnwrap_WithValues(c *qt.C) {
	v1 := NewSyntaxObject(values.NewInteger(1), p.sctx)
	v2 := NewSyntaxObject(values.NewInteger(2), p.sctx)
	vec := NewSyntaxVector(p.sctx, v1, v2)

	result := vec.Unwrap()
	resultVec, ok := result.(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*resultVec), qt.Equals, 2)
	// Unwrap keeps syntax values inside
	c.Assert((*resultVec)[0], valuestest.SchemeEquals, v1)
	c.Assert((*resultVec)[1], valuestest.SchemeEquals, v2)
}

func (p *SyntaxVectorSuite) TestUnwrap_Nil(c *qt.C) {
	var vec *SyntaxVector
	result := vec.Unwrap()
	c.Assert(result, valuestest.SchemeEquals, values.Void)
}

func (p *SyntaxVectorSuite) TestUnwrapAll_Empty(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	result := vec.UnwrapAll()

	resultVec, ok := result.(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*resultVec), qt.Equals, 0)
}

func (p *SyntaxVectorSuite) TestUnwrapAll_WithValues(c *qt.C) {
	v1 := NewSyntaxObject(values.NewInteger(1), p.sctx)
	v2 := NewSyntaxObject(values.NewInteger(2), p.sctx)
	vec := NewSyntaxVector(p.sctx, v1, v2)

	result := vec.UnwrapAll()
	resultVec, ok := result.(*values.Vector)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(*resultVec), qt.Equals, 2)
	// UnwrapAll recursively unwraps to raw values
	c.Assert((*resultVec)[0], valuestest.SchemeEquals, values.NewInteger(1))
	c.Assert((*resultVec)[1], valuestest.SchemeEquals, values.NewInteger(2))
}

func (p *SyntaxVectorSuite) TestUnwrapAll_Nested(c *qt.C) {
	// Create nested syntax: #(#(1 2) #(3 4))
	inner1 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	inner2 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(3), p.sctx),
		NewSyntaxObject(values.NewInteger(4), p.sctx))
	outer := NewSyntaxVector(p.sctx, inner1, inner2)

	result := outer.UnwrapAll()
	expected := values.NewVector(
		values.NewVector(values.NewInteger(1), values.NewInteger(2)),
		values.NewVector(values.NewInteger(3), values.NewInteger(4)))
	c.Assert(result, valuestest.SchemeEquals, expected)
}

func (p *SyntaxVectorSuite) TestUnwrapAll_Nil(c *qt.C) {
	var vec *SyntaxVector
	result := vec.UnwrapAll()
	c.Assert(result, valuestest.SchemeEquals, values.Void)
}

func (p *SyntaxVectorSuite) TestSchemeString_Empty(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)
	c.Assert(vec.SchemeString(), qt.Equals, "#'()")
}

func (p *SyntaxVectorSuite) TestSchemeString_WithValues(c *qt.C) {
	vec := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx),
		NewSyntaxObject(values.NewInteger(3), p.sctx))
	c.Assert(vec.SchemeString(), qt.Equals, "#'(#'1 #'2 #'3)")
}

func (p *SyntaxVectorSuite) TestSchemeString_Nil(c *qt.C) {
	var vec *SyntaxVector
	c.Assert(vec.SchemeString(), qt.Equals, "#'<void>")
}

func (p *SyntaxVectorSuite) TestEqualTo_Same(c *qt.C) {
	vec := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	c.Assert(vec.EqualTo(vec), qt.IsTrue)
}

func (p *SyntaxVectorSuite) TestEqualTo_Equal(c *qt.C) {
	vec1 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	vec2 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	c.Assert(vec1.EqualTo(vec2), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestEqualTo_DifferentLength(c *qt.C) {
	vec1 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	vec2 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx))
	c.Assert(vec1.EqualTo(vec2), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestEqualTo_DifferentValues(c *qt.C) {
	vec1 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(2), p.sctx))
	vec2 := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx),
		NewSyntaxObject(values.NewInteger(3), p.sctx))
	c.Assert(vec1.EqualTo(vec2), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestEqualTo_NotSyntaxVector(c *qt.C) {
	vec := NewSyntaxVector(p.sctx,
		NewSyntaxObject(values.NewInteger(1), p.sctx))
	c.Assert(vec.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestEqualTo_Empty(c *qt.C) {
	vec1 := NewSyntaxVector(p.sctx)
	vec2 := NewSyntaxVector(p.sctx)
	c.Assert(vec1.EqualTo(vec2), qt.IsFalse)
}

func (p *SyntaxVectorSuite) TestSyntaxForEach_IteratesElements(c *qt.C) {
	v1 := NewSyntaxObject(values.NewInteger(1), p.sctx)
	v2 := NewSyntaxObject(values.NewInteger(2), p.sctx)
	v3 := NewSyntaxObject(values.NewInteger(3), p.sctx)
	vec := NewSyntaxVector(p.sctx, v1, v2, v3)

	var seen []SyntaxValue
	var idxs []int
	var lastFlags []bool

	tail, err := vec.SyntaxForEach(context.Background(), func(_ context.Context, i int, hasNext bool, v SyntaxValue) error {
		seen = append(seen, v)
		idxs = append(idxs, i)
		lastFlags = append(lastFlags, hasNext)
		return nil
	})

	c.Assert(err, qt.IsNil)
	c.Assert(IsSyntaxEmptyList(tail), qt.IsTrue)
	c.Assert(len(seen), qt.Equals, 3)
	c.Assert(seen[0], valuestest.SchemeEquals, v1)
	c.Assert(seen[1], valuestest.SchemeEquals, v2)
	c.Assert(seen[2], valuestest.SchemeEquals, v3)
	c.Assert(idxs, qt.DeepEquals, []int{0, 1, 2})
	c.Assert(lastFlags, qt.DeepEquals, []bool{true, true, false})
}

func (p *SyntaxVectorSuite) TestSyntaxForEach_EmptyVector(c *qt.C) {
	vec := NewSyntaxVector(p.sctx)

	called := false
	tail, err := vec.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, _ SyntaxValue) error {
		called = true
		return nil
	})

	c.Assert(err, qt.IsNil)
	c.Assert(called, qt.IsFalse)
	c.Assert(IsSyntaxEmptyList(tail), qt.IsTrue)
}

func (p *SyntaxVectorSuite) TestSyntaxForEach_NilVector(c *qt.C) {
	var vec *SyntaxVector

	called := false
	tail, err := vec.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, _ SyntaxValue) error {
		called = true
		return nil
	})

	c.Assert(err, qt.IsNil)
	c.Assert(called, qt.IsFalse)
	c.Assert(tail, qt.Equals, SyntaxVoid)
}

func (p *SyntaxVectorSuite) TestSyntaxForEach_ErrorStopsIteration(c *qt.C) {
	v1 := NewSyntaxObject(values.NewInteger(1), p.sctx)
	v2 := NewSyntaxObject(values.NewInteger(2), p.sctx)
	v3 := NewSyntaxObject(values.NewInteger(3), p.sctx)
	vec := NewSyntaxVector(p.sctx, v1, v2, v3)

	var seen []SyntaxValue
	sentinel := errors.New("stop")

	tail, err := vec.SyntaxForEach(context.Background(), func(_ context.Context, i int, _ bool, v SyntaxValue) error {
		seen = append(seen, v)
		if i == 1 {
			return sentinel
		}
		return nil
	})

	c.Assert(err, qt.Equals, sentinel)
	c.Assert(tail, qt.IsNil)
	c.Assert(len(seen), qt.Equals, 2)
	c.Assert(seen[0], valuestest.SchemeEquals, v1)
	c.Assert(seen[1], valuestest.SchemeEquals, v2)
}

// TestAddScope_EmptyVector verifies empty vectors return unchanged
func (p *SyntaxVectorSuite) TestAddScope_EmptyVector(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})
	emptyVec := NewSyntaxVector(sc)
	scope := NewScope()

	result := emptyVec.AddScope(scope)

	c.Assert(result, qt.Equals, emptyVec)
}

// TestAddScope_NilVector verifies nil vector returns as-is
func (p *SyntaxVectorSuite) TestAddScope_NilVector(c *qt.C) {
	var nilVec *SyntaxVector
	scope := NewScope()

	result := nilVec.AddScope(scope)

	c.Assert(result, qt.IsNil)
}

// TestAddScope_VectorWithSymbols verifies symbols receive scopes
func (p *SyntaxVectorSuite) TestAddScope_VectorWithSymbols(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym1 := NewSyntaxSymbol("x", sc)
	sym2 := NewSyntaxSymbol("y", sc)
	vec := NewSyntaxVector(sc, sym1, sym2)
	scope := NewScope()

	result := vec.AddScope(scope)

	// Result should be a new vector
	c.Assert(result, qt.Not(qt.Equals), vec)
	resultVec := result.(*SyntaxVector)
	c.Assert(len(resultVec.Values), qt.Equals, 2)

	// Each symbol should have the scope
	resultSym1 := resultVec.Values[0].(*SyntaxSymbol)
	resultSym2 := resultVec.Values[1].(*SyntaxSymbol)

	c.Assert(HasScope(resultSym1.Scopes(), scope), qt.IsTrue)
	c.Assert(HasScope(resultSym2.Scopes(), scope), qt.IsTrue)
}

// TestAddScope_VectorWithObjects verifies SyntaxObject elements unchanged
func (p *SyntaxVectorSuite) TestAddScope_VectorWithObjects(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	num := NewSyntaxObject(values.NewInteger(42), sc)
	str := NewSyntaxObject(values.NewString("hello"), sc)
	vec := NewSyntaxVector(sc, num, str)
	scope := NewScope()

	result := vec.AddScope(scope)

	// Result should be same vector (no changes)
	c.Assert(result, qt.Equals, vec)
}

// TestAddScope_VectorWithPair verifies pairs propagate scopes to nested symbols
func (p *SyntaxVectorSuite) TestAddScope_VectorWithPair(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym1 := NewSyntaxSymbol("a", sc)
	sym2 := NewSyntaxSymbol("b", sc)
	pair := NewSyntaxCons(sym1, sym2, sc)
	vec := NewSyntaxVector(sc, pair)
	scope := NewScope()

	result := vec.AddScope(scope)

	// Result should be a new vector
	c.Assert(result, qt.Not(qt.Equals), vec)
	resultVec := result.(*SyntaxVector)

	// Pair should have scope propagated to its elements
	resultPair := resultVec.Values[0].(*SyntaxPair)
	resultSym1 := resultPair.SyntaxCar().(*SyntaxSymbol)
	resultSym2 := resultPair.SyntaxCdr().(*SyntaxSymbol)

	c.Assert(HasScope(resultSym1.Scopes(), scope), qt.IsTrue)
	c.Assert(HasScope(resultSym2.Scopes(), scope), qt.IsTrue)
}

// TestAddScope_NestedVectors verifies deep nesting propagates scopes correctly
func (p *SyntaxVectorSuite) TestAddScope_NestedVectors(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym := NewSyntaxSymbol("x", sc)
	innerVec := NewSyntaxVector(sc, sym)
	outerVec := NewSyntaxVector(sc, innerVec)
	scope := NewScope()

	result := outerVec.AddScope(scope)

	// Navigate to deeply nested symbol
	resultOuter := result.(*SyntaxVector)
	resultInner := resultOuter.Values[0].(*SyntaxVector)
	resultSym := resultInner.Values[0].(*SyntaxSymbol)

	c.Assert(HasScope(resultSym.Scopes(), scope), qt.IsTrue)
}

// TestAddScope_MixedElements verifies mixed types handled correctly
func (p *SyntaxVectorSuite) TestAddScope_MixedElements(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym := NewSyntaxSymbol("x", sc)
	num := NewSyntaxObject(values.NewInteger(42), sc)
	vec := NewSyntaxVector(sc, sym, num)
	scope := NewScope()

	result := vec.AddScope(scope)

	// Result should be a new vector (symbol changed)
	c.Assert(result, qt.Not(qt.Equals), vec)
	resultVec := result.(*SyntaxVector)

	// Symbol should have scope
	resultSym := resultVec.Values[0].(*SyntaxSymbol)
	c.Assert(HasScope(resultSym.Scopes(), scope), qt.IsTrue)

	// Number should be unchanged
	c.Assert(resultVec.Values[1], qt.Equals, num)
}

// TestAddScope_VectorWithNilElements verifies nil elements preserved
func (p *SyntaxVectorSuite) TestAddScope_VectorWithNilElements(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym := NewSyntaxSymbol("x", sc)
	vec := NewSyntaxVector(sc, sym, nil, sym)
	scope := NewScope()

	result := vec.AddScope(scope)

	resultVec := result.(*SyntaxVector)
	c.Assert(len(resultVec.Values), qt.Equals, 3)
	c.Assert(resultVec.Values[1], qt.IsNil)

	// Symbols should have scope
	resultSym0 := resultVec.Values[0].(*SyntaxSymbol)
	resultSym2 := resultVec.Values[2].(*SyntaxSymbol)
	c.Assert(HasScope(resultSym0.Scopes(), scope), qt.IsTrue)
	c.Assert(HasScope(resultSym2.Scopes(), scope), qt.IsTrue)
}

// TestAddScope_MultipleScopes verifies accumulation of multiple scopes
func (p *SyntaxVectorSuite) TestAddScope_MultipleScopes(c *qt.C) {
	sc := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{})

	sym := NewSyntaxSymbol("x", sc)
	vec := NewSyntaxVector(sc, sym)
	scope1 := NewScope()
	scope2 := NewScope()

	result1 := vec.AddScope(scope1).(*SyntaxVector)
	result2 := result1.AddScope(scope2)

	resultVec := result2.(*SyntaxVector)
	resultSym := resultVec.Values[0].(*SyntaxSymbol)

	// Symbol should have both scopes
	c.Assert(HasScope(resultSym.Scopes(), scope1), qt.IsTrue)
	c.Assert(HasScope(resultSym.Scopes(), scope2), qt.IsTrue)
}

func TestSyntaxVector(t *testing.T) {
	qtsuite.Run(qt.New(t), &SyntaxVectorSuite{})
}
