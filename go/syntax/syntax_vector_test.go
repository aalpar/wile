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

package syntax

import (
	"testing"

	"github.com/aalpar/wile/go/values"

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
	c.Assert(vec.Values[0], qt.Equals, v1)
	c.Assert(vec.Values[1], qt.Equals, v2)
	c.Assert(vec.Values[2], qt.Equals, v3)
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
	c.Assert((*resultVec)[0], qt.Equals, v1)
	c.Assert((*resultVec)[1], qt.Equals, v2)
}

func (p *SyntaxVectorSuite) TestUnwrap_Nil(c *qt.C) {
	var vec *SyntaxVector
	result := vec.Unwrap()
	c.Assert(result, qt.Equals, values.Void)
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
	c.Assert((*resultVec)[0], values.SchemeEquals, values.NewInteger(1))
	c.Assert((*resultVec)[1], values.SchemeEquals, values.NewInteger(2))
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
	c.Assert(result, values.SchemeEquals, expected)
}

func (p *SyntaxVectorSuite) TestUnwrapAll_Nil(c *qt.C) {
	var vec *SyntaxVector
	result := vec.UnwrapAll()
	c.Assert(result, qt.Equals, values.Void)
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

func TestSyntaxVector(t *testing.T) {
	qtsuite.Run(qt.New(t), &SyntaxVectorSuite{})
}
