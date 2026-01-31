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

	"wile/values"

	qt "github.com/frankban/quicktest"
	"github.com/frankban/quicktest/qtsuite"
)

type SyntaxValueSuite struct {
	sctx *SourceContext
}

func (p *SyntaxValueSuite) Init(c *qt.C) {
	sidx0 := NewSourceIndexes(0, 1, 1)
	sidx1 := NewSourceIndexes(0, 2, 1)
	p.sctx = NewSourceContext("test", "file.scm", sidx0, sidx1)
}

func (p *SyntaxValueSuite) makeSyntaxObject(v values.Value) SyntaxValue {
	return NewSyntaxObject(v, p.sctx)
}

func (p *SyntaxValueSuite) makeSyntaxSymbol(v string) SyntaxValue {
	return NewSyntaxSymbol(v, p.sctx)
}

func (p *SyntaxValueSuite) makeSyntaxPair(v0, v1 SyntaxValue) SyntaxValue {
	return NewSyntaxCons(v0, v1, p.sctx)
}

func (p *SyntaxValueSuite) makeSyntaxEmptyList() SyntaxValue {
	return NewSyntaxEmptyList(p.sctx)
}

// Test EqualTo and SchemeEquals methods
// different source contexts should make syntax values unequal
func (p *SyntaxValueSuite) TestEqualTo_SameValue(c *qt.C) {
	// Use SyntaxSymbol for symbols (NewSyntaxObject doesn't allow symbols)
	sidx00 := NewSourceIndexes(1, 10, 0)
	sidx01 := NewSourceIndexes(1, 20, 0)
	sidx10 := NewSourceIndexes(0, 10, 0)
	sidx11 := NewSourceIndexes(0, 20, 0)
	stx0 := NewSyntaxSymbol("foob", NewSourceContext("", "foo", sidx00, sidx01))
	stx1 := NewSyntaxSymbol("foob", NewSourceContext("", "bar", sidx10, sidx11))

	c.Assert(stx0, qt.Not(qt.Equals), stx1)
	c.Assert(stx0, qt.Not(values.SchemeEquals), stx1)
}

// Test EqualTo and SchemeEquals methods for different values
func (p *SyntaxValueSuite) TestEqualTo_DifferentValue(c *qt.C) {
	// Use SyntaxSymbol for symbols
	stx0 := p.makeSyntaxSymbol("foob")
	stx1 := p.makeSyntaxSymbol("bars")

	c.Assert(stx0, qt.Not(qt.Equals), stx1)
	c.Assert(stx0, qt.Not(values.SchemeEquals), stx1)
}

// Test EqualTo and SchemeEquals methods for different source contexts
func (p *SyntaxValueSuite) TestEqualTo_DifferentSourceContext(c *qt.C) {
	stx0 := p.makeSyntaxSymbol("foob")

	// Create with different source context
	sidx10 := NewSourceIndexes(0, 10, 0)
	sidx11 := NewSourceIndexes(0, 20, 0)
	sctx1 := NewSourceContext("foob", "file.scm", sidx10, sidx11)
	stx1 := NewSyntaxSymbol("foob", sctx1)

	c.Assert(stx0, qt.Not(qt.Equals), stx1)
	c.Assert(stx0, qt.Not(values.SchemeEquals), stx1)
}

func (p *SyntaxValueSuite) TestUnwrap_Symbol(c *qt.C) {
	// Use SyntaxSymbol for symbols
	stx := p.makeSyntaxSymbol("foob")
	v := stx.Unwrap()
	c.Assert(v.(*values.Symbol).Key, qt.Equals, "foob")
}

func (p *SyntaxValueSuite) TestUnwrap_Integer(c *qt.C) {
	// SyntaxObject can wrap integers
	stx := p.makeSyntaxObject(values.NewInteger(42))
	v := stx.Unwrap()
	c.Assert(v, values.SchemeEquals, values.NewInteger(42))
}

func (p *SyntaxValueSuite) TestUnwrapAll_Symbol(c *qt.C) {
	// Use SyntaxSymbol for symbols
	stx := p.makeSyntaxSymbol("foob")
	v := stx.UnwrapAll()
	c.Assert(v.(*values.Symbol).Key, qt.Equals, "foob")
}

func (p *SyntaxValueSuite) TestUnwrapAll_Integer(c *qt.C) {
	// SyntaxObject can wrap integers
	stx := p.makeSyntaxObject(values.NewInteger(42))
	v := stx.UnwrapAll()
	c.Assert(v, values.SchemeEquals, values.NewInteger(42))
}

func (p *SyntaxValueSuite) TestUnwrapAll_Pair(c *qt.C) {
	// Use SyntaxCons for pairs, SyntaxSymbol for symbols
	sym00 := p.makeSyntaxSymbol("foob")
	sym01 := p.makeSyntaxSymbol("barb")
	pr0 := p.makeSyntaxPair(sym00, p.makeSyntaxPair(sym01, p.makeSyntaxEmptyList()))

	pr1 := values.List(values.NewSymbol("foob"), values.NewSymbol("barb"))
	v := pr0.UnwrapAll()
	c.Assert(v, values.SchemeEquals, pr1)
}

func TestSyntaxValue(t *testing.T) {
	qtsuite.Run(qt.New(t), &SyntaxValueSuite{})
}
