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
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// SyntaxVoid methods

func TestSyntaxVoid_SchemeString(t *testing.T) {
	qt.Assert(t, SyntaxVoid.SchemeString(), qt.Equals, values.SpecialVoid)
}

func TestSyntaxVoid_IsVoid(t *testing.T) {
	qt.Assert(t, SyntaxVoid.IsVoid(), qt.IsTrue)
}

func TestSyntaxVoid_EqualTo(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxVoid.EqualTo(values.Void), qt.IsTrue)
	c.Assert(SyntaxVoid.EqualTo(values.TrueValue), qt.IsFalse)
	c.Assert(SyntaxVoid.EqualTo(nil), qt.IsFalse)
}

func TestSyntaxVoid_SourceContext(t *testing.T) {
	qt.Assert(t, SyntaxVoid.SourceContext(), qt.IsNil)
}

func TestSyntaxVoid_Unwrap(t *testing.T) {
	qt.Assert(t, SyntaxVoid.Unwrap(), values.SchemeEquals, values.Void)
}

func TestSyntaxVoid_UnwrapAll(t *testing.T) {
	qt.Assert(t, SyntaxVoid.UnwrapAll(), values.SchemeEquals, values.Void)
}

// SourceIndexes.Tab

func TestSourceIndexes_Tab(t *testing.T) {
	c := qt.New(t)

	t.Run("from column 0", func(t *testing.T) {
		sidx := NewSourceIndexes(10, 0, 1)
		result := sidx.Tab()
		c.Assert(result, qt.Equals, 11)
		c.Assert(sidx.Column(), qt.Equals, 8)
		c.Assert(sidx.Line(), qt.Equals, 1)
	})

	t.Run("from column 3", func(t *testing.T) {
		sidx := NewSourceIndexes(10, 3, 1)
		result := sidx.Tab()
		c.Assert(result, qt.Equals, 11)
		// 8 - (3 % 8) = 8 - 3 = 5, so column = 3 + 5 = 8
		c.Assert(sidx.Column(), qt.Equals, 8)
	})

	t.Run("from column 7", func(t *testing.T) {
		sidx := NewSourceIndexes(10, 7, 1)
		result := sidx.Tab()
		c.Assert(result, qt.Equals, 11)
		// 8 - (7 % 8) = 8 - 7 = 1, so column = 7 + 1 = 8
		c.Assert(sidx.Column(), qt.Equals, 8)
	})

	t.Run("from column 8", func(t *testing.T) {
		sidx := NewSourceIndexes(10, 8, 1)
		result := sidx.Tab()
		c.Assert(result, qt.Equals, 11)
		// 8 - (8 % 8) = 8 - 0 = 8, so column = 8 + 8 = 16
		c.Assert(sidx.Column(), qt.Equals, 16)
	})
}

// SourceContext.WithoutScopes

func TestSourceContext_WithoutScopes(t *testing.T) {
	c := qt.New(t)

	t.Run("non-nil context", func(t *testing.T) {
		scope := NewScope()
		origin := &OriginInfo{Identifier: "let"}
		sctx := &SourceContext{
			Text:   "hello",
			File:   "test.scm",
			Start:  NewSourceIndexes(0, 0, 1),
			End:    NewSourceIndexes(5, 5, 1),
			Scopes: []*Scope{scope},
			Origin: origin,
		}
		result := sctx.WithoutScopes()
		c.Assert(result.Text, qt.Equals, "hello")
		c.Assert(result.File, qt.Equals, "test.scm")
		c.Assert(result.Scopes, qt.IsNil)
		c.Assert(result.Origin, qt.Equals, origin)
	})

	t.Run("nil context", func(t *testing.T) {
		var sctx *SourceContext
		c.Assert(sctx.WithoutScopes(), qt.IsNil)
	})
}

// FlipScopeInSet

func TestFlipScopeInSet(t *testing.T) {
	c := qt.New(t)
	s1 := NewScope()
	s2 := NewScope()

	t.Run("add when absent", func(t *testing.T) {
		result := FlipScopeInSet(nil, s1)
		c.Assert(len(result), qt.Equals, 1)
		c.Assert(HasScope(result, s1), qt.IsTrue)
	})

	t.Run("remove when present", func(t *testing.T) {
		result := FlipScopeInSet([]*Scope{s1, s2}, s1)
		c.Assert(len(result), qt.Equals, 1)
		c.Assert(HasScope(result, s1), qt.IsFalse)
		c.Assert(HasScope(result, s2), qt.IsTrue)
	})
}

// FlipScope on syntax objects

func TestFlipScope_Symbol(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	sym := NewSyntaxSymbol("x", NewZeroValueSourceContext())

	// Add scope via flip
	result := FlipScope(sym, scope)
	flipped, ok := result.(*SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(HasScope(flipped.SourceContext().Scopes, scope), qt.IsTrue)

	// Flip again to remove
	result2 := FlipScope(flipped, scope)
	flipped2, ok := result2.(*SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(HasScope(flipped2.SourceContext().Scopes, scope), qt.IsFalse)
}

func TestFlipScope_Pair(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	sym := NewSyntaxSymbol("a", NewZeroValueSourceContext())
	pair := NewSyntaxCons(sym, SyntaxEmptyList, NewZeroValueSourceContext())

	result := FlipScope(pair, scope)
	flippedPair, ok := result.(*SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := flippedPair.SyntaxCar().(*SyntaxSymbol)
	c.Assert(HasScope(car.SourceContext().Scopes, scope), qt.IsTrue)
}

func TestFlipScope_NilAndDefault(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()

	c.Assert(FlipScope(nil, scope), qt.IsNil)
	c.Assert(FlipScope(SyntaxVoid, scope), qt.Equals, SyntaxVoid)
	c.Assert(FlipScope(SyntaxVoid, nil), qt.Equals, SyntaxVoid)

	obj := NewSyntaxObject(values.NewInteger(42), NewZeroValueSourceContext())
	c.Assert(FlipScope(obj, scope), qt.Equals, obj)
}

func TestFlipScope_SymbolNilSourceContext(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	sym := NewSyntaxSymbol("x", nil)

	result := FlipScope(sym, scope)
	flipped, ok := result.(*SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(HasScope(flipped.SourceContext().Scopes, scope), qt.IsTrue)
}

// AddScopeToSyntax

func TestAddScopeToSyntax_Symbol(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	sym := NewSyntaxSymbol("y", NewZeroValueSourceContext())

	result := AddScopeToSyntax(sym, scope)
	added, ok := result.(*SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(HasScope(added.SourceContext().Scopes, scope), qt.IsTrue)
}

func TestAddScopeToSyntax_Pair(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	sym := NewSyntaxSymbol("z", NewZeroValueSourceContext())
	pair := NewSyntaxCons(sym, SyntaxEmptyList, NewZeroValueSourceContext())

	result := AddScopeToSyntax(pair, scope)
	_, ok := result.(*SyntaxPair)
	c.Assert(ok, qt.IsTrue)
}

func TestAddScopeToSyntax_NilAndDefault(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()

	c.Assert(AddScopeToSyntax(nil, scope), qt.IsNil)
	c.Assert(AddScopeToSyntax(SyntaxVoid, nil), qt.Equals, SyntaxVoid)

	obj := NewSyntaxObject(values.NewString("hello"), NewZeroValueSourceContext())
	c.Assert(AddScopeToSyntax(obj, scope), qt.Equals, obj)
}

// SyntaxSymbol.WithResolvedBinding

func TestSyntaxSymbol_WithResolvedBinding(t *testing.T) {
	c := qt.New(t)
	sym := NewSyntaxSymbol("foo", NewZeroValueSourceContext())

	binding := "some-binding"
	result := sym.WithResolvedBinding(binding)

	c.Assert(result.Sym, qt.Equals, sym.Sym)
	c.Assert(result.ResolvedBinding, qt.Equals, binding)
	c.Assert(result.SourceContext(), qt.IsNotNil)
}

// NewSyntaxSymbolForSyntaxSymbol

func TestNewSyntaxSymbolForSyntaxSymbol(t *testing.T) {
	c := qt.New(t)
	original := NewSyntaxSymbol("bar", NewZeroValueSourceContext())
	original.ResolvedBinding = "orig-binding"

	sctx := NewSourceContext("bar", "new.scm", NewSourceIndexes(0, 0, 1), NewSourceIndexes(3, 3, 1))
	result := NewSyntaxSymbolForSyntaxSymbol(original, sctx)

	c.Assert(result.Sym, qt.Equals, original.Sym)
	// NewSyntaxSymbolForSyntaxSymbol does not copy ResolvedBinding
	c.Assert(result.ResolvedBinding, qt.IsNil)
	c.Assert(result.SourceContext(), qt.Equals, sctx)
}
