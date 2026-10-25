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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestUnwrapSpine pins the postcondition: containers come back as plain
// values, and the only syntax left anywhere in the result is a *SyntaxSymbol.
func TestUnwrapSpine(t *testing.T) {
	sym := func(s string) *SyntaxSymbol {
		return NewSyntaxSymbol(s, nil)
	}
	num := func(n int64) SyntaxValue {
		return NewSyntaxObject(values.NewInteger(n), nil)
	}
	list := func(vs ...SyntaxValue) SyntaxValue {
		return SyntaxList(nil, vs...)
	}
	tcs := []struct {
		name string
		in   SyntaxValue
		want string
	}{
		{"symbol leaf", sym("a"), "a"},
		{"atom", num(3), "3"},
		{"empty list", SyntaxEmptyList, "()"},
		{"nested list", list(sym("a"), list(sym("b"), num(1))), "(a (b 1))"},
		{"improper tail", NewSyntaxCons(sym("a"), sym("b"), nil), "(a . b)"},
		{"vector", NewSyntaxVector(nil, sym("a"), list(sym("b"))), "#(a (b))"},
		{"box", NewSyntaxBox(list(sym("a"), num(2)), nil), "#&(a 2)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, _ := UnwrapSpine(tc.in)
			qt.Assert(t, got, qt.Not(qt.IsNil))
			qt.Assert(t, onlySymbolSyntax(got), qt.IsTrue)
			qt.Assert(t, dropLeafSyntax(got).SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestUnwrapSpineKeepsTheLeaf: in every container the leaf is the input's own
// identifier, so its scopes survive.
func TestUnwrapSpineKeepsTheLeaf(t *testing.T) {
	a := NewSyntaxSymbol("a", NewZeroValueSourceContext().WithScope(NewScope()))
	car := func(v values.Value) values.Value {
		return v.(*values.Pair).Car()
	}
	tcs := []struct {
		name string
		in   SyntaxValue
		leaf func(values.Value) values.Value
	}{
		{"list", SyntaxList(nil, a), car},
		{"improper tail", NewSyntaxCons(NewSyntaxSymbol("b", nil), a, nil), func(v values.Value) values.Value {
			return v.(*values.Pair).Cdr()
		}},
		{"vector", NewSyntaxVector(nil, a), func(v values.Value) values.Value {
			return v.(*values.Vector).Elems()[0]
		}},
		{"box", NewSyntaxBox(SyntaxList(nil, a), nil), func(v values.Value) values.Value {
			return car(v.(*values.Box).Unbox())
		}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, _ := UnwrapSpine(tc.in)
			qt.Assert(t, tc.leaf(got) == values.Value(a), qt.IsTrue)
		})
	}
}

// TestUnwrapSpineCyclicLabel: a cyclic datum reaches here under its label
// wrapper, and the spine walk must terminate on it, keeping the cycle.
func TestUnwrapSpineCyclicLabel(t *testing.T) {
	cell := NewSyntaxCons(NewSyntaxSymbol("a", nil), SyntaxEmptyList, nil)
	cell.Values[1] = cell
	raw, _ := UnwrapSpine(NewSyntaxDatumLabelAssignment(0, cell, nil))
	got, ok := raw.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got.Cdr() == values.Value(got), qt.IsTrue)
}

// TestUnwrapSpineUnlabeledCycle: a cycle with no label wrapper on the path into
// it still terminates, and shared structure stays shared.
func TestUnwrapSpineUnlabeledCycle(t *testing.T) {
	cell := NewSyntaxCons(NewSyntaxSymbol("x", nil), SyntaxEmptyList, nil)
	cell.Values[1] = cell
	raw, _ := UnwrapSpine(SyntaxList(nil, cell, cell))
	got, ok := raw.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	first, ok := got.Car().(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, first.Cdr() == values.Value(first), qt.IsTrue)
	second, ok := got.Cdr().(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, second.Car() == values.Value(first), qt.IsTrue)
}

// onlySymbolSyntax reports whether v holds no syntax other than identifiers.
func onlySymbolSyntax(v values.Value) bool {
	switch x := v.(type) {
	case *SyntaxSymbol:
		return true
	case SyntaxValue:
		return values.IsEmptyList(x)
	case *values.Pair:
		return onlySymbolSyntax(x.Car()) && onlySymbolSyntax(x.Cdr())
	case *values.Vector:
		for _, e := range x.Elems() {
			if !onlySymbolSyntax(e) {
				return false
			}
		}
		return true
	case *values.Box:
		return onlySymbolSyntax(x.Unbox())
	default:
		return true
	}
}

// dropLeafSyntax replaces each identifier leaf with its symbol, so a result
// can be compared by its printed form.
func dropLeafSyntax(v values.Value) values.Value {
	switch x := v.(type) {
	case *SyntaxSymbol:
		return x.Sym
	case *values.Pair:
		return values.NewCons(dropLeafSyntax(x.Car()), dropLeafSyntax(x.Cdr()))
	case *values.Vector:
		elems := make([]values.Value, len(x.Elems()))
		for i, e := range x.Elems() {
			elems[i] = dropLeafSyntax(e)
		}
		return values.NewVector(elems...)
	case *values.Box:
		return values.NewBox(dropLeafSyntax(x.Unbox()))
	default:
		return v
	}
}

// TestInputSpine: every container of the unwrapped form maps back to the very
// node it came from, so a rebuilt pass-through keeps that node's position;
// nothing else is a member.
func TestInputSpine(t *testing.T) {
	c := qt.New(t)
	num := func(n int64) SyntaxValue {
		return NewSyntaxObject(values.NewInteger(n), nil)
	}
	deep := SyntaxList(nil, num(1))
	vec := NewSyntaxVector(nil, deep)
	box := NewSyntaxBox(num(3), nil)
	tail := NewSyntaxCons(num(2), NewSyntaxCons(box, SyntaxEmptyList, nil), nil)
	list := NewSyntaxCons(vec, tail, nil)
	raw, spine := UnwrapSpine(list)

	top := raw.(*values.Pair)
	rawVec := top.Car().(*values.Vector)
	rawDeep := rawVec.Elems()[0]
	rawTail := top.Cdr().(*values.Pair)
	rawBox := rawTail.Cdr().(*values.Pair).Car()
	c.Assert(spine.Origin(top), qt.Equals, SyntaxValue(list))
	c.Assert(spine.Origin(rawTail), qt.Equals, SyntaxValue(tail))
	c.Assert(spine.Origin(rawVec), qt.Equals, SyntaxValue(vec))
	c.Assert(spine.Origin(rawDeep), qt.Equals, deep, qt.Commentf("a pair inside a vector"))
	c.Assert(spine.Origin(rawBox), qt.Equals, SyntaxValue(box))
	c.Assert(spine.Origin(values.NewCons(values.NewInteger(1), values.EmptyList)), qt.IsNil, qt.Commentf("equal but not identical"))
	c.Assert(spine.Origin(rawTail.Car()), qt.IsNil, qt.Commentf("an atom"))
	c.Assert((*InputSpine)(nil).Origin(top), qt.IsNil)

	c.Run("a label wrapper is not the origin", func(c *qt.C) {
		cell := NewSyntaxCons(NewSyntaxSymbol("a", nil), SyntaxEmptyList, nil)
		raw, spine := UnwrapSpine(NewSyntaxDatumLabelAssignment(0, cell, nil))
		c.Assert(spine.Origin(raw), qt.Equals, SyntaxValue(cell))
	})
	c.Run("a cyclic spine terminates", func(c *qt.C) {
		loop := NewSyntaxCons(num(1), SyntaxEmptyList, nil)
		loop.Values[1] = loop
		raw, spine := UnwrapSpine(loop)
		c.Assert(spine.Origin(raw), qt.Equals, SyntaxValue(loop))
	})
	c.Run("a non-syntax value has an empty spine", func(c *qt.C) {
		v := values.NewCons(values.NewInteger(1), values.EmptyList)
		raw, spine := UnwrapSpine(v)
		c.Assert(raw, qt.Equals, values.Value(v))
		c.Assert(spine.Origin(v), qt.IsNil)
	})
}
