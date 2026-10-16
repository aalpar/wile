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
			got := UnwrapSpine(tc.in)
			qt.Assert(t, got, qt.Not(qt.IsNil))
			qt.Assert(t, onlySymbolSyntax(got), qt.IsTrue)
			qt.Assert(t, dropLeafSyntax(got).SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestUnwrapSpineKeepsTheLeaf: the leaf is the input's own identifier, so its
// scopes survive.
func TestUnwrapSpineKeepsTheLeaf(t *testing.T) {
	a := NewSyntaxSymbol("a", NewZeroValueSourceContext().WithScope(NewScope()))
	got, ok := UnwrapSpine(SyntaxList(nil, a)).(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got.Car() == values.Value(a), qt.IsTrue)
}

// TestUnwrapSpineCyclicLabel: a cyclic datum reaches here under its label
// wrapper, and the spine walk must terminate on it, keeping the cycle.
func TestUnwrapSpineCyclicLabel(t *testing.T) {
	cell := NewSyntaxCons(NewSyntaxSymbol("a", nil), SyntaxEmptyList, nil)
	cell.Values[1] = cell
	got, ok := UnwrapSpine(NewSyntaxDatumLabelAssignment(0, cell, nil)).(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got.Cdr() == values.Value(got), qt.IsTrue)
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
