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

// TestMapSyntaxTreeTerminatesOnCycle pins MapSyntaxTree on cyclic syntax. A
// reader cycle reached past its label (`#1#` in `(m #0=(#1=(x . #0#)) #1#)`) is
// a raw cyclic pair, and the expander stamps scopes onto it; with no visited set
// the walk overflowed the Go stack, a fatal error no recover contains. Each row
// builds a cycle closing through a different container, applies the scope, and
// checks the result is a cycle of rebuilt nodes whose symbols carry it (and
// that a second flip takes it off again).
func TestMapSyntaxTreeTerminatesOnCycle(t *testing.T) {
	cases := []struct {
		name string
		// build returns a cyclic node and the symbol inside it.
		build func() SyntaxValue
		// back follows the cycle from a result node back to that node, and
		// returns the symbol on the way.
		back func(c *qt.C, stx SyntaxValue) (SyntaxValue, *SyntaxSymbol)
	}{
		{
			name: "cdr cycle #0=(x . #0#)",
			build: func() SyntaxValue {
				p := NewSyntaxCons(NewSyntaxSymbol("x", &SourceContext{}), nil, &SourceContext{})
				p.Values[1] = p
				return p
			},
			back: func(c *qt.C, stx SyntaxValue) (SyntaxValue, *SyntaxSymbol) {
				p := stx.(*SyntaxPair)
				return p.Values[1], p.Values[0].(*SyntaxSymbol)
			},
		},
		{
			name: "car cycle #0=((x . #0#))",
			build: func() SyntaxValue {
				inner := NewSyntaxCons(NewSyntaxSymbol("x", &SourceContext{}), nil, &SourceContext{})
				outer := NewSyntaxCons(inner, SyntaxEmptyList, &SourceContext{})
				inner.Values[1] = outer
				return outer
			},
			back: func(c *qt.C, stx SyntaxValue) (SyntaxValue, *SyntaxSymbol) {
				inner := stx.(*SyntaxPair).Values[0].(*SyntaxPair)
				return inner.Values[1], inner.Values[0].(*SyntaxSymbol)
			},
		},
		{
			name: "vector cycle #0=#(x #0#)",
			build: func() SyntaxValue {
				v := NewSyntaxVector(&SourceContext{}, NewSyntaxSymbol("x", &SourceContext{}), nil)
				v.Values[1] = v
				return v
			},
			back: func(c *qt.C, stx SyntaxValue) (SyntaxValue, *SyntaxSymbol) {
				v := stx.(*SyntaxVector)
				return v.Values[1], v.Values[0].(*SyntaxSymbol)
			},
		},
		{
			name: "box cycle #0=#&(x . #0#)",
			build: func() SyntaxValue {
				p := NewSyntaxCons(NewSyntaxSymbol("x", &SourceContext{}), nil, &SourceContext{})
				b := NewSyntaxBox(p, &SourceContext{})
				p.Values[1] = b
				return b
			},
			back: func(c *qt.C, stx SyntaxValue) (SyntaxValue, *SyntaxSymbol) {
				p := stx.(*SyntaxBox).Value.(*SyntaxPair)
				return p.Values[1], p.Values[0].(*SyntaxSymbol)
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			scope := NewScope()
			in := tc.build()
			// Every input node, so the result can be checked to share none.
			inputs := map[SyntaxValue]bool{}
			for node, i := in, 0; i < 4; i++ {
				inputs[node] = true
				node, _ = tc.back(c, node)
			}

			added := AddScopeToSyntax(in, scope)
			assertScopedCycle(c, tc.back, added, inputs, scope, true)
			_, inSym := tc.back(c, in)
			c.Assert(inSym.SourceContext().Scopes.Has(scope), qt.IsFalse, qt.Commentf("input is not mutated"))
			assertScopedCycle(c, tc.back, FlipScope(added, scope), inputs, scope, false)
		})
	}
}

// assertScopedCycle follows back from out until a node repeats. The memo starts
// below treeMapMemoDepth, so the result is the cycle unrolled down to there and
// then closed; every symbol on the way must agree on scope, and no node may be
// one of the input's.
func assertScopedCycle(c *qt.C, back func(*qt.C, SyntaxValue) (SyntaxValue, *SyntaxSymbol), out SyntaxValue, inputs map[SyntaxValue]bool, scope *Scope, want bool) {
	seen := map[SyntaxValue]bool{}
	node := out
	for i := 0; !seen[node]; i++ {
		c.Assert(i < 4*treeMapMemoDepth, qt.IsTrue, qt.Commentf("the result does not close"))
		c.Assert(inputs[node], qt.IsFalse, qt.Commentf("the result re-enters the input at step %d", i))
		seen[node] = true
		var sym *SyntaxSymbol
		node, sym = back(c, node)
		c.Assert(sym.SourceContext().Scopes.Has(scope), qt.Equals, want, qt.Commentf("step %d", i))
	}
}

// TestMapSyntaxTreeDeepAcyclic: past the memo depth an acyclic walk still
// rebuilds every changed node, returns an unchanged one by identity, and keeps a
// subtree reached twice shared.
func TestMapSyntaxTreeDeepAcyclic(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
	num := func() SyntaxValue {
		return NewSyntaxObject(values.NewInteger(1), &SourceContext{})
	}
	n := 2 * treeMapMemoDepth

	syms := make([]SyntaxValue, n)
	for i := range syms {
		syms[i] = NewSyntaxSymbol("x", &SourceContext{})
	}
	out := AddScopeToSyntax(SyntaxList(&SourceContext{}, syms...), scope)
	count := 0
	for node := out; !IsSyntaxEmptyList(node); node = node.(*SyntaxPair).Values[1] {
		c.Assert(node.(*SyntaxPair).Values[0].(*SyntaxSymbol).SourceContext().Scopes.Has(scope), qt.IsTrue)
		count++
	}
	c.Assert(count, qt.Equals, n)

	nums := make([]SyntaxValue, n)
	for i := range nums {
		nums[i] = num()
	}
	unchanged := SyntaxList(&SourceContext{}, nums...)
	c.Assert(AddScopeToSyntax(unchanged, scope), qt.Equals, unchanged)

	shared := NewSyntaxCons(NewSyntaxSymbol("x", &SourceContext{}), SyntaxEmptyList, &SourceContext{})
	elems := append(nums[:n-2:n-2], shared, shared)
	deep := AddScopeToSyntax(SyntaxList(&SourceContext{}, elems...), scope)
	tail := deep
	for range n - 2 {
		tail = tail.(*SyntaxPair).Values[1]
	}
	first := tail.(*SyntaxPair).Values[0]
	second := tail.(*SyntaxPair).Values[1].(*SyntaxPair).Values[0]
	c.Assert(first, qt.Not(qt.Equals), SyntaxValue(shared))
	c.Assert(second, qt.Equals, first)
}
