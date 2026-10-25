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

// cycleShapes builds cyclic syntax holding leaf, closing through each container.
// "mixed" meets the back edge into a under b before the walk reaches a's
// changed leaf, so b must not be returned as unchanged.
var cycleShapes = []struct {
	name  string
	build func(leaf SyntaxValue) SyntaxValue
}{
	{"cdr cycle #0=(x . #0#)", func(leaf SyntaxValue) SyntaxValue {
		p := NewSyntaxCons(leaf, nil, &SourceContext{})
		p.Values[1] = p
		return p
	}},
	{"car cycle #0=((x . #0#))", func(leaf SyntaxValue) SyntaxValue {
		inner := NewSyntaxCons(leaf, nil, &SourceContext{})
		outer := NewSyntaxCons(inner, SyntaxEmptyList, &SourceContext{})
		inner.Values[1] = outer
		return outer
	}},
	{"vector cycle #0=#(x #0#)", func(leaf SyntaxValue) SyntaxValue {
		v := NewSyntaxVector(&SourceContext{}, leaf, nil)
		v.Values[1] = v
		return v
	}},
	{"box cycle #0=#&(x . #0#)", func(leaf SyntaxValue) SyntaxValue {
		p := NewSyntaxCons(leaf, nil, &SourceContext{})
		b := NewSyntaxBox(p, &SourceContext{})
		p.Values[1] = b
		return b
	}},
	{"mixed #0=((1 . #0#) x)", func(leaf SyntaxValue) SyntaxValue {
		b := NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), &SourceContext{}), nil, &SourceContext{})
		a := NewSyntaxCons(b, NewSyntaxCons(leaf, SyntaxEmptyList, &SourceContext{}), &SourceContext{})
		b.Values[1] = a
		return a
	}},
}

// reachable returns every node of stx's graph but the shared empty list, and
// calls visit on each symbol.
func reachable(stx SyntaxValue, visit func(*SyntaxSymbol)) map[SyntaxValue]bool {
	q := map[SyntaxValue]bool{}
	var walk func(SyntaxValue)
	walk = func(v SyntaxValue) {
		if v == nil || q[v] || IsSyntaxEmptyList(v) {
			return
		}
		q[v] = true
		switch n := v.(type) {
		case *SyntaxSymbol:
			if visit != nil {
				visit(n)
			}
		case *SyntaxPair:
			walk(n.Values[0])
			walk(n.Values[1])
		case *SyntaxVector:
			for _, e := range n.Values {
				walk(e)
			}
		case *SyntaxBox:
			walk(n.Value)
		}
	}
	walk(stx)
	return q
}

// TestMapSyntaxTreeTerminatesOnCycle pins MapSyntaxTree on cyclic syntax. A
// reader cycle reached past its label (`#1#` in `(m #0=(#1=(x . #0#)) #1#)`) is
// a raw cyclic pair, and the expander stamps scopes onto it; with no visited set
// the walk overflowed the Go stack, a fatal error no recover contains. Each row
// applies a scope to a cycle holding a symbol and checks that the result
// terminates, reaches no input compound, and carries the scope on every symbol it
// reaches; a second flip must take it off everywhere again.
func TestMapSyntaxTreeTerminatesOnCycle(t *testing.T) {
	for _, shape := range cycleShapes {
		t.Run(shape.name, func(t *testing.T) {
			c := qt.New(t)
			scope := NewScope()
			in := shape.build(NewSyntaxSymbol("x", &SourceContext{}))
			inputs := reachable(in, nil)

			check := func(out SyntaxValue, want bool) {
				symbols := 0
				for node := range reachable(out, func(sym *SyntaxSymbol) {
					symbols++
					c.Assert(sym.SourceContext().Scopes.Has(scope), qt.Equals, want)
				}) {
					_, leaf := node.(*SyntaxObject)
					if !leaf {
						c.Assert(inputs[node], qt.IsFalse, qt.Commentf("the result reaches an input node %T", node))
					}
				}
				c.Assert(symbols > 0, qt.IsTrue)
			}
			added := AddScopeToSyntax(in, scope)
			check(added, true)
			reachable(in, func(sym *SyntaxSymbol) {
				c.Assert(sym.SourceContext().Scopes.Has(scope), qt.IsFalse, qt.Commentf("input is not mutated"))
			})
			check(FlipScope(added, scope), false)
		})
	}
}

// TestMapSyntaxTreeUnchangedCycleIsCopied pins current behaviour: a cycle the
// walk changes nothing in still comes back as new nodes, not the input, because
// a revisited in-progress node is always rebuilt through its placeholder. This
// flips if sharing is ever preserved for cycles (filed).
func TestMapSyntaxTreeUnchangedCycleIsCopied(t *testing.T) {
	for _, shape := range cycleShapes {
		t.Run(shape.name, func(t *testing.T) {
			in := shape.build(NewSyntaxObject(values.NewInteger(2), &SourceContext{}))
			qt.Assert(t, AddScopeToSyntax(in, NewScope()), qt.Not(qt.Equals), in)
		})
	}
}

// TestMapSyntaxTreeDeepAcyclic: past the memo depth an acyclic walk still
// rebuilds every changed node and returns an unchanged one by identity.
func TestMapSyntaxTreeDeepAcyclic(t *testing.T) {
	c := qt.New(t)
	scope := NewScope()
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
		nums[i] = NewSyntaxObject(values.NewInteger(1), &SourceContext{})
	}
	unchanged := SyntaxList(&SourceContext{}, nums...)
	c.Assert(AddScopeToSyntax(unchanged, scope), qt.Equals, unchanged)
}

// TestMapSyntaxTreeShallowSharingIsNotPreserved pins the documented gap: a
// changed subterm reached twice in a shallow walk is rebuilt twice, so the two
// references in the result are different nodes (past treeMapMemoDepth they are
// one). Filed as a row of its own; this pin flips when sharing is preserved.
func TestMapSyntaxTreeShallowSharingIsNotPreserved(t *testing.T) {
	c := qt.New(t)
	shared := NewSyntaxCons(NewSyntaxSymbol("x", &SourceContext{}), SyntaxEmptyList, &SourceContext{})
	out := AddScopeToSyntax(SyntaxList(&SourceContext{}, shared, shared), NewScope()).(*SyntaxPair)
	first := out.Values[0]
	second := out.Values[1].(*SyntaxPair).Values[0]
	c.Assert(first, qt.Not(qt.Equals), SyntaxValue(shared))
	c.Assert(second, qt.Not(qt.Equals), first)
}
