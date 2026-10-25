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
)

// boxedSymbol descends the first slot of pairs, vectors and boxes to the symbol
// at the bottom of a shape built by TestScopeOperationsReachBoxContent.
func boxedSymbol(c *qt.C, stx SyntaxValue) *SyntaxSymbol {
	for {
		switch s := stx.(type) {
		case *SyntaxSymbol:
			return s
		case *SyntaxPair:
			stx = s.Values[0]
		case *SyntaxVector:
			stx = s.Values[0]
		case *SyntaxBox:
			stx = s.Value
		default:
			c.Fatalf("unexpected node %T", stx)
			return nil
		}
	}
}

// TestScopeOperationsReachBoxContent pins AddScope and FlipScope as duals on a
// box's content, at the top level and nested in a pair and a vector. The add
// side always descended into a box; FlipScope and the fused stamp walk did not,
// so an intro scope added on the way into a macro never came off, and
// AddScopeToSyntax skipped a top-level box altogether.
func TestScopeOperationsReachBoxContent(t *testing.T) {
	shapes := []struct {
		name  string
		build func() SyntaxValue
	}{
		{"box", func() SyntaxValue {
			return NewSyntaxBox(NewSyntaxSymbol("y", &SourceContext{}), &SourceContext{})
		}},
		{"box in pair", func() SyntaxValue {
			box := NewSyntaxBox(NewSyntaxSymbol("y", &SourceContext{}), &SourceContext{})
			return NewSyntaxCons(box, SyntaxEmptyList, &SourceContext{})
		}},
		{"box in vector", func() SyntaxValue {
			box := NewSyntaxBox(NewSyntaxSymbol("y", &SourceContext{}), &SourceContext{})
			return NewSyntaxVector(&SourceContext{}, box)
		}},
	}
	origin := &OriginInfo{Identifier: "m"}
	ops := []struct {
		name string
		// apply returns the result and whether its symbol must carry scope.
		apply func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool)
	}{
		{"AddScopeToSyntax adds", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			return AddScopeToSyntax(stx, scope), true
		}},
		{"FlipScope adds", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			return FlipScope(stx, scope), true
		}},
		{"FlipScope undoes AddScopeToSyntax", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			return FlipScope(AddScopeToSyntax(stx, scope), scope), false
		}},
		{"FlipScope twice is identity", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			return FlipScope(FlipScope(stx, scope), scope), false
		}},
		{"stamp walk undoes AddScopeToSyntax on built nodes", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			return FlipScopeStampOrigin(AddScopeToSyntax(stx, scope), scope, origin, NewPassThrough(nil)), false
		}},
		{"stamp walk undoes AddScopeToSyntax on pass-through nodes", func(stx SyntaxValue, scope *Scope) (SyntaxValue, bool) {
			in := AddScopeToSyntax(stx, scope)
			return FlipScopeStampOrigin(in, scope, origin, NewPassThrough(in)), false
		}},
	}
	for _, shape := range shapes {
		for _, op := range ops {
			t.Run(shape.name+"/"+op.name, func(t *testing.T) {
				c := qt.New(t)
				scope := NewScope()
				out, want := op.apply(shape.build(), scope)
				got := boxedSymbol(c, out).SourceContext().Scopes.Has(scope)
				c.Assert(got, qt.Equals, want)
			})
		}
	}
}
