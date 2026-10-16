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

import "github.com/aalpar/wile/pkg/values"

// UnwrapSpine unwraps v's pairs and vectors to Scheme pairs and vectors all the way
// down, keeps identifier leaves as *SyntaxSymbol, and unwraps every other atom
// (design §3.5, Q6). It is the form an explicit-renaming transformer receives,
// on both syntax layers: (%syntax-spine stx) in the Scheme derivation and the
// Go ER boundary in pkg/machine/compilation. One function, so the two cannot
// disagree on the shape.
//
// Keeping the leaves is what keeps an identifier's scopes through the
// transformer; unwrapping the spine is what keeps (pair? x) answering #t on a
// form element, which a whole-tree pass-through of *SyntaxPair would silently
// flip to #f while car/cdr kept working.
func UnwrapSpine(v values.Value) values.Value {
	switch s := v.(type) {
	case *SyntaxSymbol:
		return s
	case *SyntaxPair:
		return unwrapSpineList(s)
	case *SyntaxVector:
		elems := make([]values.Value, len(s.Values))
		for i, e := range s.Values {
			elems[i] = UnwrapSpine(e)
		}
		return values.NewVector(elems...)
	case SyntaxValue:
		return s.Unwrap()
	default:
		return v
	}
}

// unwrapSpineList walks the cdr chain iteratively: list length must not become Go
// stack depth (the datum->syntax lesson in pkg/registry/core/prim_syntax.go).
// Nesting still recurses, bounded by the reader's and datum->syntax's depth
// limits.
func unwrapSpineList(sp *SyntaxPair) values.Value {
	var head, tail *values.Pair
	var cur SyntaxValue = sp
	for {
		p, ok := cur.(*SyntaxPair)
		if !ok || IsSyntaxEmptyList(p) {
			break
		}
		cell := values.NewCons(UnwrapSpine(p.SyntaxCar()), values.EmptyList)
		if head == nil {
			head = cell
		} else {
			tail.SetCdr(cell)
		}
		tail = cell
		cur = p.SyntaxCdr()
	}
	if head == nil {
		return values.EmptyList
	}
	if !IsSyntaxEmptyList(cur) {
		tail.SetCdr(UnwrapSpine(cur))
	}
	return head
}
