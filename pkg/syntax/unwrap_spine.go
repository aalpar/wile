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

// UnwrapSpine unwraps v's pairs, vectors and boxes to Scheme pairs, vectors and
// boxes all the way down, keeps identifier leaves as *SyntaxSymbol, and unwraps
// every other atom (design §3.5, Q6). It is the form an explicit-renaming
// transformer receives, on both syntax layers (%syntax-spine in the Scheme one),
// so the two hand over one shape. The InputSpine maps each container of that
// form back to the syntax node it was unwrapped from.
//
// Keeping the leaves is what keeps an identifier's scopes through the
// transformer; unwrapping the spine is what keeps (pair? x) answering #t on a
// form element, which a whole-tree pass-through of *SyntaxPair would silently
// flip to #f while car/cdr kept working. It is UnwrapAll's walker with the
// leaves kept, so it terminates on a cyclic datum and preserves whatever
// sharing its input still carries. The Scheme layer stamps scopes onto the
// form before %syntax-spine sees it, which copies the tree, so there a
// reader-shared datum (#0=(a b) #0#) arrives as two copies; the Go boundary
// stamps nothing and hands over one.
func UnwrapSpine(v values.Value) (values.Value, *InputSpine) {
	q := &InputSpine{
		pairs:   make(map[*values.Pair]*SyntaxPair),
		vectors: make(map[*values.Vector]*SyntaxVector),
		boxes:   make(map[*values.Box]*SyntaxBox),
	}
	sv, ok := v.(SyntaxValue)
	if !ok {
		return v, q
	}
	cache := make(map[SyntaxValue]values.Value)
	raw := unwrapShared(sv, cache, true)
	for node, out := range cache {
		q.record(node, out)
	}
	return raw, q
}

// InputSpine maps the pairs, vectors and boxes of a value UnwrapSpine produced,
// by identity, to the syntax nodes they came from. A transformer that returns
// one of them unchanged is handing the user's own form back; the expander uses
// the map to tell that from a form the transformer built, and to give the
// rebuilt node its own source position (see schemeutil.DatumToSyntaxValueKeeping).
// Atoms are not recorded: a raw 5 or #t has no identity to tell its source by.
type InputSpine struct {
	pairs   map[*values.Pair]*SyntaxPair
	vectors map[*values.Vector]*SyntaxVector
	boxes   map[*values.Box]*SyntaxBox
}

// record adds out when it is the container unwrapped from node. Only the
// container node itself is taken, never a wrapper (a SyntaxObject or a datum
// label) that caches the same result: each raw container is made from exactly
// one container node, so the map does not depend on cache iteration order.
func (p *InputSpine) record(node SyntaxValue, out values.Value) {
	switch x := node.(type) {
	case *SyntaxPair:
		raw, ok := out.(*values.Pair)
		if ok {
			p.pairs[raw] = x
		}
	case *SyntaxVector:
		raw, ok := out.(*values.Vector)
		if ok {
			p.vectors[raw] = x
		}
	case *SyntaxBox:
		raw, ok := out.(*values.Box)
		if ok {
			p.boxes[raw] = x
		}
	}
}

// Origin returns the syntax node v was unwrapped from, or nil when v is not a
// pair, vector or box of the spine.
func (p *InputSpine) Origin(v values.Value) SyntaxValue {
	if p == nil {
		return nil
	}
	switch x := v.(type) {
	case *values.Pair:
		node, ok := p.pairs[x]
		if ok {
			return node
		}
	case *values.Vector:
		node, ok := p.vectors[x]
		if ok {
			return node
		}
	case *values.Box:
		node, ok := p.boxes[x]
		if ok {
			return node
		}
	}
	return nil
}
