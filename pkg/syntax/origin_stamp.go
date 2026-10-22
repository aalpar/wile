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
	"github.com/aalpar/wile/pkg/values"
)

// StampOrigin returns stx with origin recorded on the SourceContext of every
// node the transformer constructed: pairs, vectors, boxes, symbols and
// self-evaluating literals. The expander calls it once per transformer
// invocation, on the transformer's output, so a validation error raised on any
// node of the expansion can name the macro that produced it (ValidationError
// reads Source.Origin off the offending node).
//
// input is the form the transformer was handed. A node of stx that is a node
// of input, by identity, is the caller's own syntax passed through, not
// something the macro produced, and is left untouched along with everything
// beneath it: stamping it would report `expanded from 'do'` on a malformed form
// the user wrote in a do body. This is the semantics the Go syntax-rules
// template expander always had (a pattern variable keeps the syntax it
// captured), now enforced at the boundary instead of inside one transformer.
// A nil input stamps every node.
//
// It is a separate walk from MapSyntaxTree and FlipScope on purpose.
// MapSyntaxTree never calls its function on a pair, and it hands back unchanged
// nodes by identity so that most pairs share structure; the stamp has to reach
// the pair (the offending node for a malformed binding is the binding pair) and
// so rebuilds every node it visits. Folding it into FlipScope would change the
// contract six other callers share. Call it before FlipScope, while the
// input's own nodes are still the same objects the transformer returned.
//
// WithOrigin replaces the context's Origin rather than appending, which is
// correct because the caller builds the chain: the new OriginInfo's Parent is
// the origin the use site already carried.
//
// A nil origin returns stx unchanged. The empty list, comments, directives and
// datum labels pass through.
func StampOrigin(stx SyntaxValue, origin *OriginInfo, input SyntaxValue) SyntaxValue {
	if stx == nil || origin == nil {
		return stx
	}
	var passThrough map[SyntaxValue]struct{}
	if input != nil {
		passThrough = make(map[SyntaxValue]struct{})
		collectNodes(input, passThrough)
	}
	return stampOrigin(stx, origin, passThrough)
}

// collectNodes records every node of stx by identity.
func collectNodes(stx SyntaxValue, into map[SyntaxValue]struct{}) {
	if stx == nil {
		return
	}
	into[stx] = struct{}{}
	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return
		}
		collectNodes(s.Values[0], into)
		collectNodes(s.Values[1], into)
	case *SyntaxVector:
		for _, elem := range s.Values {
			collectNodes(elem, into)
		}
	case *SyntaxBox:
		collectNodes(s.Value, into)
	}
}

func stampOrigin(stx SyntaxValue, origin *OriginInfo, passThrough map[SyntaxValue]struct{}) SyntaxValue {
	if stx == nil {
		return nil
	}
	_, ok := passThrough[stx]
	if ok {
		return stx
	}

	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return s
		}
		var car, cdr SyntaxValue
		if s.Values[0] != nil {
			car = stampOrigin(s.Values[0], origin, passThrough)
		}
		if s.Values[1] != nil {
			cdr = stampOrigin(s.Values[1], origin, passThrough)
		}
		return NewSyntaxCons(car, cdr, s.SourceContext().WithOrigin(origin))

	case *SyntaxSymbol:
		return &SyntaxSymbol{
			Sym:             s.Sym,
			syntaxBase:      values.NewSyntaxBase(s.SourceContext().WithOrigin(origin)),
			ResolvedBinding: s.ResolvedBinding,
		}

	case *SyntaxObject:
		return &SyntaxObject{
			datum:      s.datum,
			syntaxBase: values.NewSyntaxBase(s.SourceContext().WithOrigin(origin)),
		}

	case *SyntaxVector:
		elems := make([]SyntaxValue, len(s.Values))
		for i, elem := range s.Values {
			if elem != nil {
				elems[i] = stampOrigin(elem, origin, passThrough)
			}
		}
		return NewSyntaxVector(s.SourceContext().WithOrigin(origin), elems...)

	case *SyntaxBox:
		return NewSyntaxBox(stampOrigin(s.Value, origin, passThrough), s.SourceContext().WithOrigin(origin))

	default:
		return stx
	}
}
