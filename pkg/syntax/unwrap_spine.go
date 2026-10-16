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
// so the two hand over one shape.
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
func UnwrapSpine(v values.Value) values.Value {
	sv, ok := v.(SyntaxValue)
	if !ok {
		return v
	}
	return unwrapShared(sv, make(map[SyntaxValue]values.Value), true)
}
