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

// SyntaxVector wraps a Scheme vector with source context.
// Defined in package values (the empty-list duality merge — see
// values/syntax_vector.go). Methods that need to traverse concrete
// syntax types defined in this package (AddScope, UnwrapAll) dispatch
// via hooks registered at init time below.
type SyntaxVector = values.SyntaxVector

// NewSyntaxVector creates a new syntax vector with the given source context
// and elements.
func NewSyntaxVector(sc *SourceContext, vs ...SyntaxValue) *SyntaxVector {
	return values.NewSyntaxVector(sc, vs...)
}

func init() {
	// Recursive scope propagation across nested syntax types lives here in
	// internal/syntax (where the concrete syntax types are defined). The
	// values.SyntaxVector.AddScope method dispatches to this hook.
	values.SyntaxVectorAddScopeFunc = func(p *SyntaxVector, scope *Scope) values.SyntaxValue {
		return mapSyntaxTree(p, func(node values.SyntaxValue) values.SyntaxValue {
			adder, ok := node.(interface {
				AddScope(*Scope) values.SyntaxValue
			})
			if ok {
				return adder.AddScope(scope)
			}
			return node
		})
	}

	// Cycle-aware recursive unwrap. The implementation below dispatches on
	// concrete syntax types defined in this package and is registered into
	// values so that values.SyntaxVector.UnwrapAll can call it.
	values.SyntaxValueUnwrapAllFunc = UnwrapAllShared

	// Register the syntax-level void singleton so values.SyntaxVector with
	// a nil receiver returns it from SyntaxForEach (preserves the original
	// "syntax void tail" behavior).
	values.SyntaxVectorVoidValue = SyntaxVoid
}
