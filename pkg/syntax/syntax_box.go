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
	"fmt"

	"github.com/aalpar/wile/pkg/values"
)

var (
	_ values.Value = (*SyntaxBox)(nil)
	_ SyntaxValue  = (*SyntaxBox)(nil)
)

// SyntaxBox wraps a box (#&datum) with source context, keeping the boxed datum
// as syntax.
//
// The alternative — unwrapping the content at read time and storing a plain
// *values.Box in a SyntaxObject — is about ten lines instead of this file, and
// costs the content its source location and its participation in hygiene. A box
// is a container like a vector, so it gets the container treatment: the content
// keeps its wrappers, an error inside #&(1 . 2) still has somewhere to point,
// and a box in a macro template propagates scopes to what it holds.
type SyntaxBox struct {
	Value SyntaxValue
	syntaxBase
}

// NewSyntaxBox creates a new syntax box wrapping the given datum.
func NewSyntaxBox(value SyntaxValue, sctx *SourceContext) *SyntaxBox {
	return &SyntaxBox{
		Value:      value,
		syntaxBase: values.NewSyntaxBase(sctx),
	}
}

// AddScope propagates the scope into the boxed datum, as SyntaxVector does for
// its elements. Only symbols accumulate scopes; everything else is threaded
// through unchanged.
func (p *SyntaxBox) AddScope(scope *Scope) SyntaxValue {
	if p == nil || p.Value == nil {
		return p
	}
	inner := mapSyntaxTree(p.Value, func(node SyntaxValue) SyntaxValue {
		adder, ok := node.(interface {
			AddScope(*Scope) SyntaxValue
		})
		if ok {
			return adder.AddScope(scope)
		}
		return node
	})
	if inner == p.Value {
		return p
	}
	return NewSyntaxBox(inner, p.SourceContext())
}

// IsVoid returns true if the syntax box is nil.
func (p *SyntaxBox) IsVoid() bool {
	return p == nil
}

// Unwrap returns the boxed datum, still wrapped as syntax.
func (p *SyntaxBox) Unwrap() values.Value {
	return p.Value
}

// UnwrapAll recursively unwraps to a *values.Box holding the unwrapped content.
func (p *SyntaxBox) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// EqualTo performs pointer comparison, as the other syntax types do.
func (p *SyntaxBox) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxBox)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString renders the box in its read syntax, so it round trips.
func (p *SyntaxBox) SchemeString() string {
	if p == nil || p.Value == nil {
		return "#<box:void>"
	}
	return fmt.Sprintf("%s%s", values.PrefixBox, p.Value.SchemeString())
}
