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

package values

// SyntaxBase provides a common SourceContext() implementation for
// concrete syntax types via Go struct embedding. It eliminates
// boilerplate SourceContext() methods across the syntax type set
// (SyntaxObject, SyntaxSymbol, SyntaxPair, SyntaxVector, SyntaxComment,
// SyntaxDatum*, SyntaxDirective).
//
// The sourceContext field is unexported; construct via NewSyntaxBase.
//
// Note: IsVoid() and UnwrapAll() cannot be provided here:
//   - IsVoid() requires nil receiver checks, which don't work with embedding
//   - UnwrapAll() needs access to the outer type (self), not the embedded struct
type SyntaxBase struct {
	sourceContext *SourceContext
}

// NewSyntaxBase constructs a SyntaxBase carrying the given source context.
func NewSyntaxBase(sc *SourceContext) SyntaxBase {
	return SyntaxBase{sourceContext: sc}
}

// SourceContext returns the source context carried by the embedding type.
func (p *SyntaxBase) SourceContext() *SourceContext {
	return p.sourceContext
}

// SetSourceContext replaces the source context. Used by syntax-list
// builders that thread per-element source contexts through a chain of
// pairs after construction.
func (p *SyntaxBase) SetSourceContext(sc *SourceContext) {
	p.sourceContext = sc
}
