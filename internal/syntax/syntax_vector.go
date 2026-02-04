// Copyright 2025 Aaron Alpar
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
	"strings"

	"github.com/aalpar/wile/values"
)

var (
	_ values.Value = (*SyntaxVector)(nil)
	_ SyntaxValue  = (*SyntaxVector)(nil)
)

// SyntaxVector wraps a Scheme vector with source context.
type SyntaxVector struct {
	Values        []SyntaxValue
	sourceContext *SourceContext
}

// AddScope returns the vector unchanged as vectors do not track scopes.
func (p *SyntaxVector) AddScope(_ *Scope) SyntaxValue {
	return p
}

// NewSyntaxVector creates a new syntax vector with the given source context and elements.
func NewSyntaxVector(sc *SourceContext, vs ...SyntaxValue) *SyntaxVector {
	q := &SyntaxVector{
		Values:        vs,
		sourceContext: sc,
	}
	return q
}

// SourceContext returns the source context for this syntax vector.
func (p *SyntaxVector) SourceContext() *SourceContext {
	return p.sourceContext
}

// UnwrapAll recursively unwraps all elements to produce a plain values.Vector.
func (p *SyntaxVector) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

func (p *SyntaxVector) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	vq := make([]values.Value, len(p.Values))
	for i, v := range p.Values {
		vq[i] = v
	}
	q := values.NewVector(vq...)
	return q
}

// IsVoid returns true if the syntax vector is nil.
func (p *SyntaxVector) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme representation of the syntax vector.
func (p *SyntaxVector) SchemeString() string {
	if p.IsVoid() {
		return "#'<void>"
	}
	q := strings.Builder{}
	q.WriteString("#'(")
	for i, v := range p.Values {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
	}
	q.WriteString(")")
	return q.String()
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxVector) EqualTo(o values.Value) bool {
	v, ok := o.(*SyntaxVector)
	if !ok {
		return false
	}
	return p == v
}
