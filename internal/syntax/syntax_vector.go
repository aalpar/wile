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
	"context"
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

// ForEach iterates over the elements of the vector as regular values in index order.
// It provides tuple-style iteration compatible with values.ForEachFunc callbacks.
func (p *SyntaxVector) ForEach(ctx context.Context, fn values.ForEachFunc) (values.Value, error) {
	if p.IsVoid() {
		return values.Void, nil
	}
	for i, v := range p.Values {
		hasNext := i+1 < len(p.Values)
		err := fn(ctx, i, hasNext, v)
		if err != nil {
			return nil, err
		}
	}
	return values.EmptyList, nil
}

// SyntaxForEach iterates over the syntax elements of the vector.
// A nil receiver is treated as the distinguished syntax void value and
// results in no iteration and a SyntaxVoid tail.
//
// The callback is invoked for each element with its index and a boolean
// indicating whether there is another element after the current one.
// If the callback returns an error, iteration stops immediately and the
// error is returned.
func (p *SyntaxVector) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	if p.IsVoid() {
		return SyntaxVoid, nil
	}
	for i, v := range p.Values {
		hasNext := i+1 < len(p.Values)
		err := fn(ctx, i, hasNext, v)
		if err != nil {
			return nil, err
		}
	}
	// Vectors do not have a list tail, so we return SyntaxEmptyList as a
	// conventional "no remainder" sentinel for callers that expect a tail
	// value similar to SyntaxPair.SyntaxForEach.
	return SyntaxEmptyList, nil
}
