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

import (
	"context"
	"strings"
)

var (
	_ Value       = (*SyntaxVector)(nil)
	_ SyntaxValue = (*SyntaxVector)(nil)
)

// SyntaxVector wraps a Scheme vector with source context.
//
// The pair-side recursive scope-propagation logic for SyntaxVector lives in
// the internal/syntax package alongside the other concrete syntax data
// types. Only the data type itself is here so the SyntaxTuple interface
// (which references *SyntaxVector via AsSyntaxVector) can also live in
// values.
type SyntaxVector struct {
	Values        []SyntaxValue
	sourceContext *SourceContext
}

// NewSyntaxVector creates a new syntax vector with the given source context and elements.
func NewSyntaxVector(sc *SourceContext, vs ...SyntaxValue) *SyntaxVector {
	q := &SyntaxVector{
		Values:        vs,
		sourceContext: sc,
	}
	return q
}

// SourceContext returns the source context.
func (p *SyntaxVector) SourceContext() *SourceContext {
	if p == nil {
		return nil
	}
	return p.sourceContext
}

// UnwrapAllSharedFunc is the cycle-aware recursive unwrapper.
//
// The full recursive unwrap traverses concrete syntax types (SyntaxPair,
// SyntaxObject, SyntaxSymbol, etc.) defined in internal/syntax. Since
// values cannot import internal/syntax (layering), the syntax package
// registers its UnwrapAllShared implementation here at init time.
//
// If the hook is unset (e.g., when code in values constructs a
// SyntaxVector without ever importing internal/syntax), UnwrapAll falls
// back to a shallow per-element walk that lacks cross-type cycle
// detection. This is acceptable for the limited paths where it occurs.
var UnwrapAllSharedFunc func(SyntaxValue, map[SyntaxValue]Value) Value

// SyntaxVoidSingleton is the syntax-level void value, set by
// internal/syntax at init time. SyntaxVector.SyntaxForEach with a nil
// receiver returns this value to preserve the original "syntax void
// tail" behavior. If unset, it falls back to SyntaxEmptyList — which is
// only reachable in tests that import values/ without internal/syntax/.
var SyntaxVoidSingleton SyntaxValue

// SyntaxVectorAddScopeFunc implements recursive scope propagation across
// nested syntax types. Like UnwrapAllSharedFunc, the implementation lives
// in internal/syntax (where the concrete syntax types are) and is
// registered here at init time. The fallback (shallow no-op) is wrong
// for hygiene but is never reached in production where internal/syntax
// is always imported transitively before macro expansion runs.
var SyntaxVectorAddScopeFunc func(*SyntaxVector, *Scope) SyntaxValue

// AddScope recursively propagates a scope to all nested syntax values.
//
// Implements scope propagation for Flatt's "sets of scopes" hygiene.
// When a macro expands, the intro scope must be added to all identifiers
// (symbols) in the expansion. Empty vectors return self unchanged.
func (p *SyntaxVector) AddScope(scope *Scope) SyntaxValue {
	if p == nil || len(p.Values) == 0 {
		return p
	}
	if SyntaxVectorAddScopeFunc != nil {
		return SyntaxVectorAddScopeFunc(p, scope)
	}
	return p
}

// UnwrapAll recursively unwraps all elements to produce a plain values.Vector.
// Uses the syntax package's cycle-aware recursive unwrap when registered.
func (p *SyntaxVector) UnwrapAll() Value {
	if UnwrapAllSharedFunc != nil {
		return UnwrapAllSharedFunc(p, make(map[SyntaxValue]Value))
	}
	if p.IsVoid() {
		return Void
	}
	vec := NewVectorWithLength(len(p.Values))
	for i, elem := range p.Values {
		if elem != nil {
			_ = vec.Set(i, elem.UnwrapAll())
		}
	}
	return vec
}

func (p *SyntaxVector) Unwrap() Value {
	if p.IsVoid() {
		return Void
	}
	vq := make([]Value, len(p.Values))
	for i, v := range p.Values {
		vq[i] = v
	}
	q := NewVector(vq...)
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
func (p *SyntaxVector) EqualTo(o Value) bool {
	v, ok := o.(*SyntaxVector)
	if !ok {
		return false
	}
	return p == v
}

// ForEach iterates over the elements of the vector as regular values in index order.
// It provides tuple-style iteration compatible with values.ForEachFunc callbacks.
func (p *SyntaxVector) ForEach(ctx context.Context, fn ForEachFunc) (Value, error) {
	if p.IsVoid() {
		return Void, nil
	}
	for i, v := range p.Values {
		hasNext := i+1 < len(p.Values)
		err := fn(ctx, i, hasNext, v)
		if err != nil {
			return nil, err
		}
	}
	return EmptyList, nil
}

// SyntaxForEach iterates over the syntax elements of the vector.
// A nil receiver returns the empty-list singleton (which is also the
// SyntaxEmptyList) and performs no iteration.
//
// The callback is invoked for each element with its index and a boolean
// indicating whether there is another element after the current one.
// If the callback returns an error, iteration stops immediately and the
// error is returned.
func (p *SyntaxVector) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	if p.IsVoid() {
		if SyntaxVoidSingleton != nil {
			return SyntaxVoidSingleton, nil
		}
		return SyntaxEmptyList, nil
	}
	for i, v := range p.Values {
		hasNext := i+1 < len(p.Values)
		err := fn(ctx, i, hasNext, v)
		if err != nil {
			return nil, err
		}
	}
	// Vectors do not have a list tail, so we return the empty-list singleton
	// as a conventional "no remainder" sentinel for callers that expect a
	// tail value similar to SyntaxPair.SyntaxForEach.
	return SyntaxEmptyList, nil
}
