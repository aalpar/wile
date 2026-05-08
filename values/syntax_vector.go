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

	"github.com/aalpar/wile/werr"
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
	Values []SyntaxValue
	SyntaxBase
}

// NewSyntaxVector creates a new syntax vector with the given source context and elements.
func NewSyntaxVector(sc *SourceContext, vs ...SyntaxValue) *SyntaxVector {
	q := &SyntaxVector{
		Values:     vs,
		SyntaxBase: NewSyntaxBase(sc),
	}
	return q
}

// SyntaxValueUnwrapAllFunc is the cycle-aware recursive unwrapper.
//
// The full recursive unwrap traverses concrete syntax types (SyntaxPair,
// SyntaxObject, SyntaxSymbol, etc.) defined in internal/syntax. Since
// values cannot import internal/syntax (layering), the syntax package
// registers its UnwrapAllShared implementation here at init time.
//
// MUST be non-nil before any SyntaxVector method that depends on it is
// called. internal/syntax/syntax_vector.go init() sets this. A nil hook
// at call time indicates an init-order or import-graph violation; the
// methods that depend on it panic rather than silently degrade — silent
// degradation would corrupt hygiene or stack-overflow on cyclic data.
var SyntaxValueUnwrapAllFunc func(SyntaxValue, map[SyntaxValue]Value) Value

// SyntaxVectorVoidValue is the syntax-level void value, set by
// internal/syntax at init time. SyntaxVector.SyntaxForEach with a nil
// receiver returns this value to preserve the original "syntax void
// tail" semantics (distinguishable from the empty-list tail).
//
// MUST be non-nil. See SyntaxValueUnwrapAllFunc for the rationale.
var SyntaxVectorVoidValue SyntaxValue

// SyntaxVectorAddScopeFunc implements recursive scope propagation across
// nested syntax types. The implementation lives in internal/syntax
// (where the concrete syntax types are) and is registered here at init
// time.
//
// MUST be non-nil. See SyntaxValueUnwrapAllFunc for the rationale.
var SyntaxVectorAddScopeFunc func(*SyntaxVector, *Scope) SyntaxValue

// AddScope recursively propagates a scope to all nested syntax values.
//
// Implements scope propagation for Flatt's "sets of scopes" hygiene.
// When a macro expands, the intro scope must be added to all identifiers
// (symbols) in the expansion. Empty vectors return self unchanged.
//
// Panics if SyntaxVectorAddScopeFunc is nil — see the var declaration
// for why a silent fallback would be unsafe.
func (p *SyntaxVector) AddScope(scope *Scope) SyntaxValue {
	if p == nil || len(p.Values) == 0 {
		return p
	}
	if SyntaxVectorAddScopeFunc == nil {
		panic(werr.WrapForeignErrorf(werr.ErrInternal,
			"SyntaxVector.AddScope: SyntaxVectorAddScopeFunc not registered (internal/syntax must be imported before macro expansion)"))
	}
	return SyntaxVectorAddScopeFunc(p, scope)
}

// UnwrapAll recursively unwraps all elements to produce a plain values.Vector,
// using the syntax package's cycle-aware recursive unwrap.
//
// Panics if SyntaxValueUnwrapAllFunc is nil — see the var declaration
// for why a silent fallback would be unsafe (cyclic syntax structures
// would stack-overflow on a non-cycle-aware fallback walk).
func (p *SyntaxVector) UnwrapAll() Value {
	if SyntaxValueUnwrapAllFunc == nil {
		panic(werr.WrapForeignErrorf(werr.ErrInternal,
			"SyntaxVector.UnwrapAll: SyntaxValueUnwrapAllFunc not registered (internal/syntax must be imported before unwrapping syntax values)"))
	}
	return SyntaxValueUnwrapAllFunc(p, make(map[SyntaxValue]Value))
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
// A nil receiver returns the syntax-void singleton (semantically
// distinct from the empty-list tail returned for an iterated vector).
//
// The callback is invoked for each element with its index and a boolean
// indicating whether there is another element after the current one.
// If the callback returns an error, iteration stops immediately and the
// error is returned.
//
// Panics if SyntaxVectorVoidValue is nil — see the var declaration
// for why a silent fallback would be unsafe (returning the empty list
// instead of void changes the semantics callers branch on).
func (p *SyntaxVector) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	if p.IsVoid() {
		if SyntaxVectorVoidValue == nil {
			panic(werr.WrapForeignErrorf(werr.ErrInternal,
				"SyntaxVector.SyntaxForEach: SyntaxVectorVoidValue not registered (internal/syntax must be imported)"))
		}
		return SyntaxVectorVoidValue, nil
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
