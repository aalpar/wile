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
	"fmt"

	"github.com/aalpar/wile/values"
)

var (
	_ values.Value = (*SyntaxObject)(nil)
	_ SyntaxValue  = (*SyntaxObject)(nil)
)

// Scope is an identity marker for macro hygiene.
// Each macro invocation creates a fresh Scope. Hygiene checking uses pointer
// equality to determine if a binding's scopes are a subset of a reference's scopes.
// This implements Flatt's "sets of scopes" model where scopes are just unique tags,
// not environment hierarchies.
type Scope struct {
	id uint64 // ensures unique pointer identity (empty structs can share addresses in Go)
	// IsRebinding indicates whether this scope can potentially rebind auxiliary syntax.
	// True for let-syntax/letrec-syntax scopes which create local macro bindings.
	// False for with-binding-scope which only adds scopes for binding hygiene.
	// This distinction is used in literalScopesMatch to correctly handle auxiliary
	// syntax like => and else in cond/case.
	IsRebinding bool
}

// nextScopeID is a counter for generating unique scope identities
var nextScopeID uint64

// NewScope creates a new scope with unique identity for hygiene tracking.
// By default, scopes are not rebinding scopes.
func NewScope() *Scope {
	nextScopeID++
	return &Scope{id: nextScopeID, IsRebinding: false}
}

// NewRebindingScope creates a new scope that can potentially rebind auxiliary syntax.
// Used by let-syntax and letrec-syntax to mark scopes that could shadow literals.
func NewRebindingScope() *Scope {
	nextScopeID++
	return &Scope{id: nextScopeID, IsRebinding: true}
}

// ID returns the unique identifier for this scope.
// This can be used as a macro application ID for tracing.
func (p *Scope) ID() uint64 {
	if p == nil {
		return 0
	}
	return p.id
}

// NewSyntaxNil creates a syntax empty list.
//
// Deprecated: Use NewSyntaxEmptyList instead.
//
// This function exists for backward compatibility but delegates to NewSyntaxEmptyList.
func NewSyntaxNil(sctx *SourceContext) *SyntaxPair {
	return NewSyntaxEmptyList(sctx)
}

// SyntaxObject wraps a non-compound Scheme value with source context.
type SyntaxObject struct {
	datum         values.Value
	sourceContext *SourceContext
}

// NewSyntaxObject creates a new SyntaxObject wrapping the given value and source context.
// It panics if the value is already a syntax value to prevent double-wrapping.
func NewSyntaxObject(v values.Value, sctx *SourceContext) *SyntaxObject {
	switch v.(type) {
	case *SyntaxObject, *SyntaxVector, *SyntaxPair, *SyntaxSymbol: // prevent double-wrapping
		panic(values.NewForeignErrorf("cannot wrap a %T in another SyntaxObject", v))
	case *values.Vector, *values.Pair, *values.Symbol: // special types for these - SyntaxVector, SyntaxPair, SyntaxSymbol
		panic(values.NewForeignErrorf("cannot wrap a %T in another SyntaxObject", v))
	}
	q := &SyntaxObject{
		datum:         v,
		sourceContext: sctx,
	}
	return q
}

// Datum returns the underlying datum of the syntax object.
func (p *SyntaxObject) Datum() values.Value {
	return p.datum
}

// UnwrapAll recursively unwraps all syntax wrappers and returns the underlying value.
func (p *SyntaxObject) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

func (p *SyntaxObject) Unwrap() values.Value {
	return p.datum
}

// IsPair returns true if the wrapped datum is a pair.
func (p *SyntaxObject) IsPair() bool {
	_, ok := p.Datum().(*values.Pair)
	return ok
}

// IsEmptyList returns true if the wrapped datum is the empty list.
func (p *SyntaxObject) IsEmptyList() bool {
	return values.IsEmptyList(p.Datum())
}

// SourceContext returns the source context of the syntax object.
func (p *SyntaxObject) SourceContext() *SourceContext {
	return p.sourceContext
}

// IsVoid returns true if the syntax object is nil.
func (p *SyntaxObject) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme representation of the syntax object.
func (p *SyntaxObject) SchemeString() string {
	return fmt.Sprintf("#'%s", p.Datum().SchemeString())
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxObject) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxObject)
	if !ok {
		return false
	}
	return p == other
}

// UnwrapAllShared recursively unwraps a syntax value while preserving object identity.
// This is essential for datum labels (R7RS §2.4) where #n# must refer to the exact same
// object as #n=. The cache parameter tracks already-unwrapped syntax values to ensure
// the same SyntaxValue always unwraps to the same values.Value.
// This also handles circular structures by pre-registering placeholders before recursing.
func UnwrapAllShared(sv SyntaxValue, cache map[SyntaxValue]values.Value) values.Value {
	if sv == nil {
		return values.Void
	}
	// Check if we've already unwrapped this syntax value
	if cached, ok := cache[sv]; ok {
		return cached
	}

	switch v := sv.(type) {
	case *SyntaxPair:
		if v.IsVoid() {
			return values.Void
		}
		if v.IsEmptyList() {
			return values.EmptyList
		}
		// Pre-register a placeholder pair to handle circular references
		placeholder := values.NewCons(nil, nil)
		cache[sv] = placeholder
		// Now recursively unwrap car and cdr
		var car, cdr values.Value
		if v.Values[0] != nil {
			car = UnwrapAllShared(v.Values[0], cache)
		}
		if v.Values[1] != nil {
			cdr = UnwrapAllShared(v.Values[1], cache)
		} else {
			cdr = values.EmptyList
		}
		placeholder.SetCar(car)
		placeholder.SetCdr(cdr)
		return placeholder

	case *SyntaxVector:
		if v.IsVoid() {
			return values.Void
		}
		// Pre-register placeholder vector
		vec := values.NewVectorWithLength(len(v.Values))
		cache[sv] = vec
		// Recursively unwrap elements
		for i, elem := range v.Values {
			vec.Set(i, UnwrapAllShared(elem, cache))
		}
		return vec

	case *SyntaxObject:
		if datum, ok := v.Datum().(SyntaxValue); ok {
			result := UnwrapAllShared(datum, cache)
			cache[sv] = result
			return result
		}
		result := v.Unwrap()
		cache[sv] = result
		return result

	case *SyntaxDatumLabelAssignment:
		// Unwrap the labeled value
		if datum, ok := v.Value.(SyntaxValue); ok {
			result := UnwrapAllShared(datum, cache)
			cache[sv] = result
			return result
		}
		cache[sv] = v.Value
		return v.Value

	case *SyntaxDatumLabel:
		// This should not normally happen if the parser resolved the label
		result := values.NewInteger(int64(v.Label))
		cache[sv] = result
		return result

	case *SyntaxSymbol:
		result := v.Unwrap()
		cache[sv] = result
		return result

	case *SyntaxComment:
		result := values.NewString(v.Text)
		cache[sv] = result
		return result

	case *SyntaxDirective:
		result := values.NewString(v.Name)
		cache[sv] = result
		return result

	case *SyntaxDatumComment:
		// Recursively unwrap the commented value
		result := UnwrapAllShared(v.Value, cache)
		cache[sv] = result
		return result

	default:
		// All syntax types should be handled above
		result := sv.Unwrap()
		cache[sv] = result
		return result
	}
}
