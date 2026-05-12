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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	_ values.Value = (*SyntaxObject)(nil)
	_ SyntaxValue  = (*SyntaxObject)(nil)
)

// Scope is the macro-hygiene identity marker. Defined in package values
// alongside SourceContext (the empty-list duality merge).
type Scope = values.Scope

// NewScope creates a new scope with unique identity for hygiene tracking.
func NewScope() *Scope {
	return values.NewScope()
}

// NewScopeWithLabel creates a new scope with a human-readable label for debugging.
func NewScopeWithLabel(label string) *Scope {
	return values.NewScopeWithLabel(label)
}

// NewRebindingScope creates a new scope that can potentially rebind auxiliary syntax.
func NewRebindingScope() *Scope {
	return values.NewRebindingScope()
}

// NewRebindingScopeWithLabel creates a new rebinding scope with a label.
func NewRebindingScopeWithLabel(label string) *Scope {
	return values.NewRebindingScopeWithLabel(label)
}

// (Scope.ID and Scope.String are defined on values.Scope — see values/scope.go.)

// syntaxBase is the common SourceContext-carrying base for concrete syntax
// types. Defined in package values (the empty-list duality merge); the
// alias keeps existing struct-embedding sites and field names working.
//
// Construct via values.NewSyntaxBase(sc) — the underlying field is
// unexported in values.
type syntaxBase = values.SyntaxBase

// SyntaxObject wraps a non-compound Scheme value with source context.
type SyntaxObject struct {
	datum values.Value
	syntaxBase
}

// NewSyntaxObject creates a new SyntaxObject wrapping the given value and source context.
// It panics if the value is already a syntax value to prevent double-wrapping.
func NewSyntaxObject(v values.Value, sctx *SourceContext) *SyntaxObject {
	switch v.(type) {
	case *SyntaxObject, *SyntaxVector, *SyntaxPair, *SyntaxSymbol: // prevent double-wrapping
		panic(werr.WrapForeignErrorf(
			werr.ErrCannotDoubleSyntaxWrap,
			"cannot wrap a %T in another SyntaxObject",
			v,
		))
	case *values.Vector, *values.Pair, *values.Symbol: // special types for these - SyntaxVector, SyntaxPair, SyntaxSymbol
		panic(werr.WrapForeignErrorf(
			werr.ErrCannotDoubleSyntaxWrap,
			"cannot wrap a %T in another SyntaxObject",
			v,
		))
	}
	q := &SyntaxObject{
		datum:      v,
		syntaxBase: values.NewSyntaxBase(sctx),
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

// IsPair and IsEmptyList are deliberately not implemented on SyntaxObject.
//
// NewSyntaxObject's type switch rejects *values.Pair, *values.Vector, and
// *values.Symbol at construction — pairs flow through *SyntaxPair, vectors
// through *SyntaxVector, symbols through *SyntaxSymbol. The empty list is
// not in NewSyntaxObject's rejection list, but by convention the empty list
// at the syntax phase is constructed as SyntaxEmptyList (which aliases
// values.EmptyList), not wrapped in a SyntaxObject; a grep of the production
// code confirms no SyntaxObject ever wraps the empty list.
//
// Callers that need pair/empty detection should switch on the syntax-level
// type (*SyntaxPair, SyntaxEmptyList) — that is the correct dispatch site.

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
	cached, ok := cache[sv]
	if ok {
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
			_ = vec.Set(i, UnwrapAllShared(elem, cache))
		}
		return vec

	case *SyntaxObject:
		datum, ok := v.Datum().(SyntaxValue)
		if ok {
			result := UnwrapAllShared(datum, cache)
			cache[sv] = result
			return result
		}
		result := v.Unwrap()
		cache[sv] = result
		return result

	case *SyntaxDatumLabelAssignment:
		// Unwrap the labeled value
		datum, ok := v.Value.(SyntaxValue)
		if ok {
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
