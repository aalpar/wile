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
	"reflect"

	"github.com/aalpar/wile/pkg/values"
)

// SyntaxList constructs a syntax list from the given elements.
// The sc parameter provides a fallback source context for the list container.
// Each intermediate pair uses the source context of its car element when available,
// preserving per-element source location information for better error reporting.
func SyntaxList(sc *SourceContext, os ...SyntaxValue) SyntaxValue {
	l := len(os)
	switch l {
	case 0:
		return SyntaxEmptyList
	case 1:
		// Use element's source context if available, otherwise fall back to sc
		elemSc := sc
		if os[0] != nil {
			esc := os[0].SourceContext()
			if esc != nil {
				elemSc = esc
			}
		}
		return &SyntaxPair{
			Values:     [2]SyntaxValue{os[0], SyntaxEmptyList},
			syntaxBase: values.NewSyntaxBase(elemSc),
		}
	}
	// Use first element's source context for the head pair
	headSc := sc
	if os[0] != nil {
		esc := os[0].SourceContext()
		if esc != nil {
			headSc = esc
		}
	}
	q := &SyntaxPair{
		Values:     [2]SyntaxValue{os[0], &SyntaxPair{}},
		syntaxBase: values.NewSyntaxBase(headSc),
	}
	curr := q
	for _, v := range os[1:] {
		curr = curr.Cdr().(*SyntaxPair)
		curr.SetCar(v)
		curr.SetCdr(&SyntaxPair{})
		// Use element's source context for this pair
		if v != nil {
			esc := v.SourceContext()
			if esc != nil {
				curr.SetSourceContext(esc)
			}
		}
	}
	curr.SetCdr(SyntaxEmptyList)
	return q
}

// SyntaxForEach iterates over a syntax tuple, calling fn for each element.
func SyntaxForEach(ctx context.Context, o SyntaxValue, fn func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error) (SyntaxValue, error) {
	pr, ok := o.(SyntaxTuple)
	if ok {
		return pr.SyntaxForEach(ctx, fn)
	}
	return o, nil
}

// SyntaxWalk iterates over a syntax tuple, calling fn for each element.
// It is a convenience wrapper around SyntaxForEach for callers that only
// need the element value (ignoring the index and hasNext arguments).
func SyntaxWalk(ctx context.Context, o SyntaxValue, fn func(SyntaxValue) error) error {
	_, err := SyntaxForEach(ctx, o, func(_ context.Context, _ int, _ bool, v SyntaxValue) error {
		return fn(v)
	})
	return err
}

// EqualTo compares two syntax values for equality, handling nil and pointer identity.
func EqualTo(a, b SyntaxValue) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Comparable() && bv.Comparable() && a == b {
		return true
	}
	// reflect.Value.IsNil panics on any Kind that cannot be nil, and not every
	// SyntaxValue is a pointer: SyntaxVoid is syntaxVoidType{}, a struct. Comparing
	// it against a different value reaches here. EqualTo is exported, so that panic
	// escaped to Go hosts outside every recover boundary.
	if isNilableKind(av) && isNilableKind(bv) && av.IsNil() && bv.IsNil() {
		return true
	}
	return a.EqualTo(b)
}

// isNilableKind reports whether v's Kind admits a nil value, i.e. whether
// reflect.Value.IsNil may be called on it without panicking.
func isNilableKind(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface,
		reflect.Map, reflect.Pointer, reflect.Slice:
		return true
	default:
		return false
	}
}

// IsSyntaxList returns true if the value is a proper syntax list.
func IsSyntaxList(v SyntaxValue) bool {
	if v == nil {
		return false
	}
	if IsSyntaxEmptyList(v) {
		return true
	}
	pr, ok := v.(*SyntaxPair)
	if ok {
		return pr.IsList()
	}
	return false
}

// IsSyntaxVoid returns true if the value is nil or void.
func IsSyntaxVoid(v SyntaxValue) bool {
	return v == nil || v.IsVoid()
}

// IsSyntaxEmptyList returns true if the value is the syntax empty list.
//
// Delegates to values.IsEmptyList — after the empty-list duality merge,
// the syntax empty list is the same singleton as values.EmptyList, and
// values.IsEmptyList works on any Tuple. This restores symmetric
// equality (previously the strict pointer-type assertion produced
// #f for `(equal? (syntax ()) '())`, contrary to Chez).
func IsSyntaxEmptyList(v SyntaxValue) bool {
	return values.IsEmptyList(v)
}
