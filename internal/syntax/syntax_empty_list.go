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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// syntaxEmptyListType is the dedicated type for the syntax empty list ().
// It implements SyntaxTuple but is not *SyntaxPair, enforcing type safety
// parallel to values.emptyListType.
//
// This is a pointer singleton — SyntaxEmptyList is the only instance.
// Empty lists carry no scopes and no source context. AddScope returns
// the singleton unchanged because there are no symbols to propagate to.
type syntaxEmptyListType struct{}

var (
	_ values.Value = (*syntaxEmptyListType)(nil)
	_ values.Tuple = (*syntaxEmptyListType)(nil)
	_ SyntaxValue  = (*syntaxEmptyListType)(nil)
	_ SyntaxTuple  = (*syntaxEmptyListType)(nil)
)

// SchemeString returns "#'()" for the syntax empty list, matching other SyntaxValue implementations.
func (*syntaxEmptyListType) SchemeString() string {
	return "#'()"
}

// IsVoid returns false. The empty list is a valid first-class value, not void.
func (*syntaxEmptyListType) IsVoid() bool {
	return false
}

// EqualTo performs type comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same type of syntax object.
// For value comparison of syntax objects, use syntax->datum first.
// R7RS: (equal? (syntax ()) '()) => #f
func (*syntaxEmptyListType) EqualTo(v values.Value) bool {
	_, ok := v.(*syntaxEmptyListType)
	return ok
}

// Length returns 0.
func (*syntaxEmptyListType) Length() int {
	return 0
}

// Append returns vs unchanged, since appending to the empty list yields vs.
func (*syntaxEmptyListType) Append(vs values.Value) values.Value {
	return vs
}

// ForEach is a no-op on the empty list; returns the empty list and nil error.
func (p *syntaxEmptyListType) ForEach(_ context.Context, _ values.ForEachFunc) (values.Value, error) {
	return p, nil
}

// IsEmptyList returns true.
func (*syntaxEmptyListType) IsEmptyList() bool {
	return true
}

// IsList returns true. The empty list is a proper list.
func (*syntaxEmptyListType) IsList() bool {
	return true
}

// AsVector returns a new empty vector.
func (*syntaxEmptyListType) AsVector() *values.Vector {
	return values.NewVector()
}

// Car panics with ErrNotAPair. R7RS: (car '()) is an error.
func (*syntaxEmptyListType) Car() values.Value {
	panic(werr.ErrNotAPair)
}

// Cdr panics with ErrNotAPair. R7RS: (cdr '()) is an error.
func (*syntaxEmptyListType) Cdr() values.Value {
	panic(werr.ErrNotAPair)
}

// SyntaxCar panics with ErrNotAPair.
func (*syntaxEmptyListType) SyntaxCar() SyntaxValue {
	panic(werr.ErrNotAPair)
}

// SyntaxCdr panics with ErrNotAPair.
func (*syntaxEmptyListType) SyntaxCdr() SyntaxValue {
	panic(werr.ErrNotAPair)
}

// SyntaxForEach is a no-op on the empty list.
func (p *syntaxEmptyListType) SyntaxForEach(_ context.Context, _ SyntaxForEachFunc) (SyntaxValue, error) {
	return p, nil
}

// SyntaxAppend returns vs unchanged.
func (*syntaxEmptyListType) SyntaxAppend(vs SyntaxValue) SyntaxValue {
	return vs
}

// AsSyntaxVector returns a new empty syntax vector.
func (*syntaxEmptyListType) AsSyntaxVector() *SyntaxVector {
	return NewSyntaxVector(nil)
}

// SourceContext returns nil. The singleton empty list carries no source context.
func (*syntaxEmptyListType) SourceContext() *SourceContext {
	return nil
}

// Unwrap returns the empty values.Value list.
func (*syntaxEmptyListType) Unwrap() values.Value {
	return values.EmptyList
}

// UnwrapAll returns the empty values.Value list.
func (*syntaxEmptyListType) UnwrapAll() values.Value {
	return values.EmptyList
}

// AddScope returns the singleton unchanged.
// Empty lists have no symbols to propagate scopes to.
func (p *syntaxEmptyListType) AddScope(_ *Scope) SyntaxValue {
	return p
}
