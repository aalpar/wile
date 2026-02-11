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
)

// syntaxEmptyListType is the dedicated type for the syntax empty list ().
// It implements SyntaxTuple but is not *SyntaxPair, enforcing type safety
// parallel to values.emptyListType.
type syntaxEmptyListType struct {
	sourceContext *SourceContext
}

var (
	_ values.Value = syntaxEmptyListType{}
	_ values.Tuple = syntaxEmptyListType{}
	_ SyntaxValue  = syntaxEmptyListType{}
	_ SyntaxTuple  = syntaxEmptyListType{}
)

// SchemeString returns "()" for the empty list.
func (syntaxEmptyListType) SchemeString() string {
	return "()"
}

// IsVoid returns false. The empty list is a valid first-class value, not void.
func (syntaxEmptyListType) IsVoid() bool {
	return false
}

// EqualTo returns true if the other value is also the empty syntax list.
func (syntaxEmptyListType) EqualTo(v values.Value) bool {
	// Check if v is a SyntaxValue first
	sv, ok := v.(SyntaxValue)
	if ok {
		return IsSyntaxEmptyList(sv)
	}
	// Also check if it's the values.EmptyList
	return values.IsEmptyList(v)
}

// Length returns 0.
func (syntaxEmptyListType) Length() int {
	return 0
}

// Append returns vs unchanged, since appending to the empty list yields vs.
func (syntaxEmptyListType) Append(vs values.Value) values.Value {
	return vs
}

// ForEach is a no-op on the empty list; returns the empty list and nil error.
func (p syntaxEmptyListType) ForEach(_ context.Context, _ values.ForEachFunc) (values.Value, error) {
	return p, nil
}

// IsEmptyList returns true.
func (syntaxEmptyListType) IsEmptyList() bool {
	return true
}

// IsList returns true. The empty list is a proper list.
func (syntaxEmptyListType) IsList() bool {
	return true
}

// AsVector returns a new empty vector.
func (syntaxEmptyListType) AsVector() *values.Vector {
	return values.NewVector()
}

// Car panics with ErrNotAPair. R7RS: (car '()) is an error.
func (syntaxEmptyListType) Car() values.Value {
	panic(values.ErrNotAPair)
}

// Cdr panics with ErrNotAPair. R7RS: (cdr '()) is an error.
func (syntaxEmptyListType) Cdr() values.Value {
	panic(values.ErrNotAPair)
}

// SyntaxCar panics with ErrNotAPair.
func (syntaxEmptyListType) SyntaxCar() SyntaxValue {
	panic(values.ErrNotAPair)
}

// SyntaxCdr panics with ErrNotAPair.
func (syntaxEmptyListType) SyntaxCdr() SyntaxValue {
	panic(values.ErrNotAPair)
}

// SetSyntaxCar panics - empty list is immutable.
func (syntaxEmptyListType) SetSyntaxCar(_ SyntaxValue) {
	panic(values.ErrNotAPair)
}

// SetSyntaxCdr panics - empty list is immutable.
func (syntaxEmptyListType) SetSyntaxCdr(_ SyntaxValue) {
	panic(values.ErrNotAPair)
}

// SyntaxForEach is a no-op on the empty list.
func (p syntaxEmptyListType) SyntaxForEach(_ context.Context, _ SyntaxForEachFunc) (SyntaxValue, error) {
	return p, nil
}

// SyntaxAppend returns vs unchanged.
func (syntaxEmptyListType) SyntaxAppend(vs SyntaxValue) SyntaxValue {
	return vs
}

// AsSyntaxVector returns a new empty syntax vector.
func (p syntaxEmptyListType) AsSyntaxVector() *SyntaxVector {
	return NewSyntaxVector(p.sourceContext)
}

// SourceContext returns the source context.
func (p syntaxEmptyListType) SourceContext() *SourceContext {
	return p.sourceContext
}

// Unwrap returns the empty values.Value list.
func (syntaxEmptyListType) Unwrap() values.Value {
	return values.EmptyList
}

// UnwrapAll returns the empty values.Value list.
func (syntaxEmptyListType) UnwrapAll() values.Value {
	return values.EmptyList
}

// AddScope returns a new empty list with the source context updated.
// Empty lists have no symbols to propagate scopes to, but we preserve
// the scope in the source context for consistency.
func (p syntaxEmptyListType) AddScope(scope *Scope) SyntaxValue {
	if p.sourceContext == nil {
		return syntaxEmptyListType{
			sourceContext: NewSourceContext("", "", SourceIndexes{}, SourceIndexes{}).WithScope(scope),
		}
	}
	return syntaxEmptyListType{
		sourceContext: p.sourceContext.WithScope(scope),
	}
}

// NewSyntaxEmptyListWithContext creates a syntax empty list with the given source context.
func NewSyntaxEmptyListWithContext(sctx *SourceContext) SyntaxValue {
	return syntaxEmptyListType{sourceContext: sctx}
}
