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

	"github.com/aalpar/wile/pkg/werr"
)

// emptyListType is the dedicated type for the empty list ().
// It implements Tuple but is not *Pair, enforcing (pair? '()) -> #f
// at the type level per R7RS 6.4.
//
// R7RS 6.4: "The empty list is a special object of its own type.
// It is not a pair."
//
// It also implements SyntaxValue and SyntaxTuple. The empty list carries
// no symbols, no scopes, and no source-attachable hygiene content, so
// the value-level singleton serves double duty as the syntax-level
// singleton (matching Chez's `(equal? (syntax ()) '()) → #t`).
type emptyListType struct{}

var (
	_ Value       = emptyListType{}
	_ Tuple       = emptyListType{}
	_ SyntaxValue = emptyListType{}
	_ SyntaxTuple = emptyListType{}
)

// SchemeString returns "()" for the empty list.
func (emptyListType) SchemeString() string {
	return "()"
}

// IsVoid returns false. The empty list is a valid first-class value, not void.
func (emptyListType) IsVoid() bool {
	return false
}

// EqualTo returns true if the other value is also the empty list.
func (emptyListType) EqualTo(v Value) bool {
	return IsEmptyList(v)
}

// Length returns 0.
func (emptyListType) Length() int {
	return 0
}

// ForEach is a no-op on the empty list; returns the empty list and nil error.
func (p emptyListType) ForEach(_ context.Context, _ ForEachFunc) (Value, error) {
	return p, nil
}

// IsEmptyList returns true.
func (emptyListType) IsEmptyList() bool {
	return true
}

// IsList returns true. The empty list is a proper list.
func (emptyListType) IsList() bool {
	return true
}

// AsVector returns a new empty vector.
func (emptyListType) AsVector() *Vector {
	return NewVector()
}

// Car panics with werr.ErrNotAPair. R7RS: (car '()) is an error.
func (emptyListType) Car() Value {
	panic(werr.WrapForeignErrorf(werr.ErrNotAPair, "emptyList.Car: empty list has no car"))
}

// Cdr panics with werr.ErrNotAPair. R7RS: (cdr '()) is an error.
func (emptyListType) Cdr() Value {
	panic(werr.WrapForeignErrorf(werr.ErrNotAPair, "emptyList.Cdr: empty list has no cdr"))
}

// SourceContext returns nil. The empty list carries no source-attachable
// hygiene content, so it is the same singleton at value and syntax phases.
func (emptyListType) SourceContext() *SourceContext {
	return nil
}

// Unwrap returns the empty list itself.
func (p emptyListType) Unwrap() Value {
	return p
}

// UnwrapAll returns the empty list itself.
func (p emptyListType) UnwrapAll() Value {
	return p
}

// SyntaxCar panics with werr.ErrNotAPair: the empty list has no car (the
// syntax-phase analogue of R7RS (car '()) being an error).
func (emptyListType) SyntaxCar() SyntaxValue {
	panic(werr.WrapForeignErrorf(werr.ErrNotAPair, "emptyList.SyntaxCar: empty list has no car"))
}

// SyntaxCdr panics with werr.ErrNotAPair: the empty list has no cdr (the
// syntax-phase analogue of R7RS (cdr '()) being an error).
func (emptyListType) SyntaxCdr() SyntaxValue {
	panic(werr.WrapForeignErrorf(werr.ErrNotAPair, "emptyList.SyntaxCdr: empty list has no cdr"))
}

// SyntaxForEach is a no-op on the empty list; returns the empty list and nil error.
func (p emptyListType) SyntaxForEach(_ context.Context, _ SyntaxForEachFunc) (SyntaxValue, error) {
	return p, nil
}

// SyntaxAppend returns vs unchanged, since appending to the empty list yields vs.
func (emptyListType) SyntaxAppend(vs SyntaxValue) SyntaxValue {
	return vs
}

// AsSyntaxVector returns a new empty syntax vector.
func (emptyListType) AsSyntaxVector() *SyntaxVector {
	return NewSyntaxVector(nil)
}
