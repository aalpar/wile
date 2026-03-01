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

	"github.com/aalpar/wile/werr"
)

// emptyListType is the dedicated type for the empty list ().
// It implements Tuple but is not *Pair, enforcing (pair? '()) -> #f
// at the type level per R7RS 6.4.
//
// R7RS 6.4: "The empty list is a special object of its own type.
// It is not a pair."
type emptyListType struct{}

var (
	_ Value = emptyListType{}
	_ Tuple = emptyListType{}
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

// Append returns vs unchanged, since appending to the empty list yields vs.
func (emptyListType) Append(vs Value) Value {
	return vs
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
	panic(werr.ErrNotAPair)
}

// Cdr panics with werr.ErrNotAPair. R7RS: (cdr '()) is an error.
func (emptyListType) Cdr() Value {
	panic(werr.ErrNotAPair)
}
