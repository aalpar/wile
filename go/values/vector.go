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

// Package values provides Scheme runtime value types.
package values

import (
	"fmt"
	"strings"
)

var (
	_ Value      = (*Vector)(nil)
	_ Collection = (*Vector)(nil)
)

// Vector represents an R7RS vector, a fixed-size mutable array of values.
// Vectors are written as #(element ...) in Scheme syntax.
// Unlike lists, vectors provide O(1) access to elements by index.
type Vector []Value

// NewVector creates a new Vector from the given values.
// Returns an empty vector if no arguments are provided.
func NewVector(vs ...Value) *Vector {
	q := (*Vector)(&vs)
	return q
}

// Datum returns the underlying slice of values.
// Returns nil if the vector is void (nil pointer).
func (p *Vector) Datum() []Value {
	if p.IsVoid() {
		return nil
	}
	return *p
}

// IsVoid returns true if the vector is a nil pointer.
// A nil vector represents the absence of a value, distinct from an empty vector.
func (p *Vector) IsVoid() bool {
	return p == nil
}

// EqualTo implements structural equality for vectors.
// Two vectors are equal if they have the same length and all corresponding
// elements are equal (using recursive EqualTo comparison).
// Returns false if the other value is not a Vector.
func (p *Vector) EqualTo(v Value) bool {
	other, ok := v.(*Vector)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	if len(*p) != len(*other) {
		return false
	}
	for i := range *p {
		if !EqualTo((*p)[i], (*other)[i]) {
			return false
		}
	}
	return true
}

// AsList converts the vector to a proper list (linked list of pairs).
// Returns void (nil Pair) if the vector is void.
// Returns EmptyList if the vector is empty.
// Otherwise returns a newly constructed list containing the vector's elements.
func (p *Vector) AsList() Tuple {
	if p.IsVoid() {
		return (*Pair)(nil)
	}
	l := len(*p)
	switch l {
	case 0:
		return EmptyList
	case 1:
		return &Pair{(*p)[0], EmptyList}
	}
	q := &Pair{(*p)[0], &Pair{}}
	curr := q
	for _, v := range (*p)[1:] {
		curr = curr[1].(*Pair)
		curr[0] = v
		curr[1] = &Pair{}
	}
	curr[1] = EmptyList
	return q
}

// SchemeString returns the Scheme external representation of the vector.
// Format: #( element1 element2 ... ) with elements separated by spaces.
// Empty vectors are represented as #().
func (p *Vector) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("#(")
	if len(*p) > 0 {
		q.WriteString(" ")
		v := (*p)[0]
		q.WriteString(fmt.Sprintf("%s", v.SchemeString()))
		for _, v = range (*p)[1:] {
			q.WriteString(" ")
			q.WriteString(fmt.Sprintf("%s", v.SchemeString()))
		}
		q.WriteString(" ")
	}
	q.WriteString(")")
	return q.String()
}
