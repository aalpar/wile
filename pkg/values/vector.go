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

// Package values provides Scheme runtime value types.
package values

var _ Value = (*Vector)(nil)

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

// NewVectorWithLength creates a new Vector of the given length,
// with all elements initialized to the specified fill value.
func NewVectorWithLength(length int) *Vector {
	slice := make([]Value, length)
	q := Vector(slice)
	return &q
}

// Get returns the element at the specified index.
func (p *Vector) Get(i int) Value {
	return (*p)[i]
}

// Set sets the element at the specified index to the given value.
// Vectors are always mutable, so this never returns an error.
func (p *Vector) Set(i int, value Value) error {
	(*p)[i] = value
	return nil
}

// Length returns the number of elements in the vector.
// Returns 0 if the vector is void (nil pointer).
func (p *Vector) Length() int {
	if p.IsVoid() {
		return 0
	}
	return len(*p)
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
	return List((*p)...)
}

// SchemeString returns the Scheme external representation of the vector.
// Format: #( element1 element2 ... ) with elements separated by spaces.
// Empty vectors are represented as #().
// Cyclic and cross-referential structures render a bounded "..." marker
// instead of overflowing the Go stack.
func (p *Vector) SchemeString() string {
	if p.IsVoid() {
		return "#<void>"
	}
	return p.schemeStringWithVisited(make(map[Value]bool), 1)
}

// schemeStringWithVisited renders the vector using PATH-SCOPED cycle detection:
// the vector marks itself on entry and unmarks on exit, so a vector reached by
// two SIBLING paths (acyclic sharing) is rendered in full at each occurrence;
// only a vector reachable from itself (a true cycle, still on the current path)
// collapses to "...". Nested Pair/Vector children recurse via schemeStringChild
// and apply the same mark/unmark discipline, catching pair↔vector cycles.
//
// depth is this vector's nesting level (root = 1); elements sit one level
// deeper (depth+1), where formatIndexable threads the host-safety bound.
func (p *Vector) schemeStringWithVisited(visited map[Value]bool, depth int) string {
	if visited[p] {
		return "..."
	}
	visited[p] = true
	defer func() {
		delete(visited, p)
	}()
	return formatIndexable("#(", len(*p), func(i int) Value {
		return (*p)[i]
	}, visited, depth+1)
}
