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

import "github.com/aalpar/wile/pkg/werr"

var (
	_ Value     = (*Vector)(nil)
	_ Immutable = (*Vector)(nil)
)

// Vector represents an R7RS vector, a fixed-size mutable array of values.
// Vectors are written as #(element ...) in Scheme syntax.
// Unlike lists, vectors provide O(1) access to elements by index.
//
// A vector carries its own immutability, so a quoted-literal vector answers
// IsImmutable without an engine-scoped side set. The bit is written once, before
// the value is reachable from Scheme (values.MarkImmutable, called from the
// compiler's literal-pool appender), and read on the cold mutation path — so a
// plain bool is the right shape and no synchronization is needed.
//
// The elements are behind a field rather than being the type itself. Callers
// that need the backing slice use Elems.
type Vector struct {
	elems     []Value
	immutable bool
}

// NewVector creates a new Vector from the given values.
// Returns an empty vector if no arguments are provided.
//
// The variadic slice is adopted, not copied — the same aliasing the slice-typed
// predecessor had, so a caller spreading its own slice still shares storage.
func NewVector(vs ...Value) *Vector {
	q := &Vector{elems: vs}
	return q
}

// NewVectorWithLength creates a new Vector of the given length. Every element is
// the nil Value (not Void); the caller is expected to fill all slots before the
// vector escapes.
func NewVectorWithLength(length int) *Vector {
	q := &Vector{elems: make([]Value, length)}
	return q
}

// Elems returns the vector's backing element slice. It is the storage, not a
// copy: writing through it bypasses Set and therefore bypasses the immutability
// check, so a caller holding a vector the program can name must go through Set.
func (p *Vector) Elems() []Value {
	if p.IsVoid() {
		return nil
	}
	return p.elems
}

// Get returns the element at the specified index.
func (p *Vector) Get(i int) Value {
	return p.elems[i]
}

// Set sets the element at the specified index to the given value.
//
// The immutability check lives here rather than only at the primitives, the way
// *String.SetChar does it: a value that owns its own bit is the correct layer to
// enforce it, and a caller reaching Set from a path nobody has gated still gets
// the refusal.
func (p *Vector) Set(i int, value Value) error {
	if p.immutable {
		return werr.WrapForeignErrorf(werr.ErrImmutableVector, "Vector.Set: cannot mutate immutable vector")
	}
	p.elems[i] = value
	return nil
}

// IsImmutable reports whether in-place mutation of this vector is forbidden.
// R7RS §4.1.2: a quoted-literal constant is immutable.
func (p *Vector) IsImmutable() bool {
	return p != nil && p.immutable
}

// setImmutable marks the vector immutable. Unexported because the write must
// happen before the value is reachable from Scheme; values.MarkImmutable is the
// one greppable entry point.
func (p *Vector) setImmutable() {
	if p == nil {
		return
	}
	p.immutable = true
}

// Length returns the number of elements in the vector.
// Returns 0 if the vector is void (nil pointer).
func (p *Vector) Length() int {
	if p.IsVoid() {
		return 0
	}
	return len(p.elems)
}

// IsVoid returns true if the vector is a nil pointer.
// A nil vector represents the absence of a value, distinct from an empty vector.
func (p *Vector) IsVoid() bool {
	return p == nil
}

// EqualTo implements structural equality for vectors.
// Two vectors are equal if they have the same length and all corresponding
// elements are equal. Returns false if the other value is not a Vector.
//
// The comparison is delegated to Equal, whose traversal is ITERATIVE: elements
// are drained from a heap worklist, not by recursive EqualTo calls, so a deeply
// nested or cyclic vector cannot overflow the Go stack. EqualComponents below is
// this type's whole contribution to it.
//
// Immutability is not part of the comparison: R7RS equality is over contents, and
// a literal and a runtime-built vector with the same elements are equal?.
func (p *Vector) EqualTo(v Value) bool {
	return Equal(p, v)
}

// EqualComponents pushes the two vectors' corresponding elements for Equal to
// compare, once the lengths agree.
func (p *Vector) EqualComponents(v Value, push func(a, b Value)) bool {
	other, ok := v.(*Vector)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	if len(p.elems) != len(other.elems) {
		return false
	}
	for i := range p.elems {
		push(p.elems[i], other.elems[i])
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
	return List(p.elems...)
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
	return p.schemeStringWithVisited(NewMapSet[Value](0), 1)
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
func (p *Vector) schemeStringWithVisited(visited MapSet[Value], depth int) string {
	seen := visited.ContainsOne(p)
	if seen {
		return "..."
	}
	visited.Add(p)
	defer func() {
		visited.Remove(p)
	}()
	return formatIndexable("#(", len(p.elems), func(i int) Value {
		return p.elems[i]
	}, visited, depth+1)
}
