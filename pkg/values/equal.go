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

// equalPairKey identifies a pair of compound values being compared.
// Go compares interface values in arrays by type and pointer for pointer types,
// so [2]any{pairA, pairB} works as a map key without unsafe.
type equalPairKey [2]Value

// DeepEqualer is implemented by container values whose equality is defined by
// their components rather than by themselves. Equal owns the traversal; an
// implementor only decides whether the two containers are shaped alike and, if
// so, hands Equal the component pairs to compare.
//
// EqualComponents MUST NOT compare components itself, directly or by calling
// EqualTo on them: that would put the recursion back on the Go stack, which is
// exactly what this interface exists to remove. Everything it can settle
// locally (wrong type, differing length, differing record type) it settles by
// returning false; everything else it pushes.
//
// An implementor MUST be pointer-shaped, and so Go-comparable: Equal keys its
// visited set on (a, b) identity, and hashing a non-comparable dynamic type
// panics. Every implementor in this package is a pointer, and the wider Value
// contract already assumes as much — EqIdentity (utils.go), which backs eq?,
// compares interfaces with == and would fault on a slice- or map-backed Value
// long before equal? saw it.
//
// A recursive value type defined by an embedder that does NOT implement
// DeepEqualer is compared through its own EqualTo, and a cycle in it will
// overflow the host stack. See docs/extensions/architecture.md.
type DeepEqualer interface {
	EqualComponents(other Value, push func(a, b Value)) bool
}

// equalWorklist carries the explicit traversal state that Equal uses in place
// of the Go call stack.
//
// visited implements the coinductive hypothesis (Chez, Racket): a
// (pointer-a, pointer-b) pair re-encountered mid-traversal is assumed equal,
// which is what terminates on cyclic structure per R7RS §6.1. It is allocated
// lazily, so comparing two leaves costs nothing.
type equalWorklist struct {
	pending []equalPairKey
	visited map[equalPairKey]bool
}

// push enqueues a component pair for comparison. It is handed to
// DeepEqualer.EqualComponents as the sole means of descending.
func (p *equalWorklist) push(a, b Value) {
	p.pending = append(p.pending, equalPairKey{a, b})
}

// Equal reports whether two values are structurally equal (R7RS equal?).
//
// The traversal is iterative: containers decompose via DeepEqualer and their
// components are compared from a heap worklist, so no input can overflow the Go
// stack. Equal is total — it terminates on every value, cyclic or not, and has
// neither an error return nor a depth bound.
//
// Push order is load-bearing. Pair pushes cdr before car, and the worklist pops
// last-in first, so a car's subtree drains before the traversal walks on down
// the spine. A flat list therefore holds one pending entry at a time rather
// than one per element.
func Equal(a, b Value) bool {
	if IsVoid(a) || IsVoid(b) {
		return IsVoid(a) == IsVoid(b)
	}
	_, ok := a.(DeepEqualer)
	if !ok {
		// Leaf: no traversal, no allocation. This is the hot path under
		// member/assoc, which compare a key against list elements.
		return a.EqualTo(b)
	}
	w := equalWorklist{}
	for {
		if !w.step(a, b) {
			return false
		}
		n := len(w.pending)
		if n == 0 {
			return true
		}
		n--
		a, b = w.pending[n][0], w.pending[n][1]
		w.pending = w.pending[:n]
	}
}

// step compares one pair, decomposing it onto the worklist if it is a
// container and settling it outright if it is a leaf.
func (p *equalWorklist) step(a, b Value) bool {
	// IsVoid, not a == nil, is the void test: a nil Value interface and a
	// typed-nil pointer (a *Pair cdr left nil rather than EmptyList) are both
	// void and must compare equal to each other. Comparing the interfaces with
	// == would call them different.
	if IsVoid(a) || IsVoid(b) {
		return IsVoid(a) == IsVoid(b)
	}
	if a == b {
		return true
	}
	da, ok := a.(DeepEqualer)
	if !ok {
		return a.EqualTo(b)
	}
	// Settle a non-container b BEFORE it reaches the visited map. Keying on
	// equalPairKey{a, b} hashes both elements, and hashing a non-comparable
	// dynamic type panics — a strictly stronger demand than the a == b above,
	// which only panics when the two types are identical. An embedder's leaf
	// value (a slice- or map-backed Value) therefore cannot be keyed even
	// though comparing it is harmless.
	//
	// This costs nothing: every EqualComponents implementation opens by
	// asserting the other side to its own concrete pointer type and returns
	// false otherwise, so a b that is not itself a container was always going
	// to settle as false. Once past this, both sides are containers, and every
	// DeepEqualer is pointer-shaped and so hashable.
	_, ok = b.(DeepEqualer)
	if !ok {
		return false
	}
	key := equalPairKey{a, b}
	if p.visited[key] {
		return true
	}
	if p.visited == nil {
		p.visited = make(map[equalPairKey]bool)
	}
	p.visited[key] = true
	return da.EqualComponents(b, p.push)
}
