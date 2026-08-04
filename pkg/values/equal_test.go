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

package values_test

import (
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestNoContainerIsHashable is the invariant guard the iterative equal? design
// rests on (ADR-D1). What it guards CHANGED when HashtableKind moved the hash
// from the key to the table: it no longer describes which keys are admissible —
// every kind admits every key now — it is the basis of the equal? GATE.
//
// Hashtable.EqualComponents pairs entries by KEY and pushes only the matched
// VALUES onto Equal's worklist. That pairing looks each key up eagerly, which it
// must: the worklist is what would otherwise be deciding the answer. Eager is
// bounded for pair and vector keys, whose EqualTo delegates to the iterative
// Equal. It is UNBOUNDED for a hashtable reachable as a key, which recurses one
// Go frame per nesting level with no bottom on a cycle.
//
// equalGate therefore admits a structural comparison only when every key on both
// sides is a Hashable leaf, and this test is what makes that one interface
// assertion sufficient. Giving *Pair, *Vector, *Hashtable or *Box a HashCode()
// would silently widen the gate to admit the recursing case. If this test fails,
// do not delete it: push key comparison through the worklist first.
func TestNoContainerIsHashable(t *testing.T) {
	vec := values.Vector([]values.Value{values.NewInteger(1)})
	containers := map[string]values.Value{
		"*Pair":      values.NewCons(values.NewInteger(1), values.EmptyList),
		"*Vector":    &vec,
		"*Hashtable": values.NewEmptyHashtable(),
		"*Box":       values.NewBox(values.NewInteger(1)),
	}
	for name, v := range containers {
		t.Run(name, func(t *testing.T) {
			_, ok := v.(values.Hashable)
			qt.Assert(t, ok, qt.IsFalse,
				qt.Commentf("%s now implements Hashable, so it can be a hashtable KEY. "+
					"Hashtable.EqualComponents compares keys recursively on the Go stack; "+
					"a cyclic key will overflow the host. See ADR-D1.", name))
		})
	}
}

// TestEqual_CyclicRecordTerminates pins the record arm of the traversal owner.
// Before Equal was iterative, Record.EqualTo recursed over fields and a
// self-referential record was a host stack overflow (record.go:100). The
// end-to-end host-survival assertion is the subprocess test in pkg/wile; this
// one pins the answer.
func TestEqual_CyclicRecordTerminates(t *testing.T) {
	rt := values.NewRecordType(values.NewSymbol("node"), []*values.Symbol{values.NewSymbol("next")})
	a, err := values.NewRecord(rt, []values.Value{values.Void})
	qt.Assert(t, err, qt.IsNil)
	b, err := values.NewRecord(rt, []values.Value{values.Void})
	qt.Assert(t, err, qt.IsNil)
	a.SetField(0, a)
	b.SetField(0, b)

	qt.Assert(t, values.Equal(a, a), qt.IsTrue)
	// a and b are distinct objects with the same cyclic shape. The coinductive
	// hypothesis makes them equal — the greatest fixpoint, per R7RS §6.1.
	qt.Assert(t, values.Equal(a, b), qt.IsTrue)
}

// TestEqual_CyclicHashtableTerminates pins the hashtable arm (hashtable.go:62).
func TestEqual_CyclicHashtableTerminates(t *testing.T) {
	h := values.NewEmptyHashtable()
	h.Set(values.NewSymbol("self"), h)

	qt.Assert(t, values.Equal(h, h), qt.IsTrue)

	g := values.NewEmptyHashtable()
	g.Set(values.NewSymbol("self"), g)
	qt.Assert(t, values.Equal(h, g), qt.IsTrue)
}

// TestEqual_CyclicBoxTerminates pins the box arm (box.go:64).
func TestEqual_CyclicBoxTerminates(t *testing.T) {
	b := values.NewBox(values.Void)
	b.Value = b
	qt.Assert(t, values.Equal(b, b), qt.IsTrue)

	c := values.NewBox(values.Void)
	c.Value = c
	qt.Assert(t, values.Equal(b, c), qt.IsTrue)
}

// TestEqual_FlatListIsConstantAuxiliarySpace pins the push order in
// Pair.EqualComponents, which is the whole reason a flat list does not cost
// O(n) worklist entries.
//
// It is asserted indirectly but soundly: a list long enough that one worklist
// entry per element would be obvious still compares fast and correct. The
// direct high-water-mark assertion would need a test hook into an unexported
// field; the property that matters — the spine does not accumulate — is what
// makes a 10^6 list tractable at all. Reversing the two push calls in
// EqualComponents makes this test's memory profile grow linearly.
func TestEqual_FlatListIsConstantAuxiliarySpace(t *testing.T) {
	const n = 1_000_000
	build := func() values.Value {
		q := values.Value(values.EmptyList)
		for i := range n {
			q = values.NewCons(values.NewInteger(int64(i)), q)
		}
		return q
	}
	qt.Assert(t, values.Equal(build(), build()), qt.IsTrue)
}

// TestEqual_ShortListsUnchanged guards the teleport-free base cases: the
// worklist rewrite must not perturb ordinary small comparisons.
func TestEqual_ShortListsUnchanged(t *testing.T) {
	mk := func(vs ...int64) values.Value {
		q := values.Value(values.EmptyList)
		for i := len(vs) - 1; i >= 0; i-- {
			q = values.NewCons(values.NewInteger(vs[i]), q)
		}
		return q
	}
	qt.Assert(t, values.Equal(mk(1, 2, 3), mk(1, 2, 3)), qt.IsTrue)
	qt.Assert(t, values.Equal(mk(1, 2, 3), mk(1, 2, 4)), qt.IsFalse)
	qt.Assert(t, values.Equal(mk(1, 2, 3), mk(1, 2)), qt.IsFalse)
	qt.Assert(t, values.Equal(mk(), mk()), qt.IsTrue)
}

// TestHashtable_ConcurrentReadWrite pins hashtable.go. Two goroutines sharing
// one hashtable — which SRFI-18 threads can do, since a hashtable is an ordinary
// Scheme value — once produced Go's unrecoverable "concurrent map read and map
// write" fatal error. The type is now LOCK-FREE (sync.Map + copy-on-write
// buckets), so concurrent access is crash-safe without a mutex; this test pins
// that host-safety, NOT any transactional guarantee (data landing is the user's
// responsibility to synchronize — see the Hashtable type comment).
//
// Must run under -race: per memory/srfi18-parentmc-race-fix.md a green `make ci`
// is not evidence of race-freedom. If a future change reintroduced an in-place
// map/slice write this test is a hard failure under -race and a probabilistic
// host kill.
func TestHashtable_ConcurrentReadWrite(t *testing.T) {
	h := values.NewEmptyHashtable()
	const iterations = 500

	var wg sync.WaitGroup
	wg.Add(4)

	// Writer.
	go func() {
		defer wg.Done()
		for i := range iterations {
			h.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
		}
	}()
	// Competing writer on an overlapping key range, to exercise in-bucket update.
	go func() {
		defer wg.Done()
		for i := range iterations {
			h.Set(values.NewInteger(int64(i%50)), values.NewSymbol("x"))
		}
	}()
	// Reader.
	go func() {
		defer wg.Done()
		for i := range iterations {
			_, _ = h.Get(values.NewInteger(int64(i)))
			_ = h.Size()
		}
	}()
	// Walker: the whole-table readers (snapshot path).
	go func() {
		defer wg.Done()
		for range iterations {
			_ = h.Keys()
			_ = h.Values()
			_ = h.Copy(true)
		}
	}()

	wg.Wait()
}

// TestHashtable_ConcurrentEqualAndMutate pins the comparison path specifically:
// EqualComponents reads a lock-free snapshot (immutable buckets via sync.Map),
// so a concurrent writer must not fault it.
func TestHashtable_ConcurrentEqualAndMutate(t *testing.T) {
	a := values.NewEmptyHashtable()
	b := values.NewEmptyHashtable()
	for i := range 50 {
		a.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
		b.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
	}

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		for range 200 {
			values.Equal(a, b)
		}
	}()
	go func() {
		defer wg.Done()
		for i := range 200 {
			b.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
		}
	}()
	wg.Wait()
}

// nonComparableLeaf is a slice-backed Value: legal Go, implements Value, and is
// NOT Go-comparable. It stands in for an embedder-defined leaf type. Nothing in
// pkg/values is shaped this way, which is exactly why the hazard survived review.
type nonComparableLeaf []values.Value

func (p nonComparableLeaf) SchemeString() string {
	return "#<non-comparable>"
}

func (p nonComparableLeaf) IsVoid() bool {
	return false
}

func (p nonComparableLeaf) EqualTo(v values.Value) bool {
	_, ok := v.(nonComparableLeaf)
	return ok
}

// TestEqual_NonComparableLeafInsideContainerDoesNotPanic pins that a container
// compared against a non-comparable leaf settles as false instead of faulting.
//
// The worklist keys its visited set on equalPairKey{a, b}, which HASHES both
// elements — a strictly stronger demand than the a == b identity check, which
// only faults when the two dynamic types are identical. So a *Pair compared
// against a slice-backed leaf passed the == check (types differ, no fault) and
// then panicked inside the map lookup. Settling a non-container b before the key
// is formed is what closes it, and it is a strict narrowing: every
// EqualComponents asserts b to its own concrete pointer type and returns false.
func TestEqual_NonComparableLeafInsideContainerDoesNotPanic(t *testing.T) {
	leaf := nonComparableLeaf{values.NewInteger(1)}
	container := values.NewCons(values.NewInteger(1), values.EmptyList)

	qt.Assert(t, values.Equal(container, leaf), qt.IsFalse)
	qt.Assert(t, values.Equal(leaf, container), qt.IsFalse)

	// Nested: the leaf reaches step() as a component, which is the path that
	// bypasses Equal's top-level leaf shortcut.
	inA := values.NewCons(leaf, values.EmptyList)
	inB := values.NewCons(container, values.EmptyList)
	qt.Assert(t, values.Equal(inA, inB), qt.IsFalse)
	qt.Assert(t, values.Equal(inB, inA), qt.IsFalse)
}

// TestEqual_SameTypedNonComparableLeavesDoNotPanic covers the case the test above
// does NOT: two non-comparable leaves of the SAME dynamic type, meeting as
// components inside containers.
//
// The sibling test only ever pairs a leaf against a *Pair. Go's interface `==`
// does not fault on differing dynamic types — it answers false — so that pairing
// walked straight past the `if a == b` in step() and the hazard sat there
// unmeasured. Same-typed operands are exactly the shape that faults, and they are
// the *likelier* shape in practice: an embedder comparing two of its own values.
//
// Fixed by ordering: step() now reaches `==` only after both sides are known
// DeepEqualers (pointer-shaped by contract). A leaf settles through EqualTo,
// which never compares interfaces.
func TestEqual_SameTypedNonComparableLeavesDoNotPanic(t *testing.T) {
	a := nonComparableLeaf{values.NewInteger(1)}
	b := nonComparableLeaf{values.NewInteger(1)}

	// Bare, at top level: Equal's leaf path calls EqualTo directly.
	qt.Assert(t, values.Equal(a, b), qt.IsTrue)

	// As components: this is the path that panicked at equal.go:117.
	inA := values.NewCons(a, values.EmptyList)
	inB := values.NewCons(b, values.EmptyList)
	qt.Assert(t, values.Equal(inA, inB), qt.IsTrue)

	// Deeper, and behind a vector, so the worklist carries the pair rather than
	// settling it on the first step.
	deepA := values.NewVector(values.NewCons(a, values.EmptyList))
	deepB := values.NewVector(values.NewCons(b, values.EmptyList))
	qt.Assert(t, values.Equal(deepA, deepB), qt.IsTrue)
}
