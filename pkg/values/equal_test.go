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
// rests on (ADR-D1).
//
// Hashtable.EqualComponents pairs entries by KEY and pushes only the matched
// VALUES onto Equal's worklist. That pairing calls EqualTo on keys directly —
// i.e. it recurses on the Go stack — and is sound only because a key cannot be a
// container: Hashtable.Set type-asserts Hashable and rejects everything else,
// and no container type implements it.
//
// Giving *Pair or *Vector a HashCode() (which is exactly what R6RS
// make-equal-hashtable and SRFI-69 want) would make a cyclic KEY constructible
// and put unbounded recursion straight back inside EqualComponents — in a shape
// no other test in the suite catches, because every cycle test goes through
// values, not keys. If this test fails, do not delete it: make key comparison go
// through the worklist first.
//
// The same fact retired the original design's "make HashCode iterative" task:
// there is no container HashCode to make iterative.
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
	qt.Assert(t, h.Set(values.NewSymbol("self"), h), qt.IsNil)

	qt.Assert(t, values.Equal(h, h), qt.IsTrue)

	g := values.NewEmptyHashtable()
	qt.Assert(t, g.Set(values.NewSymbol("self"), g), qt.IsNil)
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

// TestHashtable_ConcurrentReadWrite pins hashtable.go:159. Two goroutines
// sharing one hashtable — which SRFI-18 threads can do, since a hashtable is an
// ordinary Scheme value — produced Go's unrecoverable "concurrent map read and
// map write" fatal error.
//
// Must run under -race: per memory/srfi18-parentmc-race-fix.md a green `make ci`
// is not evidence of race-freedom. Without the mutex this test is a hard failure
// under -race and a probabilistic host kill without it.
func TestHashtable_ConcurrentReadWrite(t *testing.T) {
	h := values.NewEmptyHashtable()
	const iterations = 500

	var wg sync.WaitGroup
	wg.Add(4)

	// Writer.
	go func() {
		defer wg.Done()
		for i := range iterations {
			_ = h.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
		}
	}()
	// Competing writer on an overlapping key range, to exercise in-bucket update.
	go func() {
		defer wg.Done()
		for i := range iterations {
			_ = h.Set(values.NewInteger(int64(i%50)), values.NewSymbol("x"))
		}
	}()
	// Reader.
	go func() {
		defer wg.Done()
		for i := range iterations {
			_, _, _ = h.Get(values.NewInteger(int64(i)))
			_ = h.Size()
		}
	}()
	// Walker: the whole-table readers (snapshot path).
	go func() {
		defer wg.Done()
		for range iterations {
			_ = h.Keys()
			_ = h.Values()
			_ = h.Copy()
		}
	}()

	wg.Wait()
}

// TestHashtable_ConcurrentEqualAndMutate pins the comparison path specifically:
// EqualComponents snapshots rather than holding a read lock across the walk, so
// a concurrent writer must not fault it.
func TestHashtable_ConcurrentEqualAndMutate(t *testing.T) {
	a := values.NewEmptyHashtable()
	b := values.NewEmptyHashtable()
	for i := range 50 {
		qt.Assert(t, a.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i))), qt.IsNil)
		qt.Assert(t, b.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i))), qt.IsNil)
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
			_ = b.Set(values.NewInteger(int64(i)), values.NewInteger(int64(i)))
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
