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

package machine

import (
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"
)

// testItem is a trivial struct used by Pool[T] tests.
type testItem struct {
	x int
	s string
}

func newTestItem() *testItem {
	return &testItem{
		x: 0,
		s: "",
	}
}

func resetTestItem(t *testItem) {
	t.x = 0
	t.s = ""
}

// ---------------------------------------------------------------------------
// Task 1: Pool[T] core
// ---------------------------------------------------------------------------

func TestPool_Acquire_ReturnsNewObject(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)

	item := p.Acquire()
	qt.Assert(t, item, qt.IsNotNil)
	qt.Assert(t, item.x, qt.Equals, 0)
	qt.Assert(t, item.s, qt.Equals, "")
}

func TestPool_Release_ResetsAndReturns(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)

	item := p.Acquire()
	item.x = 42
	item.s = "hello"
	p.Release(item)

	// Re-acquire: should get back a reset item.
	item2 := p.Acquire()
	qt.Assert(t, item2, qt.IsNotNil)
	qt.Assert(t, item2.x, qt.Equals, 0)
	qt.Assert(t, item2.s, qt.Equals, "")
}

func TestPool_Stats_TracksAcquiresAndReleases(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)

	item1 := p.Acquire()
	item2 := p.Acquire()
	p.Release(item1)

	snap := p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(1))

	p.Release(item2)
	snap = p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(2))
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(0))
}

func TestPool_Stats_TracksMisses(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)

	// First acquire is always a miss (pool starts empty).
	_ = p.Acquire()
	snap := p.Stats()
	qt.Assert(t, snap.Misses, qt.Equals, uint64(1))

	// Second acquire from empty pool is also a miss.
	_ = p.Acquire()
	snap = p.Stats()
	qt.Assert(t, snap.Misses, qt.Equals, uint64(2))
}

// ---------------------------------------------------------------------------
// Task 3: concurrent tests
// ---------------------------------------------------------------------------

func TestPool_ConcurrentAcquireRelease(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	p := NewPool("concurrent", newTestItem, resetTestItem)

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				item := p.Acquire()
				item.x = 1
				item.s = "busy"
				p.Release(item)
			}
		}()
	}
	wg.Wait()

	snap := p.Stats()
	expectedOps := uint64(goroutines * iterations)
	qt.Assert(t, snap.Acquires, qt.Equals, expectedOps)
	qt.Assert(t, snap.Releases, qt.Equals, expectedOps)
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(0))
}

// ---------------------------------------------------------------------------
// FreeList[T] core
// ---------------------------------------------------------------------------

func TestFreeList_Acquire_ReturnsNewObject(t *testing.T) {
	fl := NewFreeList("test", newTestItem, resetTestItem)

	item := fl.Acquire()
	qt.Assert(t, item, qt.IsNotNil)
	qt.Assert(t, item.x, qt.Equals, 0)
	qt.Assert(t, item.s, qt.Equals, "")
}

func TestFreeList_Release_RecyclesDeterministically(t *testing.T) {
	fl := NewFreeList("test", newTestItem, resetTestItem)

	item := fl.Acquire()
	item.x = 42
	item.s = "hello"
	fl.Release(item)

	// Re-acquire must return the same (reset) object — deterministic, no GC dependency.
	item2 := fl.Acquire()
	qt.Assert(t, item2, qt.IsNotNil)
	qt.Assert(t, item2.x, qt.Equals, 0)
	qt.Assert(t, item2.s, qt.Equals, "")

	// Second acquire was a hit, not a miss.
	snap := fl.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Misses, qt.Equals, uint64(1))
}

func TestFreeList_Stats_TracksAcquiresAndReleases(t *testing.T) {
	fl := NewFreeList("test", newTestItem, resetTestItem)

	item1 := fl.Acquire()
	item2 := fl.Acquire()
	fl.Release(item1)

	snap := fl.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(1))

	fl.Release(item2)
	snap = fl.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(2))
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(0))
}

func TestFreeList_Stats_TracksMisses(t *testing.T) {
	fl := NewFreeList("test", newTestItem, resetTestItem)

	_ = fl.Acquire()
	snap := fl.Stats()
	qt.Assert(t, snap.Misses, qt.Equals, uint64(1))

	_ = fl.Acquire()
	snap = fl.Stats()
	qt.Assert(t, snap.Misses, qt.Equals, uint64(2))
}

func TestFreeList_ConcurrentAcquireRelease(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	fl := NewFreeList("concurrent", newTestItem, resetTestItem)

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				item := fl.Acquire()
				item.x = 1
				item.s = "busy"
				fl.Release(item)
			}
		}()
	}
	wg.Wait()

	snap := fl.Stats()
	expectedOps := uint64(goroutines * iterations)
	qt.Assert(t, snap.Acquires, qt.Equals, expectedOps)
	qt.Assert(t, snap.Releases, qt.Equals, expectedOps)
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(0))
}

// ---------------------------------------------------------------------------
// unsyncFreeList[T] — the lock-free per-thread variant. Single-goroutine by the
// per-thread-pool invariant (plans/2026-06-08-per-thread-pools-invariant.local.md);
// these pin its allocate-on-miss / reset-on-release semantics and the pool
// independence that licenses dropping FreeList's mutex and atomic counters.
// ---------------------------------------------------------------------------

func TestUnsyncFreeList_Acquire_ReturnsNewObject(t *testing.T) {
	fl := newUnsyncFreeList(newTestItem, resetTestItem)

	item := fl.Acquire()
	qt.Assert(t, item, qt.IsNotNil)
	qt.Assert(t, item.x, qt.Equals, 0)
	qt.Assert(t, item.s, qt.Equals, "")
}

func TestUnsyncFreeList_Release_RecyclesDeterministically(t *testing.T) {
	fl := newUnsyncFreeList(newTestItem, resetTestItem)

	item := fl.Acquire()
	item.x = 42
	item.s = "hello"
	fl.Release(item)

	// Re-acquire returns the same object, reset — deterministic, no GC dependency.
	item2 := fl.Acquire()
	qt.Assert(t, item2, qt.IsNotNil)
	qt.Assert(t, item2.x, qt.Equals, 0)
	qt.Assert(t, item2.s, qt.Equals, "")
}

func TestUnsyncFreeList_Stats_TracksMissesAndInFlight(t *testing.T) {
	fl := newUnsyncFreeList(newTestItem, resetTestItem)

	// Three acquires from an empty freelist are three misses; release one.
	a := fl.Acquire()
	_ = fl.Acquire()
	_ = fl.Acquire()
	fl.Release(a)

	// acquires (3) > releases (1) exercises the InFlight = acquires - releases
	// branch, which balanced-round-trip VM tests (acquires == releases) never hit.
	snap := fl.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(3))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
	qt.Assert(t, snap.Misses, qt.Equals, uint64(3))
	qt.Assert(t, snap.InFlight, qt.Equals, uint64(2))
}

func TestUnsyncFreeList_ConcurrentDistinctPools(t *testing.T) {
	// The invariant that licenses dropping the mutex is that no two goroutines
	// ever touch the SAME unsyncFreeList. This pins that model: N goroutines, each
	// owning its OWN freelist, hammer acquire/release concurrently. Under -race,
	// any accidental cross-goroutine sharing of a per-thread pool — the bug the
	// removed mutex used to mask — would trip the detector here. Each goroutine
	// writes only its own snaps[g] slot, so the result collection is race-free.
	const goroutines = 16
	const iterations = 200

	snaps := make([]PoolSnapshot, goroutines)
	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := range goroutines {
		go func() {
			defer wg.Done()
			fl := newUnsyncFreeList(newTestItem, resetTestItem)
			for range iterations {
				item := fl.Acquire()
				item.x = 1
				fl.Release(item)
			}
			snaps[g] = fl.Stats()
		}()
	}
	wg.Wait()

	for g := range goroutines {
		qt.Assert(t, snaps[g].Acquires, qt.Equals, uint64(iterations))
		qt.Assert(t, snaps[g].Releases, qt.Equals, uint64(iterations))
		qt.Assert(t, snaps[g].InFlight, qt.Equals, uint64(0))
	}

	// TODO(author): the deeper invariant — that the VM never lets a frame
	// allocated on one goroutine be released on another — is exercised end-to-end
	// by the SRFI-18 -race suite (extensions/threads). To pin it at the machine
	// layer, drive N NewThreadSubContext roots (machine_context_subcontext.go),
	// each running a non-tail-call workload, and assert -race clean. Left as the
	// core-property test per the CLAUDE.local.md "leave the key test" convention.
}
