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
	"strings"
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

func TestPool_Name(t *testing.T) {
	p := NewPool("stack", newTestItem, resetTestItem)
	qt.Assert(t, p.Name(), qt.Equals, "stack")
}

func TestPool_SetEnabled_False_BypassesPool(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)
	p.SetEnabled(false)

	item := p.Acquire()
	qt.Assert(t, item, qt.IsNotNil)

	// Even after release, a new acquire creates a fresh object (bypassing pool).
	item.x = 99
	p.Release(item)

	item2 := p.Acquire()
	qt.Assert(t, item2, qt.IsNotNil)
	qt.Assert(t, item2.x, qt.Equals, 0)

	// Misses should be zero when disabled (newFn called directly, not via inner.New).
	snap := p.Stats()
	qt.Assert(t, snap.Misses, qt.Equals, uint64(0))
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
}

func TestPool_SetEnabled_ReEnable(t *testing.T) {
	p := NewPool("test", newTestItem, resetTestItem)

	// Disable then re-enable.
	p.SetEnabled(false)
	item := p.Acquire()
	p.Release(item)

	p.SetEnabled(true)
	item2 := p.Acquire()
	qt.Assert(t, item2, qt.IsNotNil)
	qt.Assert(t, item2.x, qt.Equals, 0)

	// After re-enable, pool misses resume tracking.
	snap := p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
}

// ---------------------------------------------------------------------------
// Task 2: PoolHandle interface and PoolManager
// ---------------------------------------------------------------------------

// Compile-time check: *Pool[testItem] must implement PoolHandle.
var _ PoolHandle = (*Pool[testItem])(nil)

func TestPool_ImplementsPoolHandle(t *testing.T) {
	// This test documents the interface compliance. The var_ line above
	// is the real compile-time check; this test makes the intent visible.
	var h PoolHandle = NewPool("iface-check", newTestItem, resetTestItem)
	qt.Assert(t, h.Name(), qt.Equals, "iface-check")
}

func TestPoolManager_Register_And_AllStats(t *testing.T) {
	mgr := NewPoolManager()

	p1 := NewPool("stack", newTestItem, resetTestItem)
	p2 := NewPool("cont", newTestItem, resetTestItem)
	mgr.Register(p1)
	mgr.Register(p2)

	_ = p1.Acquire()
	_ = p1.Acquire()
	_ = p2.Acquire()

	stats := mgr.AllStats()
	qt.Assert(t, len(stats), qt.Equals, 2)
	qt.Assert(t, stats[0].Name, qt.Equals, "stack")
	qt.Assert(t, stats[0].Acquires, qt.Equals, uint64(2))
	qt.Assert(t, stats[1].Name, qt.Equals, "cont")
	qt.Assert(t, stats[1].Acquires, qt.Equals, uint64(1))
}

func TestPoolManager_SetAllEnabled(t *testing.T) {
	mgr := NewPoolManager()

	p1 := NewPool("a", newTestItem, resetTestItem)
	p2 := NewPool("b", newTestItem, resetTestItem)
	mgr.Register(p1)
	mgr.Register(p2)

	mgr.SetAllEnabled(false)

	// Both pools should be disabled: acquires bypass pool (no misses).
	_ = p1.Acquire()
	_ = p2.Acquire()
	qt.Assert(t, p1.Stats().Misses, qt.Equals, uint64(0))
	qt.Assert(t, p2.Stats().Misses, qt.Equals, uint64(0))

	mgr.SetAllEnabled(true)

	// After re-enable, misses resume.
	_ = p1.Acquire()
	qt.Assert(t, p1.Stats().Misses, qt.Equals, uint64(1))
}

func TestPoolManager_String(t *testing.T) {
	mgr := NewPoolManager()
	p := NewPool("stack", newTestItem, resetTestItem)
	mgr.Register(p)

	_ = p.Acquire()

	s := mgr.String()
	qt.Assert(t, strings.Contains(s, "stack"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "Acquires"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "Releases"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "Misses"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "InFlight"), qt.IsTrue)
}

func TestPoolManager_String_Empty(t *testing.T) {
	mgr := NewPoolManager()
	s := mgr.String()
	qt.Assert(t, strings.Contains(s, "no pools registered"), qt.IsTrue)
}

func TestPoolManager_DrainAll_DoesNotPanic(t *testing.T) {
	mgr := NewPoolManager()
	p := NewPool("test", newTestItem, resetTestItem)
	mgr.Register(p)

	item := p.Acquire()
	p.Release(item)

	// Must not panic.
	mgr.DrainAll()
}

// ---------------------------------------------------------------------------
// Task 3: registerPool helper and concurrent tests
// ---------------------------------------------------------------------------

func TestRegisterPool_ReturnsPool(t *testing.T) {
	mgr := NewPoolManager()
	p := registerPool(mgr, NewPool("reg", newTestItem, resetTestItem))

	qt.Assert(t, p, qt.IsNotNil)
	qt.Assert(t, p.Name(), qt.Equals, "reg")

	// Verify the pool was registered.
	stats := mgr.AllStats()
	qt.Assert(t, len(stats), qt.Equals, 1)
	qt.Assert(t, stats[0].Name, qt.Equals, "reg")
}

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
