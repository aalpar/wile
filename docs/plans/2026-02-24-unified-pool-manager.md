# Unified Pool Manager Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace three ad-hoc `sync.Pool` instances with a generic `Pool[T]` type, a `PoolManager` registry, and unified observability/management controls.

**Architecture:** Generic `Pool[T any]` wraps `sync.Pool` with atomic counters, reset callback, and enable/disable toggle. `PoolManager` holds heterogeneous pools via `PoolHandle` interface. Three existing pools (stack, sub-context, continuation) migrate to `Pool[T]` instances. All existing acquire/release function signatures preserved — call sites unchanged.

**Tech Stack:** Go generics (1.18+), `sync/atomic`, `sync.Pool`, `github.com/frankban/quicktest`

**Design doc:** `plans/UNIFIED_POOL_MANAGER.md`

---

### Task 1: Create `Pool[T]` core type with tests

**Files:**
- Create: `machine/pool_generic.go`
- Create: `machine/pool_generic_test.go`

**Step 1: Write failing tests for Pool[T] basic operations**

Create `machine/pool_generic_test.go`:

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...same license header as other files...

package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// testItem is a minimal struct for testing Pool[T].
type testItem struct {
	x int
	s string
}

func TestPool_Acquire_ReturnsNewObject(t *testing.T) {
	p := NewPool("test",
		func() *testItem {
			return &testItem{}
		},
		func(v *testItem) {
			*v = testItem{}
		},
	)
	item := p.Acquire()
	qt.Assert(t, item, qt.IsNotNil)
	qt.Assert(t, item.x, qt.Equals, 0)
	qt.Assert(t, item.s, qt.Equals, "")
}

func TestPool_Release_ResetsAndReturns(t *testing.T) {
	p := NewPool("test",
		func() *testItem {
			return &testItem{}
		},
		func(v *testItem) {
			*v = testItem{}
		},
	)
	item := p.Acquire()
	item.x = 42
	item.s = "dirty"
	p.Release(item)

	// Re-acquire: should be zeroed by reset callback.
	item2 := p.Acquire()
	qt.Assert(t, item2.x, qt.Equals, 0)
	qt.Assert(t, item2.s, qt.Equals, "")
}

func TestPool_Stats_TracksAcquiresAndReleases(t *testing.T) {
	p := NewPool("test",
		func() *testItem {
			return &testItem{}
		},
		func(v *testItem) {
			*v = testItem{}
		},
	)
	_ = p.Acquire()
	_ = p.Acquire()
	snap := p.Stats()
	qt.Assert(t, snap.Name, qt.Equals, "test")
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(2))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(0))

	// Release one.
	item := p.Acquire()
	p.Release(item)
	snap = p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(3))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
}

func TestPool_Stats_TracksMisses(t *testing.T) {
	calls := 0
	p := NewPool("test",
		func() *testItem {
			calls++
			return &testItem{}
		},
		func(v *testItem) {
			*v = testItem{}
		},
	)

	// First acquire always misses (pool is empty).
	_ = p.Acquire()
	snap := p.Stats()
	qt.Assert(t, snap.Misses >= 1, qt.IsTrue)
}

func TestPool_Name(t *testing.T) {
	p := NewPool("my_pool",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	qt.Assert(t, p.Name(), qt.Equals, "my_pool")
}

func TestPool_SetEnabled_False_BypassesPool(t *testing.T) {
	p := NewPool("test",
		func() *testItem {
			return &testItem{}
		},
		func(v *testItem) {
			*v = testItem{}
		},
	)
	p.SetEnabled(false)

	// Acquire still works (returns fresh allocation).
	item := p.Acquire()
	qt.Assert(t, item, qt.IsNotNil)

	// Release is a no-op for the pool (reset still runs for safety).
	item.x = 99
	p.Release(item)

	// Stats still track even when disabled.
	snap := p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(1))
}

func TestPool_SetEnabled_ReEnable(t *testing.T) {
	p := NewPool("test",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)

	// Disable then re-enable.
	p.SetEnabled(false)
	p.SetEnabled(true)

	// Pool behavior should work normally.
	item := p.Acquire()
	item.x = 7
	p.Release(item)
	item2 := p.Acquire()
	qt.Assert(t, item2.x, qt.Equals, 0) // reset ran
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestPool_' ./machine/...`
Expected: FAIL — `NewPool` undefined

**Step 3: Write `Pool[T]` implementation**

Create `machine/pool_generic.go`:

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...same license header...

package machine

import (
	"sync"
	"sync/atomic"
)

// Pool[T] is a type-safe, observable object pool backed by sync.Pool.
// It provides automatic acquire/release counters, miss tracking, and
// an enable/disable toggle for debugging (e.g., detecting use-after-release
// with -race by disabling recycling).
//
// See plans/UNIFIED_POOL_MANAGER.md for design rationale.
type Pool[T any] struct {
	name  string
	inner sync.Pool
	newFn func() *T
	reset func(*T)
	stats poolStats

	// enabled controls whether the pool recycles objects. When false,
	// Acquire always allocates fresh and Release does not return objects
	// to the pool (but still calls reset for safety). Default: true.
	enabled atomic.Bool
}

// poolStats holds atomic counters for a single pool instance.
type poolStats struct {
	acquires atomic.Uint64
	releases atomic.Uint64
	misses   atomic.Uint64
}

// PoolSnapshot is a point-in-time copy of pool counters.
type PoolSnapshot struct {
	Name     string
	Acquires uint64
	Releases uint64
	Misses   uint64
	InFlight uint64
}

// NewPool creates a new Pool[T].
//   - name: human-readable pool identifier (used in stats output)
//   - newFn: allocates a fresh *T (called on pool miss or when disabled)
//   - resetFn: clears *T for reuse (called during Release, before Put)
func NewPool[T any](name string, newFn func() *T, resetFn func(*T)) *Pool[T] {
	p := &Pool[T]{
		name:  name,
		newFn: newFn,
		reset: resetFn,
	}
	p.enabled.Store(true)
	p.inner.New = func() any {
		p.stats.misses.Add(1)
		return newFn()
	}
	return p
}

// Acquire returns a *T from the pool. If the pool is empty, newFn is called.
// If the pool is disabled, newFn is called directly (bypassing sync.Pool).
func (p *Pool[T]) Acquire() *T {
	p.stats.acquires.Add(1)
	if !p.enabled.Load() {
		return p.newFn()
	}
	return p.inner.Get().(*T)
}

// Release resets v via resetFn and returns it to the pool.
// If the pool is disabled, reset still runs (for safety) but the object
// is not returned to the pool.
func (p *Pool[T]) Release(v *T) {
	p.stats.releases.Add(1)
	p.reset(v)
	if p.enabled.Load() {
		p.inner.Put(v)
	}
}

// Name returns the pool's human-readable name.
func (p *Pool[T]) Name() string {
	return p.name
}

// Stats returns a point-in-time snapshot of pool counters.
func (p *Pool[T]) Stats() PoolSnapshot {
	acq := p.stats.acquires.Load()
	rel := p.stats.releases.Load()
	var inFlight uint64
	if acq > rel {
		inFlight = acq - rel
	}
	return PoolSnapshot{
		Name:     p.name,
		Acquires: acq,
		Releases: rel,
		Misses:   p.stats.misses.Load(),
		InFlight: inFlight,
	}
}

// Drain triggers a GC cycle to clear cached pool objects.
// This is intended for test/debug scenarios, not production hot paths.
func (p *Pool[T]) Drain() {
	// sync.Pool objects are cleared on every GC cycle.
	// There is no public API to selectively drain a pool.
	// Calling runtime.GC() is the standard approach.
	// Callers should use PoolManager.DrainAll() which calls GC once.
}

// SetEnabled toggles pool recycling. When false, Acquire always allocates
// fresh objects and Release does not return objects to the pool.
func (p *Pool[T]) SetEnabled(b bool) {
	p.enabled.Store(b)
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestPool_' ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS (no formatting or import issues)

**Step 6: Commit**

```
feat(machine): add generic Pool[T] with observability and controls

Introduces Pool[T], a type-safe wrapper around sync.Pool with:
- Automatic atomic counters (acquires, releases, misses)
- Enable/disable toggle for debugging
- PoolSnapshot for point-in-time stats

Part 1 of unified pool manager (plans/UNIFIED_POOL_MANAGER.md).
```

---

### Task 2: Create `PoolHandle` interface and `PoolManager`

**Files:**
- Modify: `machine/pool_generic.go` (add PoolHandle, PoolManager)
- Modify: `machine/pool_generic_test.go` (add manager tests)

**Step 1: Write failing tests for PoolManager**

Append to `machine/pool_generic_test.go`:

```go
// ---------------------------------------------------------------------------
// PoolHandle interface compliance
// ---------------------------------------------------------------------------

func TestPool_ImplementsPoolHandle(t *testing.T) {
	p := NewPool("test",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	// Compile-time check: Pool[testItem] must satisfy PoolHandle.
	var _ PoolHandle = p
}

// ---------------------------------------------------------------------------
// PoolManager
// ---------------------------------------------------------------------------

func TestPoolManager_Register_And_AllStats(t *testing.T) {
	mgr := NewPoolManager()
	p1 := NewPool("pool_a",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	p2 := NewPool("pool_b",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	mgr.Register(p1)
	mgr.Register(p2)

	// Do some operations on p1 only.
	item := p1.Acquire()
	p1.Release(item)

	stats := mgr.AllStats()
	qt.Assert(t, len(stats), qt.Equals, 2)
	qt.Assert(t, stats[0].Name, qt.Equals, "pool_a")
	qt.Assert(t, stats[0].Acquires, qt.Equals, uint64(1))
	qt.Assert(t, stats[0].Releases, qt.Equals, uint64(1))
	qt.Assert(t, stats[1].Name, qt.Equals, "pool_b")
	qt.Assert(t, stats[1].Acquires, qt.Equals, uint64(0))
}

func TestPoolManager_SetAllEnabled(t *testing.T) {
	mgr := NewPoolManager()
	p1 := NewPool("pool_a",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	p2 := NewPool("pool_b",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	mgr.Register(p1)
	mgr.Register(p2)

	mgr.SetAllEnabled(false)

	// Both pools should now be disabled — acquires allocate fresh.
	item1 := p1.Acquire()
	item2 := p2.Acquire()
	qt.Assert(t, item1, qt.IsNotNil)
	qt.Assert(t, item2, qt.IsNotNil)

	// Stats still tracked.
	stats := mgr.AllStats()
	qt.Assert(t, stats[0].Acquires, qt.Equals, uint64(1))
	qt.Assert(t, stats[1].Acquires, qt.Equals, uint64(1))

	// Re-enable.
	mgr.SetAllEnabled(true)
}

func TestPoolManager_String(t *testing.T) {
	mgr := NewPoolManager()
	p := NewPool("test_pool",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	mgr.Register(p)

	_ = p.Acquire()
	_ = p.Acquire()
	item := p.Acquire()
	p.Release(item)

	s := mgr.String()
	qt.Assert(t, s != "", qt.IsTrue)
	// Should contain pool name and counts.
	qt.Assert(t, len(s) > 0, qt.IsTrue)
}

func TestPoolManager_DrainAll_DoesNotPanic(t *testing.T) {
	mgr := NewPoolManager()
	p := NewPool("test",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)
	mgr.Register(p)

	_ = p.Acquire()
	// DrainAll should not panic.
	mgr.DrainAll()
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestPool' ./machine/...`
Expected: FAIL — `PoolHandle`, `NewPoolManager` undefined

**Step 3: Write PoolHandle and PoolManager implementation**

Append to `machine/pool_generic.go`:

```go
// PoolHandle is the non-generic interface for managing heterogeneous pools.
// Pool[T] implements this interface automatically.
type PoolHandle interface {
	Name() string
	Stats() PoolSnapshot
	Drain()
	SetEnabled(bool)
}

// PoolManager aggregates pools for unified observation and control.
type PoolManager struct {
	mu    sync.RWMutex
	pools []PoolHandle
}

// NewPoolManager creates an empty PoolManager.
func NewPoolManager() *PoolManager {
	return &PoolManager{}
}

// Register adds a pool to the manager.
func (m *PoolManager) Register(h PoolHandle) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.pools = append(m.pools, h)
}

// AllStats returns a snapshot of all registered pools.
func (m *PoolManager) AllStats() []PoolSnapshot {
	m.mu.RLock()
	defer m.mu.RUnlock()
	stats := make([]PoolSnapshot, len(m.pools))
	for i, p := range m.pools {
		stats[i] = p.Stats()
	}
	return stats
}

// DrainAll triggers a GC cycle to clear all pool caches.
func (m *PoolManager) DrainAll() {
	runtime.GC()
}

// SetAllEnabled sets the enabled state of all registered pools.
func (m *PoolManager) SetAllEnabled(b bool) {
	m.mu.RLock()
	defer m.mu.RUnlock()
	for _, p := range m.pools {
		p.SetEnabled(b)
	}
}

// String returns a tabular summary of all pool stats.
func (m *PoolManager) String() string {
	stats := m.AllStats()
	var b strings.Builder
	for _, s := range stats {
		fmt.Fprintf(&b, "%-20s acquires=%-8d releases=%-8d misses=%-8d in_flight=%-8d\n",
			s.Name, s.Acquires, s.Releases, s.Misses, s.InFlight)
	}
	return b.String()
}
```

Add `"fmt"`, `"runtime"`, and `"strings"` to the import block.

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestPool' ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(machine): add PoolHandle interface and PoolManager

PoolManager registers heterogeneous Pool[T] instances via PoolHandle,
providing AllStats(), DrainAll(), SetAllEnabled(), and String().

Part 2 of unified pool manager (plans/UNIFIED_POOL_MANAGER.md).
```

---

### Task 3: Add `registerPool` helper and concurrent pool tests

**Files:**
- Modify: `machine/pool_generic.go` (add registerPool)
- Modify: `machine/pool_generic_test.go` (add concurrent tests)

**Step 1: Write failing tests**

Append to `machine/pool_generic_test.go`:

```go
// ---------------------------------------------------------------------------
// registerPool helper
// ---------------------------------------------------------------------------

func TestRegisterPool_ReturnsPool(t *testing.T) {
	mgr := NewPoolManager()
	p := registerPool(mgr, NewPool("test",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	))
	qt.Assert(t, p, qt.IsNotNil)
	qt.Assert(t, p.Name(), qt.Equals, "test")

	stats := mgr.AllStats()
	qt.Assert(t, len(stats), qt.Equals, 1)
	qt.Assert(t, stats[0].Name, qt.Equals, "test")
}

// ---------------------------------------------------------------------------
// Concurrent access
// ---------------------------------------------------------------------------

func TestPool_ConcurrentAcquireRelease(t *testing.T) {
	const goroutines = 16
	const iterations = 100

	p := NewPool("concurrent",
		func() *testItem { return &testItem{} },
		func(v *testItem) { *v = testItem{} },
	)

	var wg sync.WaitGroup
	wg.Add(goroutines)

	for range goroutines {
		go func() {
			defer wg.Done()
			for range iterations {
				item := p.Acquire()
				item.x = 42
				p.Release(item)
			}
		}()
	}
	wg.Wait()

	snap := p.Stats()
	qt.Assert(t, snap.Acquires, qt.Equals, uint64(goroutines*iterations))
	qt.Assert(t, snap.Releases, qt.Equals, uint64(goroutines*iterations))
}
```

Add `"sync"` to test file imports.

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestRegisterPool|TestPool_Concurrent' ./machine/...`
Expected: FAIL — `registerPool` undefined

**Step 3: Write registerPool helper**

Append to `machine/pool_generic.go`:

```go
// registerPool registers the pool with the manager and returns it.
// This enables the var-init-chain pattern:
//
//	var myPool = registerPool(mgr, NewPool(...))
func registerPool[T any](mgr *PoolManager, p *Pool[T]) *Pool[T] {
	mgr.Register(p)
	return p
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestRegisterPool|TestPool_Concurrent' ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(machine): add registerPool helper and concurrent pool tests
```

---

### Task 4: Migrate stack pool to Pool[T]

**Files:**
- Modify: `machine/pool.go` (replace stackPool var, simplify acquireStack/releaseStack)

**Step 1: Run existing pool tests as baseline**

Run: `go test -v -run 'TestAcquireStack|TestReleaseStack|TestStackPool' ./machine/...`
Expected: PASS (all existing tests pass before migration)

**Step 2: Migrate stack pool**

In `machine/pool.go`, replace the stackPool var and its acquire/release functions.

Replace the `stackPool` var declaration:

```go
// Before:
var stackPool = sync.Pool{
	New: func() any {
		s := make(Stack, 0, stackInitialCap)
		return &s
	},
}

// After:
var stackPool = registerPool(pools, NewPool("stack",
	func() *Stack {
		s := make(Stack, 0, stackInitialCap)
		return &s
	},
	func(s *Stack) {
		full := (*s)[:cap(*s)]
		for i := range full {
			full[i] = nil
		}
		*s = full[:0]
	},
))
```

Replace `acquireStack`:

```go
// Before:
func acquireStack() *Stack {
	s := stackPool.Get().(*Stack)
	*s = (*s)[:0]
	return s
}

// After:
func acquireStack() *Stack {
	return stackPool.Acquire()
}
```

Replace `releaseStack`:

```go
// Before:
func releaseStack(s *Stack) {
	if s == nil {
		return
	}
	full := (*s)[:cap(*s)]
	for i := range full {
		full[i] = nil
	}
	*s = full[:0]
	stackPool.Put(s)
}

// After:
func releaseStack(s *Stack) {
	if s == nil {
		return
	}
	stackPool.Release(s)
}
```

Add the package-level `pools` manager var at the top of the var block:

```go
var pools = NewPoolManager()
```

Remove `"sync"` from pool.go imports if it's no longer needed (it still is for `context` — check after all 3 pools are migrated).

**Step 3: Run existing stack pool tests**

Run: `go test -v -run 'TestAcquireStack|TestReleaseStack|TestStackPool' ./machine/...`
Expected: PASS — all existing tests pass unchanged

**Step 4: Run full test suite**

Run: `go test -v ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
refactor(machine): migrate stack pool to Pool[T]

stackPool now uses the generic Pool[T] with automatic counters.
acquireStack/releaseStack signatures unchanged — call sites unaffected.
```

---

### Task 5: Migrate continuation pool to Pool[T]

**Files:**
- Modify: `machine/pool.go`

**Step 1: Run existing continuation pool tests as baseline**

Run: `go test -v -run 'TestAcquireContinuation|TestReleaseContinuation|TestContinuationPool|TestRestoreAndRelease' ./machine/...`
Expected: PASS

**Step 2: Migrate continuation pool**

Replace the `continuationPool` var:

```go
// Before:
var continuationPool = sync.Pool{
	New: func() any {
		return &MachineContinuation{}
	},
}

// After:
var continuationPool = registerPool(pools, NewPool("continuation",
	func() *MachineContinuation {
		return &MachineContinuation{}
	},
	func(cont *MachineContinuation) {
		releaseStack(cont.evals)
		*cont = MachineContinuation{}
	},
))
```

Replace `acquireContinuation`:

```go
// Before:
func acquireContinuation() *MachineContinuation {
	return continuationPool.Get().(*MachineContinuation)
}

// After:
func acquireContinuation() *MachineContinuation {
	return continuationPool.Acquire()
}
```

Replace `releaseContinuation`:

```go
// Before:
func releaseContinuation(cont *MachineContinuation) {
	if cont == nil {
		return
	}
	releaseStack(cont.evals)
	*cont = MachineContinuation{}
	continuationPool.Put(cont)
}

// After:
func releaseContinuation(cont *MachineContinuation) {
	if cont == nil {
		return
	}
	continuationPool.Release(cont)
}
```

**CRITICAL: `RestoreAndRelease` in `machine_context.go` must NOT change.** It calls `releaseContinuation(cont)` after setting `cont.evals = nil`. The reset callback inside `continuationPool` calls `releaseStack(cont.evals)` which is a no-op on nil. This preserves the existing behavior: the evals transfer happens in `RestoreAndRelease`, and the nil-safe `releaseStack(nil)` inside the reset callback does nothing. Verify this by reading the code path:

1. `RestoreAndRelease` sets `cont.evals = nil`
2. `RestoreAndRelease` calls `releaseContinuation(cont)`
3. `releaseContinuation` calls `continuationPool.Release(cont)`
4. `Pool.Release` calls `resetFn(cont)` which calls `releaseStack(cont.evals)` — `cont.evals` is nil — no-op
5. Then `*cont = MachineContinuation{}` — zeroes the rest
6. Then `inner.Put(cont)` — returns to pool

This is correct.

**Step 3: Run existing continuation pool tests**

Run: `go test -v -run 'TestAcquireContinuation|TestReleaseContinuation|TestContinuationPool|TestRestoreAndRelease' ./machine/...`
Expected: PASS

**Step 4: Run full test suite**

Run: `go test -v ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
refactor(machine): migrate continuation pool to Pool[T]

continuationPool now uses generic Pool[T]. Cascading releaseStack
call lives in reset callback. RestoreAndRelease unchanged.
```

---

### Task 6: Migrate sub-context pool to Pool[T]

**Files:**
- Modify: `machine/pool.go`

**Step 1: Run existing sub-context pool tests as baseline**

Run: `go test -v -run 'TestAcquireSubContext|TestReleaseSubContext|TestSubContextPool|TestAcquireMacroContext' ./machine/...`
Expected: PASS

**Step 2: Migrate sub-context pool**

Replace the `subContextPool` var:

```go
// Before:
var subContextPool = sync.Pool{
	New: func() any {
		return &MachineContext{}
	},
}

// After:
var subContextPool = registerPool(pools, NewPool("sub_context",
	func() *MachineContext {
		return &MachineContext{}
	},
	func(mc *MachineContext) {
		releaseStack(mc.evals)
		*mc = MachineContext{}
	},
))
```

Replace `acquireSubContext`:

```go
// Before:
func acquireSubContext() *MachineContext {
	return subContextPool.Get().(*MachineContext)
}

// After:
func acquireSubContext() *MachineContext {
	return subContextPool.Acquire()
}
```

Replace `ReleaseSubContext` — keep the domain logic (parent counter), delegate reset to pool:

```go
// Before:
func ReleaseSubContext(mc *MachineContext) {
	if mc == nil {
		return
	}
	if mc.parentMC != nil {
		mc.parentMC.counters.SubContextPoolReleases++
	}
	releaseStack(mc.evals)
	*mc = MachineContext{}
	subContextPool.Put(mc)
}

// After:
func ReleaseSubContext(mc *MachineContext) {
	if mc == nil {
		return
	}
	if mc.parentMC != nil {
		mc.parentMC.counters.SubContextPoolReleases++
	}
	subContextPool.Release(mc)
}
```

**Step 3: Run existing sub-context pool tests**

Run: `go test -v -run 'TestAcquireSubContext|TestReleaseSubContext|TestSubContextPool|TestAcquireMacroContext' ./machine/...`
Expected: PASS

**Step 4: Run full test suite**

Run: `go test -v ./machine/...`
Expected: PASS

**Step 5: Clean up imports in pool.go**

After all three pools are migrated, `pool.go` no longer needs `"sync"` in its import block (the `sync.Pool` references are all gone). It still needs `"context"` for `acquireMacroContext`. Remove `"sync"` if unused.

**Step 6: Run lint**

Run: `make lint`
Expected: PASS

**Step 7: Commit**

```
refactor(machine): migrate sub-context pool to Pool[T]

All three pools (stack, continuation, sub-context) now use generic
Pool[T]. ReleaseSubContext preserves domain logic (parent counter
increment) as a wrapper around pool.Release().
```

---

### Task 7: Add pool stats assertions to existing tests

**Files:**
- Modify: `machine/pool_test.go` (add stats checks to existing tests)

**Step 1: Add stats assertions to roundtrip tests**

Add stats assertions to the existing tests in `pool_test.go` to verify counters are tracked. For example, after `TestStackPool_Roundtrip`:

```go
func TestStackPool_StatsTracked(t *testing.T) {
	// Read baseline stats (pools are shared across tests, so use relative checks).
	before := stackPool.Stats()

	s := acquireStack()
	s.Push(values.NewInteger(1))
	releaseStack(s)

	after := stackPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestContinuationPool_StatsTracked(t *testing.T) {
	before := continuationPool.Stats()

	cont := acquireContinuation()
	releaseContinuation(cont)

	after := continuationPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestSubContextPool_StatsTracked(t *testing.T) {
	before := subContextPool.Stats()

	mc := acquireSubContext()
	ReleaseSubContext(mc)

	after := subContextPool.Stats()
	qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))
	qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
}

func TestPoolManager_AllStats_ReportsAllPools(t *testing.T) {
	stats := pools.AllStats()
	qt.Assert(t, len(stats), qt.Equals, 3)

	names := make(map[string]bool)
	for _, s := range stats {
		names[s.Name] = true
	}
	qt.Assert(t, names["stack"], qt.IsTrue)
	qt.Assert(t, names["sub_context"], qt.IsTrue)
	qt.Assert(t, names["continuation"], qt.IsTrue)
}
```

**Step 2: Run all pool tests**

Run: `go test -v -run 'TestStackPool_Stats|TestContinuationPool_Stats|TestSubContextPool_Stats|TestPoolManager_AllStats_Reports' ./machine/...`
Expected: PASS

**Step 3: Run full test suite**

Run: `go test -v ./machine/...`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```
test(machine): add pool stats assertions to existing pool tests

Verifies that Pool[T] atomic counters track acquire/release
operations through the existing acquireStack/releaseStack etc.
wrapper functions.
```

---

### Task 8: Run Gabriel benchmarks to measure overhead

This task measures whether the atomic counter overhead in `Pool[T]` affects performance.

**Step 1: Run Gabriel benchmark comparison**

Run: `make bench-gabriel-compare`
Expected: Results should be within noise (~1-2%). If any benchmark regresses >3%, investigate.

**Step 2: Document results**

Add a results section to `plans/UNIFIED_POOL_MANAGER.md`:

```markdown
## Benchmark Results

Date: YYYY-MM-DD

| Benchmark | Before (ms) | After (ms) | Delta |
|-----------|------------|-----------|-------|
| ... | ... | ... | ... |

Conclusion: [acceptable / needs investigation]
```

**Step 3: Commit benchmark results**

```
docs(plans): add benchmark results for unified pool migration
```

---

### Task 9: Final cleanup and full regression

**Step 1: Run full test suite**

Run: `make test`
Expected: PASS

**Step 2: Run lint**

Run: `make lint`
Expected: PASS

**Step 3: Verify no call site changes needed**

Run: `grep -rn 'stackPool\.\|subContextPool\.\|continuationPool\.' machine/ --include='*.go' | grep -v pool.go | grep -v pool_generic.go | grep -v _test.go`
Expected: No results — all direct pool access should be through the wrapper functions, not touching pool vars directly. If any hits appear in `machine_context.go` (e.g., `RestoreAndRelease`), verify they go through `releaseContinuation` / `releaseStack`.

**Step 4: Update design doc status**

In `plans/UNIFIED_POOL_MANAGER.md`, change status from "pending implementation" to "complete".

**Step 5: Commit**

```
docs(plans): mark unified pool manager as complete
```
