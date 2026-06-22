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
	"fmt"
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
)

// Pool[T] is a type-safe, observable object pool backed by sync.Pool.
// It wraps sync.Pool with atomic counters (acquires, releases, misses)
// and an enable/disable toggle for debugging or benchmarking.
type Pool[T any] struct {
	name    string
	inner   sync.Pool
	newFn   func() *T
	reset   func(*T)
	stats   poolStats
	enabled atomic.Bool
}

// poolStats holds atomic counters for pool observability.
type poolStats struct {
	acquires atomic.Uint64
	releases atomic.Uint64
	misses   atomic.Uint64
}

// PoolSnapshot is a point-in-time view of a pool's counters.
type PoolSnapshot struct {
	Name     string
	Acquires uint64
	Releases uint64
	Misses   uint64
	InFlight uint64
}

// PoolHandle is the type-erased interface that PoolManager uses to
// observe and control heterogeneous pools.
type PoolHandle interface {
	Name() string
	Stats() PoolSnapshot
	Drain()
	SetEnabled(bool)
}

// NewPool creates a Pool[T] with the given name, constructor, and reset function.
// The pool starts enabled.
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

// Acquire returns an object from the pool. If the pool is disabled,
// it calls newFn directly (bypassing sync.Pool).
func (p *Pool[T]) Acquire() *T {
	p.stats.acquires.Add(1)
	if !p.enabled.Load() {
		return p.newFn()
	}
	return p.inner.Get().(*T)
}

// Release resets the object and returns it to the pool. If the pool is
// disabled, the object is discarded after reset.
func (p *Pool[T]) Release(v *T) {
	p.stats.releases.Add(1)
	p.reset(v)
	if p.enabled.Load() {
		p.inner.Put(v)
	}
}

// Name returns the pool's name.
func (p *Pool[T]) Name() string {
	return p.name
}

// Stats returns a point-in-time snapshot of the pool's counters.
func (p *Pool[T]) Stats() PoolSnapshot {
	acquires := p.stats.acquires.Load()
	releases := p.stats.releases.Load()
	var inFlight uint64
	if acquires > releases {
		inFlight = acquires - releases
	}
	return PoolSnapshot{
		Name:     p.name,
		Acquires: acquires,
		Releases: releases,
		Misses:   p.stats.misses.Load(),
		InFlight: inFlight,
	}
}

// Drain is a no-op on individual pools. Use PoolManager.DrainAll to
// trigger a GC-assisted drain across all pools.
func (p *Pool[T]) Drain() {
	// Individual pool drain is a no-op; sync.Pool is cleared by runtime.GC.
}

// SetEnabled toggles the pool on or off. When disabled, Acquire calls
// newFn directly and Release discards after reset, useful for
// benchmarking or debugging.
func (p *Pool[T]) SetEnabled(on bool) {
	p.enabled.Store(on)
}

// PoolManager tracks a collection of PoolHandle instances for unified
// observation and control.
type PoolManager struct {
	mu    sync.RWMutex
	pools []PoolHandle
}

// NewPoolManager creates an empty PoolManager.
func NewPoolManager() *PoolManager {
	return &PoolManager{}
}

// Register adds a pool to the manager.
func (p *PoolManager) Register(h PoolHandle) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.pools = append(p.pools, h)
}

// AllStats returns a snapshot of every registered pool's counters.
func (p *PoolManager) AllStats() []PoolSnapshot {
	p.mu.RLock()
	defer p.mu.RUnlock()
	out := make([]PoolSnapshot, len(p.pools))
	for i, h := range p.pools {
		out[i] = h.Stats()
	}
	return out
}

// DrainAll triggers a garbage collection, which clears all sync.Pool
// instances, then calls Drain on each registered pool.
func (p *PoolManager) DrainAll() {
	runtime.GC()
	p.mu.RLock()
	defer p.mu.RUnlock()
	for _, h := range p.pools {
		h.Drain()
	}
}

// SetAllEnabled sets the enabled flag on every registered pool.
func (p *PoolManager) SetAllEnabled(on bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()
	for _, h := range p.pools {
		h.SetEnabled(on)
	}
}

// String returns a tabular summary of all registered pools.
func (p *PoolManager) String() string {
	stats := p.AllStats()
	if len(stats) == 0 {
		return "PoolManager: (no pools registered)"
	}
	var b strings.Builder
	fmt.Fprintf(&b, "%-20s %10s %10s %10s %10s\n",
		"Pool", "Acquires", "Releases", "Misses", "InFlight")
	for _, s := range stats {
		fmt.Fprintf(&b, "%-20s %10d %10d %10d %10d\n",
			s.Name, s.Acquires, s.Releases, s.Misses, s.InFlight)
	}
	return b.String()
}

// FreeList[T] is a type-safe, observable object pool backed by a mutex-guarded
// slice instead of sync.Pool. Unlike sync.Pool, the freelist is NOT cleared by
// the garbage collector, so recycled objects (and any capacity they retain)
// persist across GC cycles.
//
// Use FreeList when the GC feedback loop makes sync.Pool ineffective: high
// allocation rates trigger frequent GC, which clears sync.Pool, causing more
// allocations. FreeList breaks this loop at the cost of no automatic shrinkage.
type FreeList[T any] struct {
	name    string
	mu      sync.Mutex
	free    []*T
	newFn   func() *T
	reset   func(*T)
	stats   poolStats
	enabled atomic.Bool
}

// NewFreeList creates a FreeList[T] with the given name, constructor, and reset
// function. The freelist starts enabled.
func NewFreeList[T any](name string, newFn func() *T, resetFn func(*T)) *FreeList[T] {
	q := &FreeList[T]{
		name:  name,
		newFn: newFn,
		reset: resetFn,
	}
	q.enabled.Store(true)
	return q
}

// Acquire returns a recycled object from the freelist. If the freelist is empty
// or disabled, it calls newFn to allocate a new object.
func (p *FreeList[T]) Acquire() *T {
	p.stats.acquires.Add(1)
	if p.enabled.Load() {
		p.mu.Lock()
		n := len(p.free)
		if n > 0 {
			v := p.free[n-1]
			p.free[n-1] = nil
			p.free = p.free[:n-1]
			p.mu.Unlock()
			return v
		}
		p.mu.Unlock()
	}
	p.stats.misses.Add(1)
	return p.newFn()
}

// Release resets the object and appends it to the freelist. If disabled,
// the object is discarded after reset.
func (p *FreeList[T]) Release(v *T) {
	p.stats.releases.Add(1)
	p.reset(v)
	if p.enabled.Load() {
		p.mu.Lock()
		p.free = append(p.free, v)
		p.mu.Unlock()
	}
}

// Name returns the freelist's name.
func (p *FreeList[T]) Name() string {
	return p.name
}

// Stats returns a point-in-time snapshot of the freelist's counters.
func (p *FreeList[T]) Stats() PoolSnapshot {
	acquires := p.stats.acquires.Load()
	releases := p.stats.releases.Load()
	var inFlight uint64
	if acquires > releases {
		inFlight = acquires - releases
	}
	return PoolSnapshot{
		Name:     p.name,
		Acquires: acquires,
		Releases: releases,
		Misses:   p.stats.misses.Load(),
		InFlight: inFlight,
	}
}

// Drain clears all cached objects from the freelist.
func (p *FreeList[T]) Drain() {
	p.mu.Lock()
	for i := range p.free {
		p.free[i] = nil
	}
	p.free = p.free[:0]
	p.mu.Unlock()
}

// SetEnabled toggles the freelist on or off. When disabled, Acquire calls
// newFn directly and Release discards after reset.
func (p *FreeList[T]) SetEnabled(on bool) {
	p.enabled.Store(on)
}

// unsyncFreeList is the single-goroutine variant of FreeList used by the
// per-thread pools (threadPools). It drops the mutex, the atomic stat counters,
// and the enabled flag that FreeList carries for the process-global pools —
// none of which a per-thread freelist needs, because each one is owned by
// exactly one goroutine: a frame allocated by a thread is never released by
// another (the per-thread-pool invariant,
// plans/2026-06-08-per-thread-pools-invariant.md). Removing that synchronization
// is the bulk of the env-frame pool round-trip cost on hot non-tail calls.
//
// Acquire/Release mirror FreeList's allocate-on-miss / reset-on-release
// semantics minus the synchronization. Per-thread pools are never disabled, so
// Release always recycles (no enabled check, no discard path).
// The acquires/releases/misses counters are plain (non-atomic) uint64s: a
// per-thread freelist is read and written by a single goroutine, so a memory
// barrier would be pure cost. Stats() must therefore be read from the owning
// goroutine.
type unsyncFreeList[T any] struct {
	free     []*T
	newFn    func() *T
	reset    func(*T)
	acquires uint64
	releases uint64
	misses   uint64
}

// newUnsyncFreeList creates a single-goroutine freelist with the given
// constructor and reset function.
func newUnsyncFreeList[T any](newFn func() *T, resetFn func(*T)) *unsyncFreeList[T] {
	return &unsyncFreeList[T]{
		newFn: newFn,
		reset: resetFn,
	}
}

// Acquire returns a recycled object, or a freshly allocated one when the
// freelist is empty. Single-goroutine: no lock, no atomics.
func (p *unsyncFreeList[T]) Acquire() *T {
	p.acquires++
	n := len(p.free)
	if n == 0 {
		p.misses++
		return p.newFn()
	}
	v := p.free[n-1]
	p.free[n-1] = nil
	p.free = p.free[:n-1]
	return v
}

// Release resets v and returns it to the freelist. Single-goroutine: no lock.
func (p *unsyncFreeList[T]) Release(v *T) {
	p.releases++
	p.reset(v)
	p.free = append(p.free, v)
}

// Stats returns a point-in-time snapshot of this freelist's counters. Single-
// goroutine: callers must read from the owning goroutine (per-thread invariant).
func (p *unsyncFreeList[T]) Stats() PoolSnapshot {
	var inFlight uint64
	if p.acquires > p.releases {
		inFlight = p.acquires - p.releases
	}
	return PoolSnapshot{
		Acquires: p.acquires,
		Releases: p.releases,
		Misses:   p.misses,
		InFlight: inFlight,
	}
}

// registerPool registers a Pool[T] with a PoolManager and returns it,
// enabling the var-init-chain pattern:
//
//	var myPool = registerPool(mgr, NewPool[MyType](...))
func registerPool[T any](mgr *PoolManager, p *Pool[T]) *Pool[T] {
	mgr.Register(p)
	return p
}

// registerFreeList registers a FreeList[T] with a PoolManager and returns it.
func registerFreeList[T any](mgr *PoolManager, fl *FreeList[T]) *FreeList[T] {
	mgr.Register(fl)
	return fl
}
