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
	"sync/atomic"
)

// The three pool types differ only in their storage discipline, and each one
// exists for a measured reason:
//
//	Pool[T]           sync.Pool backing; GC clears it, so retention is bounded
//	FreeList[T]       mutex-guarded slice; survives GC, process-global fallback
//	unsyncFreeList[T] plain slice, no mutex, no atomics; per-thread hot path
//
// All three expose Acquire/Release/Stats and nothing else. There is no
// enable/disable toggle and no manager: both were carried for years with zero
// production callers, and the toggle's atomic.Bool load sat on every Acquire and
// every Release. Removing it follows PR #777, which removed the same class of
// unused synchronization from the per-thread path for −3.6% fib / −8.7%
// call-bound. If a benchmark harness ever needs to bypass a pool again, add the
// bypass to the harness, not to the hot path.

// Pool is a type-safe, observable object pool backed by sync.Pool.
// It wraps sync.Pool with atomic counters (acquires, releases, misses).
type Pool[T any] struct {
	name  string
	inner sync.Pool
	newFn func() *T
	reset func(*T)
	stats poolStats
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

// NewPool creates a Pool[T] with the given name, constructor, and reset function.
func NewPool[T any](name string, newFn func() *T, resetFn func(*T)) *Pool[T] {
	p := &Pool[T]{
		name:  name,
		newFn: newFn,
		reset: resetFn,
	}
	p.inner.New = func() any {
		p.stats.misses.Add(1)
		return newFn()
	}
	return p
}

// Acquire returns an object from the pool, allocating via newFn on a miss.
func (p *Pool[T]) Acquire() *T {
	p.stats.acquires.Add(1)
	return p.inner.Get().(*T)
}

// Release resets the object and returns it to the pool.
func (p *Pool[T]) Release(v *T) {
	p.stats.releases.Add(1)
	p.reset(v)
	p.inner.Put(v)
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

// FreeList is a type-safe, observable object pool backed by a mutex-guarded
// slice instead of sync.Pool. Unlike sync.Pool, the freelist is NOT cleared by
// the garbage collector, so recycled objects (and any capacity they retain)
// persist across GC cycles.
//
// Use FreeList when the GC feedback loop makes sync.Pool ineffective: high
// allocation rates trigger frequent GC, which clears sync.Pool, causing more
// allocations. FreeList breaks this loop at the cost of no automatic shrinkage.
type FreeList[T any] struct {
	name  string
	mu    sync.Mutex
	free  []*T
	newFn func() *T
	reset func(*T)
	stats poolStats
}

// NewFreeList creates a FreeList[T] with the given name, constructor, and reset
// function.
func NewFreeList[T any](name string, newFn func() *T, resetFn func(*T)) *FreeList[T] {
	q := &FreeList[T]{
		name:  name,
		newFn: newFn,
		reset: resetFn,
	}
	return q
}

// Acquire returns a recycled object from the freelist. If the freelist is
// empty, it calls newFn to allocate a new object.
func (p *FreeList[T]) Acquire() *T {
	p.stats.acquires.Add(1)
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
	p.stats.misses.Add(1)
	return p.newFn()
}

// Release resets the object and appends it to the freelist.
func (p *FreeList[T]) Release(v *T) {
	p.stats.releases.Add(1)
	p.reset(v)
	p.mu.Lock()
	p.free = append(p.free, v)
	p.mu.Unlock()
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

// unsyncFreeList is the single-goroutine variant of FreeList used by the
// per-thread pools (threadPools). It drops the mutex and the atomic stat
// counters that FreeList carries for the process-global pools — neither of
// which a per-thread freelist needs, because each one is owned by exactly one
// goroutine: a frame allocated by a thread is never released by another (the
// per-thread-pool invariant, pinned by TestUnsyncFreeList_ConcurrentDistinctPools
// and plans/2026-06-08-per-thread-pools-invariant.local.md). Removing that
// synchronization is the bulk of the env-frame pool round-trip cost on hot
// non-tail calls.
//
// Acquire/Release mirror FreeList's allocate-on-miss / reset-on-release
// semantics minus the synchronization.
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
