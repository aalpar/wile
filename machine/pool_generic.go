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
func (m *PoolManager) Register(h PoolHandle) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.pools = append(m.pools, h)
}

// AllStats returns a snapshot of every registered pool's counters.
func (m *PoolManager) AllStats() []PoolSnapshot {
	m.mu.RLock()
	defer m.mu.RUnlock()
	out := make([]PoolSnapshot, len(m.pools))
	for i, p := range m.pools {
		out[i] = p.Stats()
	}
	return out
}

// DrainAll triggers a garbage collection, which clears all sync.Pool
// instances, then calls Drain on each registered pool.
func (m *PoolManager) DrainAll() {
	runtime.GC()
	m.mu.RLock()
	defer m.mu.RUnlock()
	for _, p := range m.pools {
		p.Drain()
	}
}

// SetAllEnabled sets the enabled flag on every registered pool.
func (m *PoolManager) SetAllEnabled(on bool) {
	m.mu.RLock()
	defer m.mu.RUnlock()
	for _, p := range m.pools {
		p.SetEnabled(on)
	}
}

// String returns a tabular summary of all registered pools.
func (m *PoolManager) String() string {
	stats := m.AllStats()
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

// registerPool registers a Pool[T] with a PoolManager and returns it,
// enabling the var-init-chain pattern:
//
//	var myPool = registerPool(mgr, NewPool[MyType](...))
func registerPool[T any](mgr *PoolManager, p *Pool[T]) *Pool[T] {
	mgr.Register(p)
	return p
}
