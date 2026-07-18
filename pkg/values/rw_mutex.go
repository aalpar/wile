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

import (
	"context"
	"fmt"
	"sync"
	"sync/atomic"
)

var (
	_ Value = (*RWMutex)(nil)

	// rwMutexIDCounter assigns each RWMutex a unique ID.
	rwMutexIDCounter atomic.Uint64
)

// RWMutex is a readers-writer lock for Scheme, owned entirely by Wile rather than
// wrapping sync.RWMutex. The reason is cancellation: sync.RWMutex.Lock/RLock have
// no ctx-aware form, so a thread parked acquiring one cannot be unparked by
// thread-terminate! and stalls the VM's teardown. Here acquisition parks on a
// cond via waitOnCondCtx and transitions the state under the guard mutex, so it
// is atomic with cancellation: a cancelled acquirer wakes and returns WITHOUT
// acquiring — no phantom hold that a wrapping goroutine would leak. A thread that
// already HOLDS the lock is unaffected: the wait side is cancellable, the held
// side is never force-released (that would expose the guarded resource out of
// serialization order). See docs/concurrency/cancellation.md.
//
// Writers are preferred: once a writer is waiting, new readers block, so a stream
// of readers cannot starve a writer. This differs slightly from sync.RWMutex's
// internal fairness and is a deliberate, self-contained choice.
type RWMutex struct {
	id   uint64
	name string

	mu            sync.Mutex
	cond          *sync.Cond
	readers       int
	writer        bool
	writerWaiting int
}

// NewRWMutex creates a new RWMutex
func NewRWMutex(name string) *RWMutex {
	id := rwMutexIDCounter.Add(1)
	if name == "" {
		name = fmt.Sprintf("rwmutex-%d", id)
	}
	q := &RWMutex{
		id:   id,
		name: name,
	}
	q.cond = sync.NewCond(&q.mu)
	return q
}

// ID returns the RWMutex's unique identifier
func (p *RWMutex) ID() uint64 {
	return p.id
}

// Name returns the RWMutex's name
func (p *RWMutex) Name() string {
	return p.name
}

// LockContext acquires the write lock, blocking until it is granted or ctx is
// cancelled. It reports true if the lock was acquired, false if ctx cancelled the
// wait first (in which case the lock is NOT held).
func (p *RWMutex) LockContext(ctx context.Context) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.writerWaiting++
	for p.writer || p.readers > 0 {
		if !waitOnCondCtx(ctx, p.cond) {
			p.writerWaiting--
			return false
		}
	}
	p.writerWaiting--
	p.writer = true
	return true
}

// Unlock releases the write lock
func (p *RWMutex) Unlock() {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.writer = false
	p.cond.Broadcast()
}

// RLockContext acquires a read lock, blocking until it is granted or ctx is
// cancelled. It reports true if the lock was acquired, false if ctx cancelled the
// wait first. Readers block while a writer holds or is waiting (writer priority).
func (p *RWMutex) RLockContext(ctx context.Context) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	for p.writer || p.writerWaiting > 0 {
		if !waitOnCondCtx(ctx, p.cond) {
			return false
		}
	}
	p.readers++
	return true
}

// RUnlock releases a read lock
func (p *RWMutex) RUnlock() {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.readers--
	if p.readers == 0 {
		p.cond.Broadcast()
	}
}

// TryLock tries to acquire the write lock without blocking
func (p *RWMutex) TryLock() bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.writer || p.readers > 0 {
		return false
	}
	p.writer = true
	return true
}

// TryRLock tries to acquire the read lock without blocking
func (p *RWMutex) TryRLock() bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.writer || p.writerWaiting > 0 {
		return false
	}
	p.readers++
	return true
}

// buf interface implementation

// IsVoid returns true if the RWMutex is nil.
func (p *RWMutex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the RWMutexes are the same object.
func (p *RWMutex) EqualTo(v Value) bool {
	other, ok := v.(*RWMutex)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the RWMutex.
func (p *RWMutex) SchemeString() string {
	if p == nil {
		return "#<rw-mutex:void>"
	}
	return fmt.Sprintf("#<rw-mutex:%s id=%d>", p.name, p.id)
}
