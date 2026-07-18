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
	"time"
)

var (
	_ Value = (*ConditionVariable)(nil)

	// condVarIDCounter assigns each ConditionVariable a unique ID.
	condVarIDCounter atomic.Uint64
)

// ConditionVariable represents a Scheme condition variable (SRFI-18)
type ConditionVariable struct {
	id       uint64
	name     string
	specific Value // user data

	mu      sync.Mutex
	waitChs []chan struct{} // per-waiter notification channels
}

// NewConditionVariable creates a new condition variable
func NewConditionVariable(name string) *ConditionVariable {
	id := condVarIDCounter.Add(1)
	if name == "" {
		name = fmt.Sprintf("condvar-%d", id)
	}
	return &ConditionVariable{
		id:   id,
		name: name,
	}
}

// ID returns the condition variable's unique identifier
func (p *ConditionVariable) ID() uint64 {
	return p.id
}

// Name returns the condition variable's name
func (p *ConditionVariable) Name() string {
	return p.name
}

// Specific returns the condition variable's specific field
func (p *ConditionVariable) Specific() Value {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.specific
}

// SetSpecific sets the condition variable's specific field
func (p *ConditionVariable) SetSpecific(v Value) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.specific = v
}

// Signal wakes one waiting thread (FIFO order).
func (p *ConditionVariable) Signal() {
	p.mu.Lock()
	defer p.mu.Unlock()
	if len(p.waitChs) > 0 {
		close(p.waitChs[0])
		p.waitChs = p.waitChs[1:]
	}
}

// Broadcast wakes all waiting threads.
func (p *ConditionVariable) Broadcast() {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, ch := range p.waitChs {
		close(ch)
	}
	p.waitChs = nil
}

// Wait waits on the condition variable until signaled or (if timeout is non-nil)
// the timeout elapses. Returns true if signaled, false if timed out.
//
// The *Mutex argument is unused: atomic unlock-and-wait is owned by
// Mutex.UnlockContext, which enqueues the waiter (registerWaiter) before releasing
// the mutex. Wait itself registers and blocks in one step, which is correct only for
// a caller that holds no mutex.
func (p *ConditionVariable) Wait(_ *Mutex, timeout *time.Duration) bool {
	return p.blockOnWaiter(context.Background(), p.registerWaiter(), timeout)
}

// registerWaiter appends a fresh notification channel to the wait set and returns
// it. Separating registration from blocking lets Mutex.UnlockContext enqueue the
// waiter BEFORE releasing the mutex, closing the SRFI-18 lost-wakeup window in which
// a signal could land on an empty wait set.
func (p *ConditionVariable) registerWaiter() chan struct{} {
	ch := make(chan struct{})
	p.mu.Lock()
	p.waitChs = append(p.waitChs, ch)
	p.mu.Unlock()
	return ch
}

// blockOnWaiter parks until ch is closed by Signal/Broadcast, the timeout elapses,
// or ctx is cancelled. Returns true iff woken by a signal. On timeout or
// cancellation it deregisters ch; a removeWaiter miss means Signal/Broadcast already
// claimed ch at the boundary, so that case still reports signaled.
func (p *ConditionVariable) blockOnWaiter(ctx context.Context, ch chan struct{}, timeout *time.Duration) bool {
	if timeout == nil {
		select {
		case <-ch:
			return true
		case <-ctx.Done():
			return !p.removeWaiter(ch)
		}
	}

	timer := time.NewTimer(*timeout)
	defer timer.Stop()

	select {
	case <-ch:
		return true
	case <-ctx.Done():
		return !p.removeWaiter(ch)
	case <-timer.C:
		return !p.removeWaiter(ch)
	}
}

// removeWaiter removes ch from the wait set, reporting whether it was present. A
// false return means Signal/Broadcast already closed and removed ch.
func (p *ConditionVariable) removeWaiter(ch chan struct{}) bool {
	p.mu.Lock()
	defer p.mu.Unlock()
	for i, c := range p.waitChs {
		if c == ch {
			p.waitChs = append(p.waitChs[:i], p.waitChs[i+1:]...)
			return true
		}
	}
	return false
}

// WaiterCount returns the number of threads waiting on this condition variable.
func (p *ConditionVariable) WaiterCount() int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return len(p.waitChs)
}

// buf interface implementation

// IsVoid returns true if the condition variable is nil.
func (p *ConditionVariable) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the condition variables are the same object.
func (p *ConditionVariable) EqualTo(v Value) bool {
	other, ok := v.(*ConditionVariable)
	if !ok {
		return false
	}
	return p == other // Identity is reference equality
}

// SchemeString returns the Scheme representation of this condition variable.
func (p *ConditionVariable) SchemeString() string {
	if p == nil {
		return "#<condition-variable:void>"
	}
	return fmt.Sprintf("#<condition-variable:%s id=%d>", p.name, p.id)
}
