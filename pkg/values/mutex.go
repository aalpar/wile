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

// Mutex state symbol singletons.
//
// StateValue returns these instead of allocating fresh symbols on each call.
// The singletons avoid re-allocating the symbol; eq? on symbols is by name
// (see EqIdentity), so a singleton is eq? to a reader-produced 'not-owned.
var (
	SymbolMutexNotOwned  = NewSymbol("not-owned")
	SymbolMutexAbandoned = NewSymbol("abandoned")
)

var (
	_ Value = (*Mutex)(nil)

	// mutexIDCounter assigns each Mutex a unique ID.
	mutexIDCounter atomic.Uint64
)

// MutexState represents the lifecycle state of a mutex.
//
// The owned-vs-not-owned distinction (R7RS SRFI-18) is NOT a state — it's
// the contents of the owner field. Splitting "locked with owner" and
// "locked without owner" into separate states would force every site that
// reads state to also know which states permit owner != nil. Instead,
// MutexLocked is one state; owner = nil iff acquired without owner.
//
// Invariants enforced by Lock/Unlock/MarkAbandoned:
//
//	state == MutexUnlocked   ⇒ owner == nil
//	state == MutexLocked     — owner is the identity (nil ⇒ "not-owned")
//	state == MutexAbandoned  ⇒ owner == nil
type MutexState int

// MutexState constants. See the invariant block above for state↔owner relations.
const (
	MutexUnlocked  MutexState = iota // Not locked
	MutexLocked                      // Held
	MutexAbandoned                   // Owner terminated while holding lock
)

func (p MutexState) String() string {
	switch p {
	case MutexUnlocked:
		return "unlocked"
	case MutexLocked:
		return "locked"
	case MutexAbandoned:
		return "abandoned"
	default:
		return "unknown"
	}
}

// Mutex represents a Scheme mutex (SRFI-18)
type Mutex struct {
	id       uint64
	name     string
	specific Value // user data

	mu    sync.Mutex
	cond  *sync.Cond
	state MutexState
	owner *Thread // nil if not owned
}

// NewMutex creates a new unlocked mutex
func NewMutex(name string) *Mutex {
	id := mutexIDCounter.Add(1)
	if name == "" {
		name = fmt.Sprintf("mutex-%d", id)
	}
	m := &Mutex{
		id:    id,
		name:  name,
		state: MutexUnlocked,
	}
	m.cond = sync.NewCond(&m.mu)
	return m
}

// ID returns the mutex's unique identifier
func (p *Mutex) ID() uint64 {
	return p.id
}

// Name returns the mutex's name
func (p *Mutex) Name() string {
	return p.name
}

// Specific returns the mutex's specific field
func (p *Mutex) Specific() Value {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.specific
}

// SetSpecific sets the mutex's specific field
func (p *Mutex) SetSpecific(v Value) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.specific = v
}

// State returns the current state of the mutex
func (p *Mutex) State() MutexState {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.state
}

// StateValue returns the state as a Scheme value per R7RS SRFI-18.
// Returns package-level singletons for symbol states; the singletons avoid
// re-allocating the symbol, and eq? on symbols is by name (see EqIdentity).
// Returns: 'not-owned, 'abandoned, or the owner thread.
//
// SRFI-18 collapses "unlocked" and "locked without owner" into the single
// 'not-owned symbol — they are indistinguishable to Scheme. The Go-side
// distinction is preserved by MutexState (Unlocked is acquirable without
// blocking; Locked-without-owner is held by a non-thread caller).
func (p *Mutex) StateValue() Value {
	p.mu.Lock()
	defer p.mu.Unlock()

	switch p.state {
	case MutexUnlocked:
		return SymbolMutexNotOwned
	case MutexLocked:
		if p.owner != nil {
			return p.owner
		}
		return SymbolMutexNotOwned
	case MutexAbandoned:
		return SymbolMutexAbandoned
	default:
		return SymbolMutexNotOwned
	}
}

// Owner returns the current owner thread, or nil if not owned
func (p *Mutex) Owner() *Thread {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.owner
}

// Lock acquires the mutex with optional timeout and owner, without ctx
// cancellation. It is LockContext with a background ctx — for callers that have
// no ctx to thread (tests, internal helpers). VM primitives use LockContext so a
// terminated thread parked here is unparked.
func (p *Mutex) Lock(timeout *time.Duration, owner *Thread) (bool, error) {
	return p.LockContext(context.Background(), timeout, owner)
}

// LockContext acquires the mutex with optional timeout and owner.
// Returns true if acquired, false if the wait ended without acquiring (timeout or
// ctx cancellation).
//
// When acquired, state becomes MutexLocked and owner is set to whatever
// the caller supplied (nil produces a "locked-but-unowned" mutex, valid
// per SRFI-18). Acquiring an abandoned mutex succeeds but returns
// *AbandonedMutexException so the caller can observe the prior owner's
// termination.
//
// ctx cancellation unparks the untimed wait so a thread blocked here is reaped by
// thread-terminate! rather than stalling on a bare cond.Wait. A cancelled acquire
// surfaces to mutex-lock! as #f, error-free: the timed form already spends #f on
// "did not acquire", so cancellation needs no separate channel and no manufactured
// error. Error-free is the load-bearing half. callForeignCached's eager
// ErrTimerExpired recheck runs only on the error-free return path, so a wrapping
// with-timeout gets its handler run without this call site having to special-case
// the cancellation source (docs/concurrency/cancellation.md, "wait side vs held
// side"). The held side is untouched: a terminated holder's lock stays held
// (abandonment is a separate, owner-driven path via MarkAbandoned).
func (p *Mutex) LockContext(ctx context.Context, timeout *time.Duration, owner *Thread) (bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()

	// Fast paths: abandoned ⇒ acquire with notification; unlocked ⇒ acquire.
	if p.state == MutexAbandoned {
		p.state = MutexLocked
		p.owner = owner
		return true, &AbandonedMutexException{Mutex: p}
	}
	if p.state == MutexUnlocked {
		p.state = MutexLocked
		p.owner = owner
		return true, nil
	}

	// Slow path: wait for the lock to free, with or without deadline.
	if timeout == nil {
		for p.state != MutexUnlocked && p.state != MutexAbandoned {
			if !waitOnCondCtx(ctx, p.cond) {
				return false, nil // ctx cancelled: acquire abandoned, lock not held
			}
		}
	} else {
		deadline := time.Now().Add(*timeout)
		for p.state != MutexUnlocked && p.state != MutexAbandoned {
			remaining := time.Until(deadline)
			if remaining <= 0 {
				return false, nil // timeout
			}

			// sync.Cond doesn't support deadlines natively, so we
			// arrange a wakeup via Broadcast from a side goroutine.
			done := make(chan struct{})
			go func() {
				select {
				case <-time.After(remaining):
					p.cond.Broadcast()
				case <-done:
				}
			}()
			p.cond.Wait()
			close(done)

			if time.Now().After(deadline) {
				return false, nil // timeout
			}
		}
	}

	// Wait loop exited: state is MutexUnlocked or MutexAbandoned.
	abandoned := p.state == MutexAbandoned
	p.state = MutexLocked
	p.owner = owner
	if abandoned {
		return true, &AbandonedMutexException{Mutex: p}
	}
	return true, nil
}

// Unlock releases the mutex without ctx cancellation, delegating to UnlockContext
// with a background ctx — for callers with no ctx to thread (tests, internal
// helpers). VM primitives use UnlockContext so a thread parked on the condition
// variable is reaped by thread-terminate!.
func (p *Mutex) Unlock(cv *ConditionVariable, timeout *time.Duration) bool {
	return p.UnlockContext(context.Background(), cv, timeout)
}

// UnlockContext releases the mutex. If cv is non-nil it performs the SRFI-18 atomic
// unlock-and-wait: the waiter is enqueued on cv BEFORE the mutex is released, so an
// idiomatic signaller (which holds this mutex while changing the predicate) cannot
// signal an empty wait set and lose the wakeup. ctx cancellation unparks the cv wait
// so thread-terminate! reaps a thread blocked here.
//
// cv.mu and this mutex's internal lock are never held simultaneously, so no
// lock-order inversion arises. A non-idiomatic signaller that signals WITHOUT
// holding the mutex can still race ahead of registerWaiter; SRFI-18 requires the
// mutex be held, and closing that window would nest cv.mu inside p.mu and
// re-introduce a lock-order edge.
func (p *Mutex) UnlockContext(ctx context.Context, cv *ConditionVariable, timeout *time.Duration) bool {
	if cv == nil {
		p.mu.Lock()
		p.state = MutexUnlocked
		p.owner = nil
		p.cond.Signal()
		p.mu.Unlock()
		return true
	}

	// Enqueue on the cv while the mutex is still MutexLocked, then release: this pins
	// the waiter into the wait set before any signaller can acquire the mutex.
	ch := cv.registerWaiter()

	p.mu.Lock()
	p.state = MutexUnlocked
	p.owner = nil
	p.cond.Signal()
	p.mu.Unlock()

	return cv.blockOnWaiter(ctx, ch, timeout)
}

// MarkAbandoned marks the mutex as abandoned (called when owner thread terminates).
// Only mutexes in MutexLocked state can be abandoned — unlocked and already-
// abandoned mutexes are no-ops.
func (p *Mutex) MarkAbandoned() {
	p.mu.Lock()
	defer p.mu.Unlock()

	if p.state == MutexLocked {
		p.state = MutexAbandoned
		p.owner = nil
		p.cond.Broadcast()
	}
}

// buf interface implementation

// IsVoid returns true if the mutex is nil.
func (p *Mutex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the mutexes are the same object.
func (p *Mutex) EqualTo(v Value) bool {
	other, ok := v.(*Mutex)
	if !ok {
		return false
	}
	return p == other // Mutex identity is reference equality
}

// SchemeString returns the Scheme representation of the mutex.
func (p *Mutex) SchemeString() string {
	if p == nil {
		return "#<mutex:void>"
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	return fmt.Sprintf("#<mutex:%s id=%d state=%s>", p.name, p.id, p.state)
}
