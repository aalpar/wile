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
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

// Mutex state symbol singletons.
//
// StateValue() returns these instead of allocating fresh symbols on each call.
// Same process-global vs per-VM identity subtlety as the thread state symbols
// — see the doc comment on SymbolThreadNew in thread.go for details.
var (
	SymbolMutexNotOwned  = NewSymbol("not-owned")
	SymbolMutexAbandoned = NewSymbol("abandoned")
)

var (
	_ Value = (*Mutex)(nil)

	// Mutex ID counter
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

// MutexState constants.
const (
	MutexUnlocked  MutexState = iota // Not locked; owner == nil
	MutexLocked                      // Held; owner is the acquirer, or nil if acquired without owner
	MutexAbandoned                   // Owner terminated while holding lock; owner == nil
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
// Returns package-level singletons for symbol states so that repeated calls
// return the same pointer: (eq? (mutex-state m) (mutex-state m)) → #t.
// See the doc comment on SymbolThreadNew in thread.go for eq? vs equal? caveats.
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

// Lock acquires the mutex with optional timeout and owner.
// Returns true if acquired, false if timeout.
//
// When acquired, state becomes MutexLocked and owner is set to whatever
// the caller supplied (nil produces a "locked-but-unowned" mutex, valid
// per SRFI-18). Acquiring an abandoned mutex succeeds but returns
// *AbandonedMutexException so the caller can observe the prior owner's
// termination.
func (p *Mutex) Lock(timeout *time.Duration, owner *Thread) (bool, error) {
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
			p.cond.Wait()
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

// Unlock releases the mutex
// If cv is provided, atomically unlock and wait on condition variable
func (p *Mutex) Unlock(cv *ConditionVariable, timeout *time.Duration) bool {
	p.mu.Lock()

	// Release the mutex
	p.state = MutexUnlocked
	p.owner = nil
	p.cond.Signal()

	if cv == nil {
		p.mu.Unlock()
		return true
	}

	// Atomically release mutex and wait on condition variable
	p.mu.Unlock()
	return cv.Wait(p, timeout)
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
