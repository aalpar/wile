// Copyright 2025 Aaron Alpar
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

var (
	_ Value = (*Mutex)(nil)

	// Mutex ID counter
	mutexIDCounter uint64
)

// MutexState represents the state of a mutex
type MutexState int

// MutexState constants.
const (
	MutexUnlocked       MutexState = iota // Not locked
	MutexLockedOwned                      // Locked with owner
	MutexLockedNotOwned                   // Locked without owner
	MutexAbandoned                        // Owner terminated while holding lock
)

func (p MutexState) String() string {
	switch p {
	case MutexUnlocked:
		return "not-owned"
	case MutexLockedOwned:
		return "owned"
	case MutexLockedNotOwned:
		return "not-owned"
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
	id := atomic.AddUint64(&mutexIDCounter, 1)
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

// StateValue returns the state as a Scheme value
// Returns: 'not-owned, 'abandoned, or the owner thread
func (p *Mutex) StateValue() Value {
	p.mu.Lock()
	defer p.mu.Unlock()

	switch p.state {
	case MutexUnlocked:
		return NewSymbol("not-owned")
	case MutexLockedOwned:
		if p.owner != nil {
			return p.owner
		}
		return NewSymbol("not-owned")
	case MutexLockedNotOwned:
		return NewSymbol("not-owned")
	case MutexAbandoned:
		return NewSymbol("abandoned")
	default:
		return NewSymbol("not-owned")
	}
}

// Owner returns the current owner thread, or nil if not owned
func (p *Mutex) Owner() *Thread {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.owner
}

// Lock acquires the mutex with optional timeout and owner
// Returns true if acquired, false if timeout
func (p *Mutex) Lock(timeout *time.Duration, owner *Thread) (bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()

	// Check for abandoned state
	if p.state == MutexAbandoned {
		// Clear abandoned state and acquire
		p.state = MutexLockedOwned
		p.owner = owner
		return true, &AbandonedMutexException{Mutex: p}
	}

	// If already unlocked, acquire immediately
	if p.state == MutexUnlocked {
		if owner != nil {
			p.state = MutexLockedOwned
			p.owner = owner
		} else {
			p.state = MutexLockedNotOwned
		}
		return true, nil
	}

	// Need to wait
	if timeout == nil {
		// Wait indefinitely
		for p.state != MutexUnlocked && p.state != MutexAbandoned {
			p.cond.Wait()
		}
		if p.state == MutexAbandoned {
			p.state = MutexLockedOwned
			p.owner = owner
			return true, &AbandonedMutexException{Mutex: p}
		}
		if owner != nil {
			p.state = MutexLockedOwned
			p.owner = owner
		} else {
			p.state = MutexLockedNotOwned
		}
		return true, nil
	}

	// Wait with timeout
	deadline := time.Now().Add(*timeout)
	for p.state != MutexUnlocked && p.state != MutexAbandoned {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			return false, nil // timeout
		}

		// Use a goroutine to implement timeout since sync.Cond doesn't support it natively
		done := make(chan struct{})
		go func() {
			select {
			case <-time.After(remaining):
				p.cond.Broadcast() // Wake up to check timeout
			case <-done:
			}
		}()
		p.cond.Wait()
		close(done)

		if time.Now().After(deadline) {
			return false, nil // timeout
		}
	}

	if p.state == MutexAbandoned {
		p.state = MutexLockedOwned
		p.owner = owner
		return true, &AbandonedMutexException{Mutex: p}
	}

	if owner != nil {
		p.state = MutexLockedOwned
		p.owner = owner
	} else {
		p.state = MutexLockedNotOwned
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

// MarkAbandoned marks the mutex as abandoned (called when owner thread terminates)
func (p *Mutex) MarkAbandoned() {
	p.mu.Lock()
	defer p.mu.Unlock()

	if p.state == MutexLockedOwned || p.state == MutexLockedNotOwned {
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
