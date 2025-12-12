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

const (
	MutexUnlocked       MutexState = iota // Not locked
	MutexLockedOwned                      // Locked with owner
	MutexLockedNotOwned                   // Locked without owner
	MutexAbandoned                        // Owner terminated while holding lock
)

func (s MutexState) String() string {
	switch s {
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
func (m *Mutex) ID() uint64 {
	return m.id
}

// Name returns the mutex's name
func (m *Mutex) Name() string {
	return m.name
}

// Specific returns the mutex's specific field
func (m *Mutex) Specific() Value {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.specific
}

// SetSpecific sets the mutex's specific field
func (m *Mutex) SetSpecific(v Value) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.specific = v
}

// State returns the current state of the mutex
func (m *Mutex) State() MutexState {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.state
}

// StateValue returns the state as a Scheme value
// Returns: 'not-owned, 'abandoned, or the owner thread
func (m *Mutex) StateValue() Value {
	m.mu.Lock()
	defer m.mu.Unlock()

	switch m.state {
	case MutexUnlocked:
		return NewSymbol("not-owned")
	case MutexLockedOwned:
		if m.owner != nil {
			return m.owner
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
func (m *Mutex) Owner() *Thread {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.owner
}

// Lock acquires the mutex with optional timeout and owner
// Returns true if acquired, false if timeout
func (m *Mutex) Lock(timeout *time.Duration, owner *Thread) (bool, error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	// Check for abandoned state
	if m.state == MutexAbandoned {
		// Clear abandoned state and acquire
		m.state = MutexLockedOwned
		m.owner = owner
		return true, &AbandonedMutexException{Mutex: m}
	}

	// If already unlocked, acquire immediately
	if m.state == MutexUnlocked {
		if owner != nil {
			m.state = MutexLockedOwned
			m.owner = owner
		} else {
			m.state = MutexLockedNotOwned
		}
		return true, nil
	}

	// Need to wait
	if timeout == nil {
		// Wait indefinitely
		for m.state != MutexUnlocked && m.state != MutexAbandoned {
			m.cond.Wait()
		}
		if m.state == MutexAbandoned {
			m.state = MutexLockedOwned
			m.owner = owner
			return true, &AbandonedMutexException{Mutex: m}
		}
		if owner != nil {
			m.state = MutexLockedOwned
			m.owner = owner
		} else {
			m.state = MutexLockedNotOwned
		}
		return true, nil
	}

	// Wait with timeout
	deadline := time.Now().Add(*timeout)
	for m.state != MutexUnlocked && m.state != MutexAbandoned {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			return false, nil // timeout
		}

		// Use a goroutine to implement timeout since sync.Cond doesn't support it natively
		done := make(chan struct{})
		go func() {
			select {
			case <-time.After(remaining):
				m.cond.Broadcast() // Wake up to check timeout
			case <-done:
			}
		}()
		m.cond.Wait()
		close(done)

		if time.Now().After(deadline) {
			return false, nil // timeout
		}
	}

	if m.state == MutexAbandoned {
		m.state = MutexLockedOwned
		m.owner = owner
		return true, &AbandonedMutexException{Mutex: m}
	}

	if owner != nil {
		m.state = MutexLockedOwned
		m.owner = owner
	} else {
		m.state = MutexLockedNotOwned
	}
	return true, nil
}

// Unlock releases the mutex
// If cv is provided, atomically unlock and wait on condition variable
func (m *Mutex) Unlock(cv *ConditionVariable, timeout *time.Duration) bool {
	m.mu.Lock()

	// Release the mutex
	m.state = MutexUnlocked
	m.owner = nil
	m.cond.Signal()

	if cv == nil {
		m.mu.Unlock()
		return true
	}

	// Atomically release mutex and wait on condition variable
	m.mu.Unlock()
	return cv.Wait(m, timeout)
}

// MarkAbandoned marks the mutex as abandoned (called when owner thread terminates)
func (m *Mutex) MarkAbandoned() {
	m.mu.Lock()
	defer m.mu.Unlock()

	if m.state == MutexLockedOwned || m.state == MutexLockedNotOwned {
		m.state = MutexAbandoned
		m.owner = nil
		m.cond.Broadcast()
	}
}

// Value interface implementation

func (m *Mutex) IsVoid() bool {
	return m == nil
}

func (m *Mutex) EqualTo(v Value) bool {
	other, ok := v.(*Mutex)
	if !ok {
		return false
	}
	return m == other // Mutex identity is reference equality
}

func (m *Mutex) SchemeString() string {
	if m == nil {
		return "#<mutex:void>"
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return fmt.Sprintf("#<mutex:%s id=%d state=%s>", m.name, m.id, m.state)
}
