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

var (
	_ Value = (*ConditionVariable)(nil)

	// Condition variable ID counter
	condVarIDCounter atomic.Uint64
)

// ConditionVariable represents a Scheme condition variable (SRFI-18)
type ConditionVariable struct {
	id       uint64
	name     string
	specific Value // user data

	mu      sync.Mutex
	cond    *sync.Cond
	waiters int // number of waiting threads
}

// NewConditionVariable creates a new condition variable
func NewConditionVariable(name string) *ConditionVariable {
	id := condVarIDCounter.Add(1)
	if name == "" {
		name = fmt.Sprintf("condvar-%d", id)
	}
	cv := &ConditionVariable{
		id:   id,
		name: name,
	}
	cv.cond = sync.NewCond(&cv.mu)
	return cv
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

// Signal wakes one waiting thread
func (p *ConditionVariable) Signal() {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.cond.Signal()
}

// Broadcast wakes all waiting threads
func (p *ConditionVariable) Broadcast() {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.cond.Broadcast()
}

// Wait waits on the condition variable
// The mutex must be held when calling Wait
// Returns true if signaled, false if timeout
func (p *ConditionVariable) Wait(_ *Mutex, timeout *time.Duration) bool {
	p.mu.Lock()
	p.waiters++
	p.mu.Unlock()

	defer func() {
		p.mu.Lock()
		p.waiters--
		p.mu.Unlock()
	}()

	if timeout == nil {
		// Wait indefinitely
		p.mu.Lock()
		p.cond.Wait()
		p.mu.Unlock()
		return true
	}

	// Wait with timeout
	result := make(chan bool, 1)
	timedout := make(chan struct{})
	done := make(chan struct{})
	timer := time.NewTimer(*timeout)
	defer timer.Stop()

	// Waiter goroutine
	go func() {
		p.mu.Lock()
		p.cond.Wait()
		p.mu.Unlock()

		// Try to send result (non-blocking)
		select {
		case result <- true:
			// Success: main goroutine received signal
		default:
			// Timeout already fired, channel full
			// Goroutine exits cleanly
		}
	}()

	// Timeout handler goroutine
	go func() {
		select {
		case <-timer.C:
			// Timeout fired - wake the waiter so it can exit
			p.mu.Lock()
			p.cond.Broadcast()
			p.mu.Unlock()
			close(timedout)
		case <-done:
			// Signaled before timeout - exit cleanly
		}
	}()

	select {
	case <-result:
		close(done)
		return true
	case <-timedout:
		close(done)
		return false
	}
}

// WaiterCount returns the number of threads waiting on this condition variable
func (p *ConditionVariable) WaiterCount() int {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.waiters
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
