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
	_ Value = (*ConditionVariable)(nil)

	// Condition variable ID counter
	condVarIDCounter uint64
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
	id := atomic.AddUint64(&condVarIDCounter, 1)
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
func (cv *ConditionVariable) ID() uint64 {
	return cv.id
}

// Name returns the condition variable's name
func (cv *ConditionVariable) Name() string {
	return cv.name
}

// Specific returns the condition variable's specific field
func (cv *ConditionVariable) Specific() Value {
	cv.mu.Lock()
	defer cv.mu.Unlock()
	return cv.specific
}

// SetSpecific sets the condition variable's specific field
func (cv *ConditionVariable) SetSpecific(v Value) {
	cv.mu.Lock()
	defer cv.mu.Unlock()
	cv.specific = v
}

// Signal wakes one waiting thread
func (cv *ConditionVariable) Signal() {
	cv.mu.Lock()
	defer cv.mu.Unlock()
	cv.cond.Signal()
}

// Broadcast wakes all waiting threads
func (cv *ConditionVariable) Broadcast() {
	cv.mu.Lock()
	defer cv.mu.Unlock()
	cv.cond.Broadcast()
}

// Wait waits on the condition variable
// The mutex must be held when calling Wait
// Returns true if signaled, false if timeout
func (cv *ConditionVariable) Wait(mutex *Mutex, timeout *time.Duration) bool {
	cv.mu.Lock()
	cv.waiters++
	cv.mu.Unlock()

	defer func() {
		cv.mu.Lock()
		cv.waiters--
		cv.mu.Unlock()
	}()

	if timeout == nil {
		// Wait indefinitely
		cv.mu.Lock()
		cv.cond.Wait()
		cv.mu.Unlock()
		return true
	}

	// Wait with timeout
	done := make(chan struct{})
	signaled := make(chan struct{})

	go func() {
		cv.mu.Lock()
		cv.cond.Wait()
		cv.mu.Unlock()
		close(signaled)
	}()

	go func() {
		select {
		case <-time.After(*timeout):
			cv.mu.Lock()
			cv.cond.Broadcast() // Wake the waiter so it can exit
			cv.mu.Unlock()
		case <-done:
		}
	}()

	select {
	case <-signaled:
		close(done)
		return true
	case <-time.After(*timeout):
		close(done)
		return false
	}
}

// WaiterCount returns the number of threads waiting on this condition variable
func (cv *ConditionVariable) WaiterCount() int {
	cv.mu.Lock()
	defer cv.mu.Unlock()
	return cv.waiters
}

// Value interface implementation

func (cv *ConditionVariable) IsVoid() bool {
	return cv == nil
}

func (cv *ConditionVariable) EqualTo(v Value) bool {
	other, ok := v.(*ConditionVariable)
	if !ok {
		return false
	}
	return cv == other // Identity is reference equality
}

func (cv *ConditionVariable) SchemeString() string {
	if cv == nil {
		return "#<condition-variable:void>"
	}
	return fmt.Sprintf("#<condition-variable:%s id=%d>", cv.name, cv.id)
}
