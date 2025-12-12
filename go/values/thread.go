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
	"context"
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

var (
	_ Value = (*Thread)(nil)

	// Thread ID counter
	threadIDCounter uint64

	// ErrJoinTimeout is returned when thread-join! times out
	ErrJoinTimeout = NewStaticError("thread-join!: timeout")

	// ErrThreadAlreadyStarted is returned when starting an already-started thread
	ErrThreadAlreadyStarted = NewStaticError("thread-start!: thread already started")

	// ErrCrossThreadContinuation is returned when invoking continuation from different thread
	ErrCrossThreadContinuation = NewStaticError("cannot invoke continuation from different thread")
)

// ThreadState represents the state of a thread
type ThreadState int

const (
	ThreadNew        ThreadState = iota // Created but not started
	ThreadRunnable                      // Running or ready to run
	ThreadBlocked                       // Waiting for mutex/cv/sleep
	ThreadTerminated                    // Finished execution
)

func (s ThreadState) String() string {
	switch s {
	case ThreadNew:
		return "new"
	case ThreadRunnable:
		return "runnable"
	case ThreadBlocked:
		return "blocked"
	case ThreadTerminated:
		return "terminated"
	default:
		return "unknown"
	}
}

// Thread represents a Scheme thread (SRFI-18)
type Thread struct {
	id       uint64
	name     string
	specific Value // thread-local storage

	// State management
	mu    sync.Mutex
	state ThreadState

	// Execution result
	result    Value // result when terminated normally
	exception error // exception if terminated abnormally

	// Go runtime integration
	ctx    context.Context
	cancel context.CancelFunc
	done   chan struct{}

	// The thunk to execute (set at creation)
	thunk Value

	// RunFunc is set by the machine package to actually run the thread
	// This avoids circular dependency between values and machine
	RunFunc func(ctx context.Context, thunk Value) (Value, error)
}

// NewThread creates a new thread that will execute the given thunk
func NewThread(thunk Value, name string) *Thread {
	id := atomic.AddUint64(&threadIDCounter, 1)
	if name == "" {
		name = fmt.Sprintf("thread-%d", id)
	}
	return &Thread{
		id:    id,
		name:  name,
		state: ThreadNew,
		thunk: thunk,
		done:  make(chan struct{}),
	}
}

// ID returns the thread's unique identifier
func (t *Thread) ID() uint64 {
	return t.id
}

// Name returns the thread's name
func (t *Thread) Name() string {
	return t.name
}

// Specific returns the thread's specific field (thread-local storage)
func (t *Thread) Specific() Value {
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.specific
}

// SetSpecific sets the thread's specific field
func (t *Thread) SetSpecific(v Value) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.specific = v
}

// State returns the current state of the thread
func (t *Thread) State() ThreadState {
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.state
}

// StateSymbol returns the state as a Scheme symbol
func (t *Thread) StateSymbol() *Symbol {
	t.mu.Lock()
	defer t.mu.Unlock()
	switch t.state {
	case ThreadNew:
		return NewSymbol("new")
	case ThreadRunnable:
		return NewSymbol("runnable")
	case ThreadBlocked:
		return NewSymbol("blocked")
	case ThreadTerminated:
		return NewSymbol("terminated")
	default:
		return NewSymbol("unknown")
	}
}

// Start begins execution of the thread
func (t *Thread) Start() error {
	t.mu.Lock()
	if t.state != ThreadNew {
		t.mu.Unlock()
		return ErrThreadAlreadyStarted
	}
	if t.RunFunc == nil {
		t.mu.Unlock()
		return NewForeignError("thread-start!: no run function set")
	}

	t.state = ThreadRunnable
	t.ctx, t.cancel = context.WithCancel(context.Background())
	t.mu.Unlock()

	go func() {
		defer close(t.done)
		defer func() {
			if r := recover(); r != nil {
				t.mu.Lock()
				t.state = ThreadTerminated
				t.exception = fmt.Errorf("thread panic: %v", r)
				t.mu.Unlock()
			}
		}()

		result, err := t.RunFunc(t.ctx, t.thunk)

		t.mu.Lock()
		t.state = ThreadTerminated
		if err != nil {
			t.exception = err
		} else {
			t.result = result
		}
		t.mu.Unlock()
	}()

	return nil
}

// Join waits for the thread to terminate with optional timeout
// Returns the thread's result or an error
func (t *Thread) Join(timeout *time.Duration) (Value, error) {
	if timeout == nil {
		<-t.done
	} else {
		select {
		case <-t.done:
			// Thread completed
		case <-time.After(*timeout):
			return nil, ErrJoinTimeout
		}
	}

	t.mu.Lock()
	defer t.mu.Unlock()

	if t.exception != nil {
		return nil, &UncaughtThreadException{Reason: t.exception}
	}
	return t.result, nil
}

// Terminate forcefully terminates the thread
func (t *Thread) Terminate() {
	t.mu.Lock()
	defer t.mu.Unlock()

	if t.state == ThreadTerminated {
		return
	}

	if t.cancel != nil {
		t.cancel()
	}
	t.state = ThreadTerminated
	t.exception = &TerminatedThreadException{Thread: t}
}

// Yield yields execution to other threads
func (t *Thread) Yield() {
	// In Go, this is a hint to the scheduler
	// runtime.Gosched() is called in the primitive
}

// Sleep pauses the thread for the given duration
func (t *Thread) Sleep(d time.Duration) {
	t.mu.Lock()
	t.state = ThreadBlocked
	t.mu.Unlock()

	time.Sleep(d)

	t.mu.Lock()
	if t.state == ThreadBlocked {
		t.state = ThreadRunnable
	}
	t.mu.Unlock()
}

// Done returns a channel that's closed when the thread terminates
func (t *Thread) Done() <-chan struct{} {
	return t.done
}

// Value interface implementation

func (t *Thread) IsVoid() bool {
	return t == nil
}

func (t *Thread) EqualTo(v Value) bool {
	other, ok := v.(*Thread)
	if !ok {
		return false
	}
	return t == other // Thread identity is reference equality
}

func (t *Thread) SchemeString() string {
	if t == nil {
		return "#<thread:void>"
	}
	return fmt.Sprintf("#<thread:%s id=%d state=%s>", t.name, t.id, t.state)
}

// Thread exception types for SRFI-18

// JoinTimeoutException is raised when thread-join! times out
type JoinTimeoutException struct{}

func (e *JoinTimeoutException) Error() string {
	return "thread-join!: timeout"
}

// TerminatedThreadException is raised when joining a terminated thread
type TerminatedThreadException struct {
	Thread *Thread
}

func (e *TerminatedThreadException) Error() string {
	if e.Thread != nil {
		return fmt.Sprintf("thread terminated: %s", e.Thread.name)
	}
	return "thread terminated"
}

// UncaughtThreadException wraps an exception that wasn't caught in a thread
type UncaughtThreadException struct {
	Reason error
}

func (e *UncaughtThreadException) Error() string {
	if e.Reason != nil {
		return fmt.Sprintf("uncaught exception in thread: %v", e.Reason)
	}
	return "uncaught exception in thread"
}

func (e *UncaughtThreadException) Unwrap() error {
	return e.Reason
}

// AbandonedMutexException is raised when a mutex owner terminates
type AbandonedMutexException struct {
	Mutex Value // *Mutex, but avoid circular import
}

func (e *AbandonedMutexException) Error() string {
	return "mutex abandoned by terminated thread"
}
