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

// Thread state symbol singletons.
//
// StateSymbol() and PrimCurrentThread return these package-level singletons
// instead of allocating fresh symbols on each call. This guarantees
// self-consistency: (eq? (thread-state t) (thread-state t)) → #t.
//
// However, these are process-global pointers, not per-VM interned symbols.
// Reader-interned 'new (created via the parser gate) lives in the per-VM
// TopLevelEnvironment intern table and is a different pointer. Therefore:
//
//	(eq? (thread-state t) (thread-state t))  → #t  (same singleton)
//	(eq? (thread-state t) 'new)              → #f  (singleton ≠ interned)
//	(equal? (thread-state t) 'new)           → #t  (string comparison)
//
// SRFI-18 defines thread states as symbols but does not mandate eq? identity
// against reader-interned symbols, so this is conformant. The equal? path
// works because Symbol.EqualTo compares .Key strings.
//
// To make eq? work against reader-interned symbols, these singletons would
// need to participate in the per-VM intern table. That requires either:
//   - Lazily interning on first use (needs access to an environment)
//   - Registering these symbols during Engine initialization
//
// The values/ package cannot import environment/ (it's lower in the dependency
// graph), so this would need to be driven from engine.go or registry/. This
// is deferred until there is a concrete need.
var (
	SymbolThreadNew        = NewSymbol("new")
	SymbolThreadRunnable   = NewSymbol("runnable")
	SymbolThreadBlocked    = NewSymbol("blocked")
	SymbolThreadTerminated = NewSymbol("terminated")
	SymbolThreadUnknown    = NewSymbol("unknown")
	SymbolPrimordial       = NewSymbol("primordial")
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

// ThreadState constants.
const (
	ThreadNew        ThreadState = iota // Created but not started
	ThreadRunnable                      // Running or ready to run
	ThreadBlocked                       // Waiting for mutex/cv/sleep
	ThreadTerminated                    // Finished execution
)

func (p ThreadState) String() string {
	switch p {
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
	thunk Callable

	// RunFunc is set by the machine package to actually run the thread
	// This avoids circular dependency between values and machine
	RunFunc func(ctx context.Context, thunk Callable) (Value, error)

	// CleanupFunc is injected by the machine package to run dynamic-wind
	// after thunks (UnwindTo(0)) on thread exit. Called on both normal exit
	// and forced termination.
	CleanupFunc func()

	// ownedMutexes tracks mutexes currently owned by this thread.
	// On thread termination, all owned mutexes are marked as abandoned.
	ownedMutexes []*Mutex
	mutexMu      sync.Mutex // protects ownedMutexes
}

// NewThread creates a new thread that will execute the given thunk.
func NewThread(thunk Callable, name string) *Thread {
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
func (p *Thread) ID() uint64 {
	return p.id
}

// Context returns the context associated with this thread.
// Returns nil if the thread has not been started.
func (p *Thread) Context() context.Context {
	return p.ctx
}

// Name returns the thread's name
func (p *Thread) Name() string {
	return p.name
}

// Specific returns the thread's specific field (thread-local storage)
func (p *Thread) Specific() Value {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.specific
}

// SetSpecific sets the thread's specific field
func (p *Thread) SetSpecific(v Value) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.specific = v
}

// State returns the current state of the thread
func (p *Thread) State() ThreadState {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.state
}

// StateSymbol returns the state as a Scheme symbol.
// Returns package-level singletons so that repeated calls return the same pointer:
// (eq? (thread-state t) (thread-state t)) → #t.
// See the doc comment on SymbolThreadNew for eq? vs equal? caveats.
func (p *Thread) StateSymbol() *Symbol {
	p.mu.Lock()
	defer p.mu.Unlock()
	switch p.state {
	case ThreadNew:
		return SymbolThreadNew
	case ThreadRunnable:
		return SymbolThreadRunnable
	case ThreadBlocked:
		return SymbolThreadBlocked
	case ThreadTerminated:
		return SymbolThreadTerminated
	default:
		return SymbolThreadUnknown
	}
}

// Start begins execution of the thread.
// The parentCtx is used as the parent for the thread's cancellable context,
// enabling cancellation propagation from the engine/caller while allowing
// independent termination via thread-terminate!.
func (p *Thread) Start(parentCtx context.Context) error {
	p.mu.Lock()
	if p.state != ThreadNew {
		p.mu.Unlock()
		return ErrThreadAlreadyStarted
	}
	if p.RunFunc == nil {
		p.mu.Unlock()
		return WrapForeignErrorf(ErrInvalidArgument, "thread-start!: no run function set")
	}

	p.state = ThreadRunnable
	p.ctx, p.cancel = context.WithCancel(parentCtx)
	p.mu.Unlock()

	go func() {
		defer close(p.done)
		defer func() {
			// Run dynamic-wind after thunks first — they may include proper
			// mutex-unlock! calls that release mutexes cleanly.
			if p.CleanupFunc != nil {
				p.CleanupFunc()
			}
			// Abandon any mutexes still owned after cleanup ran.
			p.AbandonOwnedMutexes()
		}()
		defer func() {
			r := recover()
			if r != nil {
				p.mu.Lock()
				p.state = ThreadTerminated
				switch v := r.(type) {
				case error:
					p.exception = WrapForeignErrorWithCause(ErrThreadPanic, v, "thread %q: panic recovery", p.name)
				default:
					p.exception = WrapForeignErrorf(ErrThreadPanic, "thread %q: panic recovery: %v", p.name, r)
				}
				p.mu.Unlock()
			}
		}()

		result, err := p.RunFunc(p.ctx, p.thunk)

		p.mu.Lock()
		p.state = ThreadTerminated
		if err != nil {
			p.exception = err
		} else {
			p.result = result
		}
		p.mu.Unlock()
	}()

	return nil
}

// Join waits for the thread to terminate with optional timeout
// Returns the thread's result or an error
func (p *Thread) Join(timeout *time.Duration) (Value, error) {
	if timeout == nil {
		<-p.done
	} else {
		select {
		case <-p.done:
			// Thread completed
		case <-time.After(*timeout):
			return nil, ErrJoinTimeout
		}
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	if p.exception != nil {
		return nil, &UncaughtThreadException{Reason: p.exception}
	}
	return p.result, nil
}

// Terminate forcefully terminates the thread.
// Marks all owned mutexes as abandoned and cancels the thread's context.
// The deferred cleanup in the goroutine (dynamic-wind after thunks) will fire
// when the goroutine exits. However, AbandonOwnedMutexes is also called here
// directly because the goroutine may be blocked on a Go-level operation
// (e.g., sync.Cond.Wait) and won't exit immediately on context cancellation.
func (p *Thread) Terminate() {
	p.mu.Lock()
	defer p.mu.Unlock()

	if p.state == ThreadTerminated {
		return
	}

	// Mark mutexes abandoned even if goroutine is blocked
	p.AbandonOwnedMutexes()

	if p.cancel != nil {
		p.cancel()
	}
	p.state = ThreadTerminated
	p.exception = &TerminatedThreadException{Thread: p}
}

// Yield yields execution to other threads
func (p *Thread) Yield() {
	// In Go, this is a hint to the scheduler
	// runtime.Gosched() is called in the primitive
}

// Sleep pauses the thread for the given duration
func (p *Thread) Sleep(d time.Duration) {
	p.mu.Lock()
	p.state = ThreadBlocked
	p.mu.Unlock()

	time.Sleep(d)

	p.mu.Lock()
	if p.state == ThreadBlocked {
		p.state = ThreadRunnable
	}
	p.mu.Unlock()
}

// TrackMutex adds a mutex to this thread's ownership tracking set.
// Called by mutex-lock! when a mutex is acquired with this thread as owner.
func (p *Thread) TrackMutex(m *Mutex) {
	p.mutexMu.Lock()
	defer p.mutexMu.Unlock()
	p.ownedMutexes = append(p.ownedMutexes, m)
}

// UntrackMutex removes a mutex from this thread's ownership tracking set.
// Called by mutex-unlock! when a mutex is released.
func (p *Thread) UntrackMutex(m *Mutex) {
	p.mutexMu.Lock()
	defer p.mutexMu.Unlock()
	for i, owned := range p.ownedMutexes {
		if owned == m {
			p.ownedMutexes = append(p.ownedMutexes[:i], p.ownedMutexes[i+1:]...)
			return
		}
	}
}

// AbandonOwnedMutexes marks all mutexes owned by this thread as abandoned.
// Called during thread termination to ensure waiting threads are notified.
func (p *Thread) AbandonOwnedMutexes() {
	p.mutexMu.Lock()
	mutexes := p.ownedMutexes
	p.ownedMutexes = nil
	p.mutexMu.Unlock()

	for _, m := range mutexes {
		m.MarkAbandoned()
	}
}

// Done returns a channel that's closed when the thread terminates
func (p *Thread) Done() <-chan struct{} {
	return p.done
}

// buf interface implementation

// IsVoid returns true if this thread is nil.
func (p *Thread) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both threads are the same object.
func (p *Thread) EqualTo(v Value) bool {
	other, ok := v.(*Thread)
	if !ok {
		return false
	}
	return p == other // Thread identity is reference equality
}

// SchemeString returns the Scheme representation of this thread.
func (p *Thread) SchemeString() string {
	if p == nil {
		return "#<thread:void>"
	}
	return fmt.Sprintf("#<thread:%s id=%d state=%s>", p.name, p.id, p.state)
}

// Thread exception types for SRFI-18

// JoinTimeoutException is raised when thread-join! times out
type JoinTimeoutException struct{}

func (p *JoinTimeoutException) Error() string {
	return "thread-join!: timeout"
}

// TerminatedThreadException is raised when joining a terminated thread
type TerminatedThreadException struct {
	Thread *Thread
}

func (p *TerminatedThreadException) Error() string {
	if p.Thread != nil {
		return fmt.Sprintf("thread terminated: %s", p.Thread.name)
	}
	return "thread terminated"
}

// UncaughtThreadException wraps an exception that wasn't caught in a thread
type UncaughtThreadException struct {
	Reason error
}

func (p *UncaughtThreadException) Error() string {
	if p.Reason != nil {
		return fmt.Sprintf("uncaught exception in thread: %v", p.Reason)
	}
	return "uncaught exception in thread"
}

func (p *UncaughtThreadException) Unwrap() error {
	return p.Reason
}

// AbandonedMutexException is raised when a mutex owner terminates
type AbandonedMutexException struct {
	Mutex Value // *Mutex, but avoid circular import
}

func (p *AbandonedMutexException) Error() string {
	return "mutex abandoned by terminated thread"
}
