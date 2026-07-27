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

	"github.com/aalpar/wile/pkg/werr"
)

// Thread state symbol singletons.
//
// StateSymbol and PrimCurrentThread return these package-level singletons
// instead of allocating fresh symbols on each call.
//
// These are process-global pointers, but symbol identity in Wile is by name,
// not by pointer: EqIdentity compares *Symbol by .Key, so a singleton is eq?
// (and equal?) to a reader-produced symbol of the same name. The singletons
// exist only to avoid allocating a fresh *Symbol on every call. Observable via
// PrimCurrentThread, which yields SymbolPrimordial off a Thread:
// (eq? (current-thread) 'primordial) → #t.
//
// The state symbols themselves have no Scheme-level primitive today: nothing
// exposes StateSymbol, so SRFI-18's thread-state is not reachable from Scheme.
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

	// threadIDCounter assigns each Thread a unique ID.
	threadIDCounter atomic.Uint64
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

// threadOutcome holds the terminal result of a thread's execution.
// Nil until the thread terminates; then exactly one of value or err
// is meaningful (err != nil means abnormal termination).
type threadOutcome struct {
	value Value
	err   error
}

// Thread represents a Scheme thread (SRFI-18)
type Thread struct {
	id       uint64
	name     string
	specific Value // thread-local storage

	// State management
	mu    sync.Mutex
	state ThreadState

	// Execution outcome — nil until terminated, then non-nil.
	outcome *threadOutcome

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
	id := threadIDCounter.Add(1)
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
//
// Hazard: p.ctx is read without holding p.mu, while Start writes it under p.mu.
// Calling this concurrently with Start is a data race.
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

// StateSymbol returns the state as a Scheme symbol. Returns package-level
// singletons rather than fresh symbols; see the doc comment on SymbolThreadNew.
// StateSymbol has no Scheme-level primitive today.
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
		return werr.ErrThreadAlreadyStarted
	}
	if p.RunFunc == nil {
		p.mu.Unlock()
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "thread-start!: no run function set")
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
				// RecoverAsError applies the sentinel to error and non-error panic
				// values alike, chaining an error one as the cause, so this is the
				// whole of the identity the outcome needs: no second wrap, and no
				// ErrInternal layered under ErrThreadPanic for a runtime fault.
				p.setOutcome(&threadOutcome{err: werr.RecoverAsError(r, werr.ErrThreadPanic, fmt.Sprintf("thread %q", p.name))})
			}
		}()

		result, err := p.RunFunc(p.ctx, p.thunk)

		if err != nil {
			p.setOutcome(&threadOutcome{err: err})
		} else {
			p.setOutcome(&threadOutcome{value: result})
		}
	}()

	return nil
}

// setOutcome records the thread's final outcome and marks it terminated.
// The first writer wins; later writers are dropped.
//
// Normal completion, a panic, and Terminate race to be the thread's last word,
// and only the first of them is true. Once Terminate has stored the
// terminated-thread exception, the goroutine it cancelled is merely unwinding,
// and whatever that unwind produces is not a result: a thread parked in a
// cancellable operation returns a laundered ordinary value when its context is
// cancelled (mutex-lock! yields #f — see
// docs/concurrency/cancellation.md), and if that return sits in tail
// position, no further VM op runs to unwind it. Letting the completion path win
// there reports a terminated thread as having succeeded.
//
// SRFI-18: thread-terminate! stores a terminated-thread exception in the
// thread's end-exception field, which thread-join! then raises. Dropping a
// later writer can discard a panic raised while unwinding an already-terminated
// thread; the termination is the salient fact, and reporting the panic instead
// would lose the SRFI-18 outcome.
func (p *Thread) setOutcome(o *threadOutcome) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.setOutcomeLocked(o)
}

// setOutcomeLocked is setOutcome for callers already holding p.mu.
func (p *Thread) setOutcomeLocked(o *threadOutcome) {
	p.state = ThreadTerminated
	if p.outcome != nil {
		return
	}
	p.outcome = o
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
			return nil, werr.ErrJoinTimeout
		}
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	if p.outcome.err != nil {
		return nil, &UncaughtThreadException{Reason: p.outcome.err}
	}
	return p.outcome.value, nil
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

	// A thread still in ThreadNew has no goroutine and can never acquire one:
	// Start spawns only on the ThreadNew -> ThreadRunnable transition it makes
	// under this same lock, and the outcome recorded below leaves this thread
	// ThreadTerminated, which Start refuses. Closing done is therefore this
	// call's job — nothing else will ever do it, and Join parks on done before
	// it reads the outcome, so a never-started thread would otherwise block its
	// joiner forever while holding the very exception the joiner wants.
	//
	// The two closers are mutually exclusive: observing ThreadNew here means
	// Start has not transitioned yet, and once it has, this branch is
	// unreachable (no state returns to ThreadNew). So done is closed exactly
	// once without a sync.Once.
	ownsDone := p.state == ThreadNew

	// Mark mutexes abandoned even if goroutine is blocked
	p.AbandonOwnedMutexes()

	if p.cancel != nil {
		p.cancel()
	}
	p.setOutcomeLocked(&threadOutcome{err: &TerminatedThreadException{Thread: p}})

	if ownsDone {
		close(p.done)
	}
}

// Yield is a no-op placeholder. Yielding is done by the thread-yield!
// primitive, which calls runtime.Gosched directly; nothing calls this method.
func (p *Thread) Yield() {
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
//
// Hazard: it formats p.state without holding p.mu, unlike every other reader of
// that field (State, StateSymbol, Sleep, setOutcome) and unlike the sibling
// Mutex.SchemeString. Displaying a running thread therefore races with its own
// state transitions.
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

// UncaughtException is the SRFI-18 uncaught-exception object. thread-join! raises
// it into the joining thread when the joined thread terminated by raising an
// exception it did not handle; uncaught-exception-reason recovers the original
// condition. Unlike its Go-error sibling UncaughtThreadException, this is a
// Scheme-visible values.Value, so it can be handed to machine.RaiseInPlace.
//
// It is an opaque control-flow handle, not a structural container: identity is
// pointer identity across eq?, eqv?, AND equal?, matching the sibling SRFI-18
// objects (Thread, Mutex, ConditionVariable). Comparing by pointer keeps it
// Go-comparable regardless of Reason's dynamic type, and — because it is not a
// DeepEqualer — avoids the host-stack overflow a structural EqualTo would hit on
// a Reason cycle (equal.go documents that hazard for non-DeepEqualer recursive
// types). Each thread-join! mints a fresh wrapper, so structural equality would
// buy nothing.
type UncaughtException struct {
	Reason Value
}

// NewUncaughtException wraps the original raised condition in an SRFI-18
// uncaught-exception object.
func NewUncaughtException(reason Value) *UncaughtException {
	q := &UncaughtException{Reason: reason}
	return q
}

// IsVoid reports whether the receiver is nil, per the default Value convention.
func (p *UncaughtException) IsVoid() bool {
	return p == nil
}

// EqualTo compares by pointer identity: an uncaught-exception is equal only to
// itself. See the type doc for why this is a handle, not a structural container.
func (p *UncaughtException) EqualTo(v Value) bool {
	other, ok := v.(*UncaughtException)
	return ok && p == other
}

// SchemeString renders the wrapper and, for a human reader, the reason it carries.
func (p *UncaughtException) SchemeString() string {
	if p == nil || p.Reason == nil {
		return "#<uncaught-exception>"
	}
	return fmt.Sprintf("#<uncaught-exception %s>", p.Reason.SchemeString())
}

// AbandonedMutexException is raised when a mutex owner terminates
type AbandonedMutexException struct {
	Mutex Value // always *Mutex; typed as Value so Scheme can carry it
}

func (p *AbandonedMutexException) Error() string {
	return "mutex abandoned by terminated thread"
}
