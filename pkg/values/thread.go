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
// StateSymbol backs the thread-state primitive. That primitive is a Wile
// extension, not SRFI-18: SRFI-18 specifies mutex-state but has no thread-state,
// so the name and this symbol vocabulary follow Gambit's.
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
	//
	// mutexesAbandoned is the termination barrier: once AbandonOwnedMutexes has
	// run, this thread can no longer accumulate ownership, so a mutex it
	// acquires afterwards is abandoned on the spot rather than added to a list
	// nobody will read again. See TrackMutex.
	ownedMutexes     []*Mutex
	mutexesAbandoned bool
	mutexMu          sync.Mutex // protects ownedMutexes and mutexesAbandoned
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
// It backs the thread-state primitive (a Wile extension, not SRFI-18).
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

// Join waits for the thread to terminate, with an optional timeout, and reports
// the thread's result.
//
// ctx belongs to the JOINING thread, not to p. Three things can end the wait and
// they are not interchangeable:
//
//   - p terminates: done closes and p's outcome is read.
//   - the timeout elapses: ErrJoinTimeout. A nil timeout channel blocks forever,
//     which is what makes the untimed form fall out of the same select.
//   - the joiner itself is cancelled — thread-terminate! on the JOINER, an
//     embedder deadline, a with-timeout: ErrOperationCancelled carrying the ctx
//     cause.
//
// The third arm is the one that was missing. Terminate on a started thread
// cancels its ctx but does not close its done channel (ownsDone is true only for
// a never-started thread), so a joiner parked in a bare <-p.done had no way to
// observe its own termination: it stayed parked, its done never closed, and its
// own joiner then timed out. The joinee's termination was always observable —
// that closes p.done — which is why the reading matters.
func (p *Thread) Join(ctx context.Context, timeout *time.Duration) (Value, error) {
	// A terminated joinee outranks the other two arms. With more than one channel
	// ready select picks at random, which would report a thread that has already
	// finished as a cancelled or timed-out join. The outcome exists; report it.
	select {
	case <-p.done:
		return p.terminalResult()
	default:
	}

	var deadline <-chan time.Time
	if timeout != nil {
		timer := time.NewTimer(*timeout)
		defer timer.Stop()
		deadline = timer.C
	}

	select {
	case <-p.done:
		// Thread completed
	case <-deadline:
		return nil, werr.ErrJoinTimeout
	case <-ctx.Done():
		return nil, werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, context.Cause(ctx),
			"Thread.Join: the joining thread was cancelled before %s terminated", p.name)
	}

	return p.terminalResult()
}

// terminalResult reads the outcome of a thread whose done channel has closed.
// Only reachable after that close, which is what makes p.outcome non-nil.
func (p *Thread) terminalResult() (Value, error) {
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

// TrackMutex adds a mutex to this thread's ownership tracking set.
// Called by mutex-lock! when a mutex is acquired with this thread as owner.
//
// Past the termination barrier there is no set left to add to, so it abandons
// on the spot instead. An acquisition tracked past that point would be held by
// a thread that has already terminated and would never be abandoned: the mutex
// stays locked forever and every untimed mutex-lock! on it parks with no wakeup
// to come.
//
// The barrier makes that structural rather than incidental. The VM closes the
// window today by accident — Terminate cancels the thread's context, the machine
// loop's ctx poll aborts, and the goroutine's exit defer abandons a second time
// — so anything that widens it reopens the hole: a primitive that blocks without
// watching ctx, a cleanup that outlives cancellation.
//
// MarkAbandoned is called with mutexMu released, keeping the Thread.mutexMu →
// Mutex.mu order this type already establishes in AbandonOwnedMutexes.
func (p *Thread) TrackMutex(m *Mutex) {
	p.mutexMu.Lock()
	if p.mutexesAbandoned {
		p.mutexMu.Unlock()
		m.MarkAbandoned()
		return
	}
	p.ownedMutexes = append(p.ownedMutexes, m)
	p.mutexMu.Unlock()
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
//
// It also raises the termination barrier, permanently: this thread will not
// accumulate ownership again, and TrackMutex abandons rather than tracks from
// here on. Calling it twice is ordinary — Terminate calls it while the goroutine
// is still unwinding, and the goroutine's exit defer calls it again after the
// dynamic-wind after thunks have had their chance to unlock cleanly. The second
// call finds an empty list and re-raises an already-raised barrier.
func (p *Thread) AbandonOwnedMutexes() {
	p.mutexMu.Lock()
	mutexes := p.ownedMutexes
	p.ownedMutexes = nil
	p.mutexesAbandoned = true
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

// Thread exception types for SRFI-18.
//
// These four objects (JoinTimeoutException, TerminatedThreadException,
// AbandonedMutexException, UncaughtException) are the conditions SRFI-18 names,
// and each is BOTH a Go error and a Scheme-visible Value. The dual role is the
// point: the condition originates deep in this package, where the only channel
// out is an error return (Mutex.LockContext, Thread.Join), but the spec requires
// the joining/locking thread to see a discriminable object its handler chain can
// test with join-timeout-exception? and friends. The threads extension bridges
// the two by matching the Go error with errors.As and handing the SAME object to
// machine.RaiseInPlace.
//
// Like UncaughtException, each is an opaque control-flow handle rather than a
// structural container: identity is pointer identity across eq?, eqv?, and
// equal?, matching the sibling SRFI-18 objects (Thread, Mutex,
// ConditionVariable). SRFI-18 defines no accessor on any of the three, so
// structural equality would buy nothing.

var (
	_ Value = (*JoinTimeoutException)(nil)
	_ Value = (*TerminatedThreadException)(nil)
	_ Value = (*AbandonedMutexException)(nil)
	_ Value = (*UncaughtException)(nil)

	_ error = (*JoinTimeoutException)(nil)
	_ error = (*TerminatedThreadException)(nil)
	_ error = (*AbandonedMutexException)(nil)
)

// JoinTimeoutException is raised when thread-join! times out
type JoinTimeoutException struct{}

func (p *JoinTimeoutException) Error() string {
	return "thread-join!: timeout"
}

// IsVoid reports whether the receiver is nil, per the default Value convention.
func (p *JoinTimeoutException) IsVoid() bool {
	return p == nil
}

// EqualTo compares by pointer identity; see the exception-types doc above.
func (p *JoinTimeoutException) EqualTo(v Value) bool {
	other, ok := v.(*JoinTimeoutException)
	return ok && p == other
}

// SchemeString renders the condition.
func (p *JoinTimeoutException) SchemeString() string {
	return "#<join-timeout-exception>"
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

// IsVoid reports whether the receiver is nil, per the default Value convention.
func (p *TerminatedThreadException) IsVoid() bool {
	return p == nil
}

// EqualTo compares by pointer identity; see the exception-types doc above.
func (p *TerminatedThreadException) EqualTo(v Value) bool {
	other, ok := v.(*TerminatedThreadException)
	return ok && p == other
}

// SchemeString renders the condition, naming the thread when one is attached.
// It reads p.Thread.name directly rather than calling Thread.SchemeString: the
// sibling renderer reads p.state without holding the lock its own mutators take,
// and a terminated-thread exception is displayed precisely while the thread it
// names is being reaped.
func (p *TerminatedThreadException) SchemeString() string {
	if p == nil || p.Thread == nil {
		return "#<terminated-thread-exception>"
	}
	return fmt.Sprintf("#<terminated-thread-exception %s>", p.Thread.name)
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

// IsVoid reports whether the receiver is nil, per the default Value convention.
func (p *AbandonedMutexException) IsVoid() bool {
	return p == nil
}

// EqualTo compares by pointer identity; see the exception-types doc above.
func (p *AbandonedMutexException) EqualTo(v Value) bool {
	other, ok := v.(*AbandonedMutexException)
	return ok && p == other
}

// SchemeString renders the condition and the mutex it names.
func (p *AbandonedMutexException) SchemeString() string {
	if p == nil || p.Mutex == nil {
		return "#<abandoned-mutex-exception>"
	}
	return fmt.Sprintf("#<abandoned-mutex-exception %s>", p.Mutex.SchemeString())
}
