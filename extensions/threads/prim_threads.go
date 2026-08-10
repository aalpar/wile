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

// SRFI-18 Threading Primitives
// See: https://srfi.schemers.org/srfi-18/srfi-18.html

package threads

import (
	"context"
	"errors"
	"runtime"
	"time"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// parseTimeout extracts a timeout duration from a Scheme value.
// Accepts *values.Time (absolute), *values.Integer (seconds), *values.Float (seconds),
// or *values.Boolean (#f = no timeout). Returns nil for no timeout.
func parseTimeout(v values.Value, name string) (*time.Duration, error) {
	switch t := v.(type) {
	case *values.Time:
		d := time.Until(t.GoTime())
		return &d, nil
	case *values.Integer:
		d := time.Duration(t.Value) * time.Second
		return &d, nil
	case *values.Float:
		d := time.Duration(t.Value * float64(time.Second))
		return &d, nil
	case *values.Boolean:
		if !t.Value {
			return nil, nil
		}
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected time or number for timeout, got #t", name)
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected time or number for timeout, got %T", name, v)
	}
}

// =============================================================================
// Thread Primitives
// =============================================================================

// PrimCurrentThread returns the current executing thread.
// Returns the thread object if running inside a thread, or the symbol 'primordial
// for the main goroutine.
// (current-thread) -> thread
func PrimCurrentThread(mc machine.CallContext) error {
	thread := mc.Thread()
	if thread == nil {
		// Return primordial thread placeholder
		mc.SetValue(values.SymbolPrimordial)
	} else {
		mc.SetValue(thread)
	}
	return nil
}

// PrimThreadQ tests if an object is a thread
// (thread? obj) -> boolean
var PrimThreadQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Thread)
	return ok
})

// PrimMakeThread creates a new thread
// (make-thread thunk [name]) -> thread
func PrimMakeThread(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "make-thread")
	if err != nil {
		return err
	}
	thunk, ok := mc.Arg(0).(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"make-thread: thunk must be a procedure")
	}
	restVal := mc.Arg(1)

	name := helpers.OptionalName(restVal)

	thread := values.NewThread(thunk, name)

	// Capture parent state BEFORE creating the closure that will run in a different goroutine.
	// This avoids the data race in T4 from the architectural review, where mc.NewSubContext()
	// would access parent MachineContext fields from the child goroutine.
	params := mc.CaptureSubContextParams()

	// Set the run function that will execute the thunk
	thread.RunFunc = func(ctx context.Context, thunk values.Callable) (values.Value, error) {
		// Create a new machine context for this thread using captured parent state.
		// This is safe to call from a different goroutine because it doesn't access
		// the parent MachineContext fields.
		// Use the thread's own cancellable context (from Thread.Start) so that
		// thread-terminate! can stop the VM loop via context cancellation.
		params.Ctx = ctx
		sub := machine.NewThreadSubContext(params, thread)
		thread.CleanupFunc = func() {
			_ = sub.UnwindTo(0) // Run dynamic-wind after thunks on thread exit
		}
		_, err := sub.ApplyCallable(thunk)
		if err != nil {
			return nil, err
		}

		// Run the thunk under the thread's own DefaultPromptTag driver: a thread root
		// is a DefaultPromptTag owner (the thread's continuations are delimited here),
		// so it must use RunWithEscapeHandling — which installs DefaultPromptTag and
		// resolves the call/cc resume trampoline (ErrResumeContinuation) on this thread
		// — not RunWithinBoundary, which would re-raise the resume out of the thread.
		err = sub.RunWithEscapeHandling()
		if err != nil {
			return nil, err
		}

		return sub.GetValue(), nil
	}

	mc.SetValue(thread)
	return nil
}

// PrimThreadName returns the thread's name
// (thread-name thread) -> string
var PrimThreadName = helpers.MakeUnaryAccessor(werr.ErrNotAThread, "thread-name", func(thread *values.Thread) values.Value {
	return values.NewString(thread.Name())
})

// PrimThreadSpecific returns the thread's specific field
// (thread-specific thread) -> value
var PrimThreadSpecific = helpers.MakeUnaryAccessor(werr.ErrNotAThread, "thread-specific", func(thread *values.Thread) values.Value {
	return values.ValueOrVoid(thread.Specific())
})

// PrimThreadSpecificSet sets the thread's specific field
// (thread-specific-set! thread obj) -> void
var PrimThreadSpecificSet = helpers.MakeBinarySetter(werr.ErrNotAThread, "thread-specific-set!", func(thread *values.Thread, val values.Value) {
	thread.SetSpecific(val)
})

// PrimThreadStart starts a thread
// (thread-start! thread) -> thread
//
// The registered thread-start! is NOT this function: addThreads binds a
// per-engine variant that also records the thread for Engine.Close (close.go).
// An embedder registering this one directly gets a thread nobody terminates.
func PrimThreadStart(mc machine.CallContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-start!")
	if err != nil {
		return err
	}

	err = thread.Start(mc.Context())
	if err != nil {
		return werr.WrapForeignErrorf(err, "thread-start!")
	}

	mc.SetValue(thread)
	return nil
}

// PrimThreadYield yields execution to other threads
// (thread-yield!) -> void
func PrimThreadYield(mc machine.CallContext) error {
	runtime.Gosched()
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadSleep pauses execution for a time
// (thread-sleep! timeout) -> void
// timeout can be a time object or a number (seconds)
func PrimThreadSleep(mc machine.CallContext) error {
	o := mc.Arg(0)

	var d time.Duration

	switch v := o.(type) {
	case *values.Time:
		// Sleep until the specified time
		d = max(time.Until(v.GoTime()), 0)
	case *values.Integer:
		d = time.Duration(v.Value) * time.Second
	case *values.Float:
		d = time.Duration(v.Value * float64(time.Second))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "thread-sleep!: expected time or number, got %T", o)
	}

	select {
	case <-time.After(d):
	case <-mc.Context().Done():
		return waitCancelled(mc, "thread-sleep!")
	}
	mc.SetValue(values.Void)
	return nil
}

// waitCancelled reports a blocking primitive in this extension whose context was
// cancelled before the wait completed. Which of the two outcomes applies is
// decided by the cancellation SOURCE, and the two callers — thread-sleep! and
// thread-join! — need it for the same reason: neither has a spare value channel.
// thread-sleep! returns Void; thread-join! returns the joined thread's result.
// mutex-lock!, whose #f already means "did not acquire", needs none of this.
//
//   - Timer expiry (with-timeout). Return error-free. callForeignCached's eager
//     ErrTimerExpired recheck runs only on the error-free foreign return, and it
//     is what raises the ErrTimerInterrupt that dispatches the with-timeout
//     handler. Returning an error here instead routes through
//     applyCallableError, which turns it into an ordinary Scheme condition: with
//     no guard the whole evaluation aborts and the handler never runs, which was
//     the defect in both primitives. The Void is never observed — the interrupt
//     discards it.
//   - Anything else (the embedder cancelled the engine context, thread-terminate!
//     cancelled THIS thread). Report ErrOperationCancelled, with the raw ctx cause
//     retained so a host can still tell context.Canceled from
//     context.DeadlineExceeded. thread-sleep! used to return ctx.Err() raw, which
//     handed the embedder an error carrying no sentinel to match on.
func waitCancelled(mc machine.CallContext, site string) error {
	cause := context.Cause(mc.Context())
	if errors.Is(cause, machine.ErrTimerExpired) {
		mc.SetValue(values.Void)
		return nil
	}
	return werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, cause,
		"%s: cancelled before the wait completed", site)
}

// PrimThreadTerminate forcefully terminates a thread
// (thread-terminate! thread) -> void
var PrimThreadTerminate = helpers.MakeUnarySideEffect(werr.ErrNotAThread, "thread-terminate!", func(thread *values.Thread) {
	thread.Terminate()
})

// PrimThreadJoin waits for a thread to terminate
// (thread-join! thread [timeout [timeout-val]]) -> value
func PrimThreadJoin(mc machine.CallContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-join!")
	if err != nil {
		return err
	}
	restVal := mc.Arg(1)

	var timeout *time.Duration
	var timeoutVal values.Value

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "thread-join!: invalid rest argument")
		}

		// Parse timeout (first optional arg)
		var err error
		timeout, err = parseTimeout(restList.Car(), "thread-join!")
		if err != nil {
			return err
		}

		// Parse timeout-val (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			rest2List, ok := rest2.(values.Tuple)
			if ok {
				timeoutVal = rest2List.Car()
			}
		}
	}

	result, err := thread.Join(mc.Context(), timeout)

	// The joiner's own cancellation is the FOURTH outcome, and it is not an
	// SRFI-18 condition: the other three describe how the JOINEE ended, this one
	// says the joiner stopped waiting. It is therefore handled before
	// raiseJoinCondition rather than inside joinConditionFor, which maps joinee
	// outcomes only.
	//
	// How the four compose for a Scheme guard around the join:
	//
	//	(guard (e ((join-timeout-exception? e) ...)        ; TIMEOUT elapsed
	//	          ((terminated-thread-exception? e) ...)   ; joinee was terminated
	//	          ((uncaught-exception? e) ...)            ; joinee raised
	//	          ((error-object? e) ...))                 ; joiner was cancelled
	//	  (thread-join! t))
	//
	// The first three arrive as their own condition objects via RaiseInPlace. The
	// fourth arrives as an ordinary error-object, because ErrOperationCancelled is
	// a Go sentinel with no SRFI-18 condition to name it — SRFI-18 has none, the
	// spec never contemplated a cancellable joiner. Under a with-timeout the
	// fourth outcome does not reach the guard at all: waitCancelled returns
	// error-free and the timer interrupt runs the handler instead, exactly as it
	// does for a parked mutex-lock!.
	if errors.Is(err, werr.ErrOperationCancelled) {
		return waitCancelled(mc, "thread-join!")
	}
	if err != nil {
		return raiseJoinCondition(mc, err, timeoutVal)
	}

	mc.SetValue(values.ValueOrVoid(result))
	return nil
}

// raiseJoinCondition maps a non-nil Thread.Join error onto the SRFI-18 condition
// thread-join! raises in the JOINING thread, and raises it there.
//
// All three conditions reach Scheme through machine.RaiseInPlace rather than by
// being returned as Go errors. A returned error becomes a generic error-object,
// which the SRFI-18 predicates cannot discriminate; RaiseInPlace hands the
// condition object itself to this thread's %exception-handlers, so a guard around
// the join can test it with join-timeout-exception? / terminated-thread-exception?
// / uncaught-exception? and, in the last case, recover the original condition.
//
// Ordering is not arbitrary. The terminate check precedes the uncaught-exception
// check because Thread.setOutcome is write-once with the terminated exception
// beating the completion path (see its doc comment): when both could describe the
// outcome, SRFI-18 says the termination is what thread-join! reports.
func raiseJoinCondition(mc machine.CallContext, err error, timeoutVal values.Value) error {
	cond := joinConditionFor(err)
	if cond == nil {
		// Not an SRFI-18 condition — leave it to applyCallableError, which
		// already re-raises ordinary Go errors.
		return err
	}

	// Timeout with a supplied timeout-val is the one path that is not a raise:
	// SRFI-18 returns the value instead. Keyed on the mapped condition rather
	// than re-testing the error, so the timeout is classified in one place.
	_, isTimeout := cond.(*values.JoinTimeoutException)
	if isTimeout && timeoutVal != nil {
		mc.SetValue(timeoutVal)
		return nil
	}

	realMC, mcErr := machine.RequireMachineContext(mc, "thread-join!")
	if mcErr != nil {
		return mcErr
	}
	return machine.RaiseInPlace(realMC, cond, false)
}

// joinConditionFor projects a Thread.Join error onto the Scheme condition object
// SRFI-18 names for it, or nil when the error is not one of them.
//
// The terminated object is the very one stored in the thread's end-exception
// field, forwarded rather than rebuilt, so every joiner of that thread receives
// an eq? condition. The other two are minted per call: a join timeout is a
// property of this call, not of the thread, and the uncaught-exception wrapper
// exists to carry the escaping condition that uncaught-exception-reason returns.
func joinConditionFor(err error) values.Value {
	if errors.Is(err, werr.ErrJoinTimeout) {
		return &values.JoinTimeoutException{}
	}

	var termErr *values.TerminatedThreadException
	if errors.As(err, &termErr) {
		return termErr
	}

	// A thread that ended via an uncaught Scheme exception surfaces it here as an
	// *ErrExceptionEscape carrier (wrapped in *UncaughtThreadException). The
	// carrier is already-formed control flow, so applyCallableError would pass it
	// through untouched — straight past any guard around the join.
	var escErr *machine.ErrExceptionEscape
	if errors.As(err, &escErr) {
		return values.NewUncaughtException(escErr.Condition)
	}

	return nil
}

// PrimThreadState returns the thread's state as a symbol: new, runnable,
// blocked, or terminated. (thread-state thread) -> symbol
//
// NOT SRFI-18 — that spec has mutex-state but no thread-state. The name and the
// symbol vocabulary follow Gambit's thread-state.
var PrimThreadState = helpers.MakeUnaryAccessor(werr.ErrNotAThread, "thread-state", func(t *values.Thread) values.Value {
	return t.StateSymbol()
})

// PrimJoinTimeoutExceptionQ tests whether an object is an SRFI-18
// join-timeout-exception (the object thread-join! raises when its timeout is
// reached and no timeout-val was supplied).
// (join-timeout-exception? obj) -> boolean
var PrimJoinTimeoutExceptionQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.JoinTimeoutException)
	return ok
})

// PrimTerminatedThreadExceptionQ tests whether an object is an SRFI-18
// terminated-thread-exception (the object thread-join! raises when the joined
// thread died by thread-terminate!).
// (terminated-thread-exception? obj) -> boolean
var PrimTerminatedThreadExceptionQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.TerminatedThreadException)
	return ok
})

// PrimAbandonedMutexExceptionQ tests whether an object is an SRFI-18
// abandoned-mutex-exception (the object mutex-lock! raises when it acquires a
// mutex whose owner terminated while holding it).
// (abandoned-mutex-exception? obj) -> boolean
var PrimAbandonedMutexExceptionQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.AbandonedMutexException)
	return ok
})

// PrimUncaughtExceptionQ tests whether an object is an SRFI-18 uncaught-exception
// (the object thread-join! raises when a joined thread died via an uncaught
// exception). (uncaught-exception? obj) -> boolean
var PrimUncaughtExceptionQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.UncaughtException)
	return ok
})

// PrimUncaughtExceptionReason returns the original condition the joined thread
// raised. (uncaught-exception-reason uncaught-exception) -> value. ValueOrVoid
// guards a nil reason so a Go nil never reaches the value register.
var PrimUncaughtExceptionReason = helpers.MakeUnaryAccessor(werr.ErrNotAnUncaughtException, "uncaught-exception-reason", func(u *values.UncaughtException) values.Value {
	return values.ValueOrVoid(u.Reason)
})

// =============================================================================
// Mutex Primitives
// =============================================================================

// PrimMutexQ tests if an object is a mutex
// (mutex? obj) -> boolean
var PrimMutexQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Mutex)
	return ok
})

// PrimMakeMutex creates a new mutex
// (make-mutex [name]) -> mutex
func PrimMakeMutex(mc machine.CallContext) error {
	restVal := mc.Arg(0)

	name := helpers.OptionalName(restVal)

	mutex := values.NewMutex(name)
	mc.SetValue(mutex)
	return nil
}

// PrimMutexName returns the mutex's name
// (mutex-name mutex) -> string
var PrimMutexName = helpers.MakeUnaryAccessor(werr.ErrNotAMutex, "mutex-name", func(mutex *values.Mutex) values.Value {
	return values.NewString(mutex.Name())
})

// PrimMutexSpecific returns the mutex's specific field
// (mutex-specific mutex) -> value
var PrimMutexSpecific = helpers.MakeUnaryAccessor(werr.ErrNotAMutex, "mutex-specific", func(mutex *values.Mutex) values.Value {
	return values.ValueOrVoid(mutex.Specific())
})

// PrimMutexSpecificSet sets the mutex's specific field
// (mutex-specific-set! mutex obj) -> void
var PrimMutexSpecificSet = helpers.MakeBinarySetter(werr.ErrNotAMutex, "mutex-specific-set!", func(mutex *values.Mutex, val values.Value) {
	mutex.SetSpecific(val)
})

// PrimMutexState returns the mutex's state
// (mutex-state mutex) -> symbol or thread
//
// SRFI-18's four answers, all reachable:
//   - the owner thread — held, and a thread owns it
//   - 'not-owned       — HELD, by a caller with no Thread object. Two ways in,
//     both ordinary: PRIMORDIAL ownership (a top-level lock, because
//     mutex-lock!'s default owner is mc.Thread() and the primordial context
//     has none), or an explicit #f owner argument
//   - 'abandoned       — the owner terminated while holding it
//   - 'not-abandoned   — not held, and not abandoned
var PrimMutexState = helpers.MakeUnaryAccessor(werr.ErrNotAMutex, "mutex-state", func(mutex *values.Mutex) values.Value {
	return mutex.StateValue()
})

// PrimMutexLock acquires the mutex
// (mutex-lock! mutex [timeout [thread]]) -> boolean
// Returns #t if acquired, #f if timeout. Acquiring a mutex abandoned by a
// terminated owner returns #t and additionally signals an abandoned-mutex
// exception.
func PrimMutexLock(mc machine.CallContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-lock!")
	if err != nil {
		return err
	}
	restVal := mc.Arg(1)

	var timeout *time.Duration
	// The default owner is the CALLING thread — and at the top level there
	// isn't one. MachineContext.SetThread is called from exactly one site
	// (NewThreadSubContext), so the primordial context's thread field is nil
	// and a top-level (mutex-lock! m) holds the mutex with NO owner. That is
	// not a defect: SRFI-18 names it, and mutex-state answers 'not-owned for
	// it. The same form inside a thread answers that thread. An explicit #f
	// third argument reaches the same state on purpose.
	owner := mc.Thread()

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "mutex-lock!: invalid rest argument")
		}

		// Parse timeout (first optional arg)
		var err error
		timeout, err = parseTimeout(restList.Car(), "mutex-lock!")
		if err != nil {
			return err
		}

		// Parse thread (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			rest2List, ok := rest2.(values.Tuple)
			if ok {
				threadArg := rest2List.Car()
				t, ok := threadArg.(*values.Thread)
				if ok { //nolint:gocritic // ifElseChain: type assertion + value check, not a switch candidate
					owner = t
				} else if values.ValueToBool(threadArg) {
					return werr.WrapForeignErrorf(werr.ErrNotAThread, "mutex-lock!: expected thread or #f for owner, got %T", threadArg)
				} else {
					owner = nil
				}
			}
		}
	}

	acquired, err := mutex.LockContext(mc.Context(), timeout, owner)
	if err != nil {
		// SRFI-18: "After changing the state of the mutex, an abandoned mutex
		// exception is raised if the mutex was abandoned before the state change."
		// The lock IS held on this path, so the ownership bookkeeping happens
		// first; only then does the condition escape, as a Scheme-visible object
		// raised into this thread's handlers (see raiseJoinCondition for why a
		// returned Go error will not do).
		var abandoned *values.AbandonedMutexException
		if errors.As(err, &abandoned) {
			if owner != nil {
				owner.TrackMutex(mutex)
			}
			realMC, mcErr := machine.RequireMachineContext(mc, "mutex-lock!")
			if mcErr != nil {
				return mcErr
			}
			return machine.RaiseInPlace(realMC, abandoned, false)
		}
		return err
	}

	if acquired && owner != nil {
		owner.TrackMutex(mutex)
	}
	mc.SetValue(values.BoolToBoolean(acquired))
	return nil
}

// PrimMutexUnlock releases the mutex
// (mutex-unlock! mutex [condition-variable [timeout]]) -> boolean
func PrimMutexUnlock(mc machine.CallContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-unlock!")
	if err != nil {
		return err
	}
	restVal := mc.Arg(1)

	var cv *values.ConditionVariable
	var timeout *time.Duration

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "mutex-unlock!: invalid rest argument")
		}

		// Parse condition-variable (first optional arg)
		cvArg := restList.Car()
		c, ok := cvArg.(*values.ConditionVariable)
		if ok {
			cv = c
		} else if values.ValueToBool(cvArg) {
			return werr.WrapForeignErrorf(werr.ErrNotAConditionVariable, "mutex-unlock!: expected condition-variable or #f, got %T", cvArg)
		}

		// Parse timeout (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			rest2List, ok := rest2.(values.Tuple)
			if ok {
				var err error
				timeout, err = parseTimeout(rest2List.Car(), "mutex-unlock!")
				if err != nil {
					return err
				}
			}
		}
	}

	// Untrack mutex from the owning thread before unlocking
	owner := mutex.Owner()
	if owner != nil {
		owner.UntrackMutex(mutex)
	}

	result := mutex.UnlockContext(mc.Context(), cv, timeout)
	mc.SetValue(values.BoolToBoolean(result))
	return nil
}

// =============================================================================
// Condition Variable Primitives
// =============================================================================

// PrimConditionVariableQ tests if an object is a condition variable
// (condition-variable? obj) -> boolean
var PrimConditionVariableQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.ConditionVariable)
	return ok
})

// PrimMakeConditionVariable creates a new condition variable
// (make-condition-variable [name]) -> condition-variable
func PrimMakeConditionVariable(mc machine.CallContext) error {
	restVal := mc.Arg(0)

	name := helpers.OptionalName(restVal)

	cv := values.NewConditionVariable(name)
	mc.SetValue(cv)
	return nil
}

// PrimConditionVariableName returns the condition variable's name
// (condition-variable-name cv) -> string
var PrimConditionVariableName = helpers.MakeUnaryAccessor(werr.ErrNotAConditionVariable, "condition-variable-name", func(cv *values.ConditionVariable) values.Value {
	return values.NewString(cv.Name())
})

// PrimConditionVariableSpecific returns the condition variable's specific field
// (condition-variable-specific cv) -> value
var PrimConditionVariableSpecific = helpers.MakeUnaryAccessor(werr.ErrNotAConditionVariable, "condition-variable-specific", func(cv *values.ConditionVariable) values.Value {
	return values.ValueOrVoid(cv.Specific())
})

// PrimConditionVariableSpecificSet sets the condition variable's specific field
// (condition-variable-specific-set! cv obj) -> void
var PrimConditionVariableSpecificSet = helpers.MakeBinarySetter(werr.ErrNotAConditionVariable, "condition-variable-specific-set!", func(cv *values.ConditionVariable, val values.Value) {
	cv.SetSpecific(val)
})

// PrimConditionVariableSignal signals one waiting thread
// (condition-variable-signal! cv) -> void
var PrimConditionVariableSignal = helpers.MakeUnarySideEffect(werr.ErrNotAConditionVariable, "condition-variable-signal!", func(cv *values.ConditionVariable) {
	cv.Signal()
})

// PrimConditionVariableBroadcast signals all waiting threads
// (condition-variable-broadcast! cv) -> void
var PrimConditionVariableBroadcast = helpers.MakeUnarySideEffect(werr.ErrNotAConditionVariable, "condition-variable-broadcast!", func(cv *values.ConditionVariable) {
	cv.Broadcast()
})

// =============================================================================
// Time Primitives
// =============================================================================

// PrimCurrentTime returns the current time
// (current-time) -> time
func PrimCurrentTime(mc machine.CallContext) error {
	mc.SetValue(values.CurrentTime())
	return nil
}

// PrimTimeQ tests if an object is a time
// (time? obj) -> boolean
var PrimTimeQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Time)
	return ok
})

// PrimTimeToSeconds converts a time to seconds
// (time->seconds time) -> number
var PrimTimeToSeconds = helpers.MakeUnaryAccessor(werr.ErrNotATime, "time->seconds", func(t *values.Time) values.Value {
	return values.NewFloat(t.Seconds())
})

// PrimSecondsToTime converts seconds to a time
// (seconds->time x) -> time
func PrimSecondsToTime(mc machine.CallContext) error {
	o := mc.Arg(0)

	var seconds float64
	switch v := o.(type) {
	case *values.Integer:
		seconds = float64(v.Value)
	case *values.Float:
		seconds = v.Value
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "seconds->time: expected number, got %T", o)
	}

	mc.SetValue(values.NewTimeFromSeconds(seconds))
	return nil
}
