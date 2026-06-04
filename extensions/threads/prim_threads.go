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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
	mc := cc.(*machine.MachineContext)
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

		// Run the thunk
		err = sub.Run()
		if err != nil {
			return nil, err
		}

		return sub.GetValue(), nil
	}

	mc.SetValue(thread)
	return nil
}

// PrimThreadName returns the thread's name
// (thread-name thread) -> string or symbol
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
		return mc.Context().Err()
	}
	mc.SetValue(values.Void)
	return nil
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

	result, err := thread.Join(timeout)
	if err != nil {
		// Check for timeout
		if errors.Is(err, werr.ErrJoinTimeout) {
			if timeoutVal != nil {
				mc.SetValue(timeoutVal)
				return nil
			}
			return &values.JoinTimeoutException{}
		}
		return err
	}

	mc.SetValue(values.ValueOrVoid(result))
	return nil
}

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
// (mutex-name mutex) -> string or symbol
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
// Returns: 'not-owned, 'abandoned, or the owner thread
var PrimMutexState = helpers.MakeUnaryAccessor(werr.ErrNotAMutex, "mutex-state", func(mutex *values.Mutex) values.Value {
	return mutex.StateValue()
})

// PrimMutexLock acquires the mutex
// (mutex-lock! mutex [timeout [thread]]) -> boolean
// Returns #t if acquired, #f if timeout
func PrimMutexLock(mc machine.CallContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-lock!")
	if err != nil {
		return err
	}
	restVal := mc.Arg(1)

	var timeout *time.Duration
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

	acquired, err := mutex.Lock(timeout, owner)
	if err != nil {
		// Check for abandoned mutex exception
		_, ok := err.(*values.AbandonedMutexException)
		if ok {
			// Still acquired, but signal the exception
			if owner != nil {
				owner.TrackMutex(mutex)
			}
			mc.SetValue(values.TrueValue)
			return err
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

	result := mutex.Unlock(cv, timeout)
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
// (condition-variable-name cv) -> string or symbol
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
