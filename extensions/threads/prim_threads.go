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

// parseOptionalName extracts an optional string or symbol name from a rest parameter list.
// Returns empty string if no name provided.
func parseOptionalName(rest values.Value) string {
	if values.IsEmptyList(rest) {
		return ""
	}
	restList, ok := rest.(values.Tuple)
	if !ok {
		return ""
	}
	switch v := restList.Car().(type) {
	case *values.String:
		return v.Value
	case *values.Symbol:
		return v.Key
	}
	return ""
}

// =============================================================================
// Thread Primitives
// =============================================================================

// PrimCurrentThread returns the current executing thread.
// Returns the thread object if running inside a thread, or the symbol 'primordial
// for the main goroutine.
// (current-thread) -> thread
func PrimCurrentThread(mc *machine.MachineContext) error {
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
func PrimThreadQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Thread)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimMakeThread creates a new thread
// (make-thread thunk [name]) -> thread
func PrimMakeThread(mc *machine.MachineContext) error {
	thunk, ok := mc.Arg(0).(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"make-thread: thunk must be a procedure")
	}
	restVal := mc.Arg(1)

	name := parseOptionalName(restVal)

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
func PrimThreadName(mc *machine.MachineContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-name")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(thread.Name()))
	return nil
}

// PrimThreadSpecific returns the thread's specific field
// (thread-specific thread) -> value
func PrimThreadSpecific(mc *machine.MachineContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-specific")
	if err != nil {
		return err
	}
	v := thread.Specific()
	if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimThreadSpecificSet sets the thread's specific field
// (thread-specific-set! thread obj) -> void
func PrimThreadSpecificSet(mc *machine.MachineContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-specific-set!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	thread.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadStart starts a thread
// (thread-start! thread) -> thread
func PrimThreadStart(mc *machine.MachineContext) error {
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
func PrimThreadYield(mc *machine.MachineContext) error {
	runtime.Gosched()
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadSleep pauses execution for a time
// (thread-sleep! timeout) -> void
// timeout can be a time object or a number (seconds)
func PrimThreadSleep(mc *machine.MachineContext) error {
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
func PrimThreadTerminate(mc *machine.MachineContext) error {
	thread, err := helpers.RequireArg[*values.Thread](mc, 0, werr.ErrNotAThread, "thread-terminate!")
	if err != nil {
		return err
	}

	thread.Terminate()
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadJoin waits for a thread to terminate
// (thread-join! thread [timeout [timeout-val]]) -> value
func PrimThreadJoin(mc *machine.MachineContext) error {
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

	if result == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(result)
	}
	return nil
}

// =============================================================================
// Mutex Primitives
// =============================================================================

// PrimMutexQ tests if an object is a mutex
// (mutex? obj) -> boolean
func PrimMutexQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Mutex)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimMakeMutex creates a new mutex
// (make-mutex [name]) -> mutex
func PrimMakeMutex(mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	name := parseOptionalName(restVal)

	mutex := values.NewMutex(name)
	mc.SetValue(mutex)
	return nil
}

// PrimMutexName returns the mutex's name
// (mutex-name mutex) -> string or symbol
func PrimMutexName(mc *machine.MachineContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-name")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(mutex.Name()))
	return nil
}

// PrimMutexSpecific returns the mutex's specific field
// (mutex-specific mutex) -> value
func PrimMutexSpecific(mc *machine.MachineContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-specific")
	if err != nil {
		return err
	}
	v := mutex.Specific()
	if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimMutexSpecificSet sets the mutex's specific field
// (mutex-specific-set! mutex obj) -> void
func PrimMutexSpecificSet(mc *machine.MachineContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-specific-set!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	mutex.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimMutexState returns the mutex's state
// (mutex-state mutex) -> symbol or thread
// Returns: 'not-owned, 'abandoned, 'not-abandoned, or the owner thread
func PrimMutexState(mc *machine.MachineContext) error {
	mutex, err := helpers.RequireArg[*values.Mutex](mc, 0, werr.ErrNotAMutex, "mutex-state")
	if err != nil {
		return err
	}
	mc.SetValue(mutex.StateValue())
	return nil
}

// PrimMutexLock acquires the mutex
// (mutex-lock! mutex [timeout [thread]]) -> boolean
// Returns #t if acquired, #f if timeout
func PrimMutexLock(mc *machine.MachineContext) error {
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
func PrimMutexUnlock(mc *machine.MachineContext) error {
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
func PrimConditionVariableQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.ConditionVariable)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimMakeConditionVariable creates a new condition variable
// (make-condition-variable [name]) -> condition-variable
func PrimMakeConditionVariable(mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	name := parseOptionalName(restVal)

	cv := values.NewConditionVariable(name)
	mc.SetValue(cv)
	return nil
}

// PrimConditionVariableName returns the condition variable's name
// (condition-variable-name cv) -> string or symbol
func PrimConditionVariableName(mc *machine.MachineContext) error {
	cv, err := helpers.RequireArg[*values.ConditionVariable](mc, 0, werr.ErrNotAConditionVariable, "condition-variable-name")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(cv.Name()))
	return nil
}

// PrimConditionVariableSpecific returns the condition variable's specific field
// (condition-variable-specific cv) -> value
func PrimConditionVariableSpecific(mc *machine.MachineContext) error {
	cv, err := helpers.RequireArg[*values.ConditionVariable](mc, 0, werr.ErrNotAConditionVariable, "condition-variable-specific")
	if err != nil {
		return err
	}
	v := cv.Specific()
	if v == nil {
		mc.SetValue(values.Void)
	} else {
		mc.SetValue(v)
	}
	return nil
}

// PrimConditionVariableSpecificSet sets the condition variable's specific field
// (condition-variable-specific-set! cv obj) -> void
func PrimConditionVariableSpecificSet(mc *machine.MachineContext) error {
	cv, err := helpers.RequireArg[*values.ConditionVariable](mc, 0, werr.ErrNotAConditionVariable, "condition-variable-specific-set!")
	if err != nil {
		return err
	}
	val := mc.Arg(1)

	cv.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableSignal signals one waiting thread
// (condition-variable-signal! cv) -> void
func PrimConditionVariableSignal(mc *machine.MachineContext) error {
	cv, err := helpers.RequireArg[*values.ConditionVariable](mc, 0, werr.ErrNotAConditionVariable, "condition-variable-signal!")
	if err != nil {
		return err
	}
	cv.Signal()
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableBroadcast signals all waiting threads
// (condition-variable-broadcast! cv) -> void
func PrimConditionVariableBroadcast(mc *machine.MachineContext) error {
	cv, err := helpers.RequireArg[*values.ConditionVariable](mc, 0, werr.ErrNotAConditionVariable, "condition-variable-broadcast!")
	if err != nil {
		return err
	}
	cv.Broadcast()
	mc.SetValue(values.Void)
	return nil
}

// =============================================================================
// Time Primitives
// =============================================================================

// PrimCurrentTime returns the current time
// (current-time) -> time
func PrimCurrentTime(mc *machine.MachineContext) error {
	mc.SetValue(values.CurrentTime())
	return nil
}

// PrimTimeQ tests if an object is a time
// (time? obj) -> boolean
func PrimTimeQ(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Time)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimTimeToSeconds converts a time to seconds
// (time->seconds time) -> number
func PrimTimeToSeconds(mc *machine.MachineContext) error {
	t, err := helpers.RequireArg[*values.Time](mc, 0, werr.ErrNotATime, "time->seconds")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewFloat(t.Seconds()))
	return nil
}

// PrimSecondsToTime converts seconds to a time
// (seconds->time x) -> time
func PrimSecondsToTime(mc *machine.MachineContext) error {
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
