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

	"wile/machine"
	"wile/utils"
	"wile/values"
)

// currentThread stores the thread for the current goroutine
// This is set when a thread starts execution
var currentThread *values.Thread

// SetCurrentThread sets the current thread for the goroutine
func SetCurrentThread(t *values.Thread) {
	currentThread = t
}

// GetCurrentThread returns the current thread
func GetCurrentThread() *values.Thread {
	return currentThread
}

// =============================================================================
// Thread Primitives
// =============================================================================

// PrimCurrentThread returns the current executing thread
// (current-thread) -> thread
func PrimCurrentThread(_ context.Context, mc *machine.MachineContext) error {
	if currentThread == nil {
		// Return primordial thread placeholder
		mc.SetValue(values.NewSymbol("primordial"))
	} else {
		mc.SetValue(currentThread)
	}
	return nil
}

// PrimThreadQ tests if an object is a thread
// (thread? obj) -> boolean
func PrimThreadQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Thread)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimMakeThread creates a new thread
// (make-thread thunk [name]) -> thread
func PrimMakeThread(_ context.Context, mc *machine.MachineContext) error {
	thunk := mc.Arg(0)
	restVal := mc.Arg(1)

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			nameVal := restList.Car()
			if s, ok := nameVal.(*values.String); ok {
				name = s.Value
			} else if sym, ok := nameVal.(*values.Symbol); ok {
				name = sym.Key
			}
		}
	}

	thread := values.NewThread(thunk, name)

	// Set the run function that will execute the thunk
	thread.RunFunc = func(ctx context.Context, thunk values.Value) (values.Value, error) {
		// Get closure
		cls, ok := thunk.(*machine.MachineClosure)
		if !ok {
			return nil, values.NewForeignError("make-thread: thunk must be a procedure")
		}

		// Create a new machine context for this thread.
		// Sub-contexts have isolated continuation chains, which is appropriate for threads.
		sub := mc.NewSubContext()
		if _, err := sub.Apply(cls); err != nil {
			return nil, err
		}

		// Run the thunk
		err := sub.Run()
		if err != nil {
			// Ignore machine halt, it's normal
			if !errors.Is(err, machine.ErrMachineHalt) {
				return nil, err
			}
		}

		return sub.GetValue(), nil
	}

	mc.SetValue(thread)
	return nil
}

// PrimThreadName returns the thread's name
// (thread-name thread) -> string or symbol
func PrimThreadName(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-name: expected thread, got %T", o)
	}
	mc.SetValue(values.NewString(thread.Name()))
	return nil
}

// PrimThreadSpecific returns the thread's specific field
// (thread-specific thread) -> value
func PrimThreadSpecific(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-specific: expected thread, got %T", o)
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
func PrimThreadSpecificSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	val := mc.Arg(1)

	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-specific-set!: expected thread, got %T", o)
	}

	thread.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadStart starts a thread
// (thread-start! thread) -> thread
func PrimThreadStart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-start!: expected thread, got %T", o)
	}

	err := thread.Start()
	if err != nil {
		return values.WrapForeignErrorf(err, "thread-start!")
	}

	mc.SetValue(thread)
	return nil
}

// PrimThreadYield yields execution to other threads
// (thread-yield!) -> void
func PrimThreadYield(_ context.Context, mc *machine.MachineContext) error {
	runtime.Gosched()
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadSleep pauses execution for a time
// (thread-sleep! timeout) -> void
// timeout can be a time object or a number (seconds)
func PrimThreadSleep(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)

	var d time.Duration

	switch v := o.(type) {
	case *values.Time:
		// Sleep until the specified time
		d = time.Until(v.GoTime())
		if d < 0 {
			d = 0
		}
	case *values.Integer:
		d = time.Duration(v.Value) * time.Second
	case *values.Float:
		d = time.Duration(v.Value * float64(time.Second))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "thread-sleep!: expected time or number, got %T", o)
	}

	time.Sleep(d)
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadTerminate forcefully terminates a thread
// (thread-terminate! thread) -> void
func PrimThreadTerminate(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-terminate!: expected thread, got %T", o)
	}

	thread.Terminate()
	mc.SetValue(values.Void)
	return nil
}

// PrimThreadJoin waits for a thread to terminate
// (thread-join! thread [timeout [timeout-val]]) -> value
func PrimThreadJoin(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	restVal := mc.Arg(1)

	thread, ok := o.(*values.Thread)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAThread, "thread-join!: expected thread, got %T", o)
	}

	var timeout *time.Duration
	var timeoutVal values.Value

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "thread-join!: invalid rest argument")
		}

		// Parse timeout (first optional arg)
		timeoutArg := restList.Car()
		switch v := timeoutArg.(type) {
		case *values.Time:
			d := time.Until(v.GoTime())
			timeout = &d
		case *values.Integer:
			d := time.Duration(v.Value) * time.Second
			timeout = &d
		case *values.Float:
			d := time.Duration(v.Value * float64(time.Second))
			timeout = &d
		case *values.Boolean:
			if !v.Value {
				// #f means no timeout
				timeout = nil
			}
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "thread-join!: expected time or number for timeout, got %T", timeoutArg)
		}

		// Parse timeout-val (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			if rest2List, ok := rest2.(*values.Pair); ok {
				timeoutVal = rest2List.Car()
			}
		}
	}

	result, err := thread.Join(timeout)
	if err != nil {
		// Check for timeout
		if err == values.ErrJoinTimeout {
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
func PrimMutexQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Mutex)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimMakeMutex creates a new mutex
// (make-mutex [name]) -> mutex
func PrimMakeMutex(_ context.Context, mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			nameVal := restList.Car()
			if s, ok := nameVal.(*values.String); ok {
				name = s.Value
			} else if sym, ok := nameVal.(*values.Symbol); ok {
				name = sym.Key
			}
		}
	}

	mutex := values.NewMutex(name)
	mc.SetValue(mutex)
	return nil
}

// PrimMutexName returns the mutex's name
// (mutex-name mutex) -> string or symbol
func PrimMutexName(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-name: expected mutex, got %T", o)
	}
	mc.SetValue(values.NewString(mutex.Name()))
	return nil
}

// PrimMutexSpecific returns the mutex's specific field
// (mutex-specific mutex) -> value
func PrimMutexSpecific(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-specific: expected mutex, got %T", o)
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
func PrimMutexSpecificSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	val := mc.Arg(1)

	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-specific-set!: expected mutex, got %T", o)
	}

	mutex.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimMutexState returns the mutex's state
// (mutex-state mutex) -> symbol or thread
// Returns: 'not-owned, 'abandoned, 'not-abandoned, or the owner thread
func PrimMutexState(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-state: expected mutex, got %T", o)
	}
	mc.SetValue(mutex.StateValue())
	return nil
}

// PrimMutexLock acquires the mutex
// (mutex-lock! mutex [timeout [thread]]) -> boolean
// Returns #t if acquired, #f if timeout
func PrimMutexLock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	restVal := mc.Arg(1)

	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-lock!: expected mutex, got %T", o)
	}

	var timeout *time.Duration
	owner := currentThread

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "mutex-lock!: invalid rest argument")
		}

		// Parse timeout (first optional arg)
		timeoutArg := restList.Car()
		switch v := timeoutArg.(type) {
		case *values.Time:
			d := time.Until(v.GoTime())
			timeout = &d
		case *values.Integer:
			d := time.Duration(v.Value) * time.Second
			timeout = &d
		case *values.Float:
			d := time.Duration(v.Value * float64(time.Second))
			timeout = &d
		case *values.Boolean:
			if !v.Value {
				timeout = nil
			}
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "mutex-lock!: expected time or number for timeout, got %T", timeoutArg)
		}

		// Parse thread (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			if rest2List, ok := rest2.(*values.Pair); ok {
				threadArg := rest2List.Car()
				if t, ok := threadArg.(*values.Thread); ok {
					owner = t
				} else if utils.ValueToBool(threadArg) {
					return values.WrapForeignErrorf(values.ErrNotAThread, "mutex-lock!: expected thread or #f for owner, got %T", threadArg)
				} else {
					owner = nil
				}
			}
		}
	}

	acquired, err := mutex.Lock(timeout, owner)
	if err != nil {
		// Check for abandoned mutex exception
		if _, ok := err.(*values.AbandonedMutexException); ok {
			// Still acquired, but signal the exception
			mc.SetValue(values.TrueValue)
			return err
		}
		return err
	}

	if acquired {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimMutexUnlock releases the mutex
// (mutex-unlock! mutex [condition-variable [timeout]]) -> boolean
func PrimMutexUnlock(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	restVal := mc.Arg(1)

	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-unlock!: expected mutex, got %T", o)
	}

	var cv *values.ConditionVariable
	var timeout *time.Duration

	// Parse optional arguments from rest list
	if !values.IsEmptyList(restVal) {
		restList, ok := restVal.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "mutex-unlock!: invalid rest argument")
		}

		// Parse condition-variable (first optional arg)
		cvArg := restList.Car()
		if c, ok := cvArg.(*values.ConditionVariable); ok {
			cv = c
		} else if utils.ValueToBool(cvArg) {
			return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "mutex-unlock!: expected condition-variable or #f, got %T", cvArg)
		}

		// Parse timeout (second optional arg)
		rest2 := restList.Cdr()
		if !values.IsEmptyList(rest2) {
			if rest2List, ok := rest2.(*values.Pair); ok {
				timeoutArg := rest2List.Car()
				switch v := timeoutArg.(type) {
				case *values.Time:
					d := time.Until(v.GoTime())
					timeout = &d
				case *values.Integer:
					d := time.Duration(v.Value) * time.Second
					timeout = &d
				case *values.Float:
					d := time.Duration(v.Value * float64(time.Second))
					timeout = &d
				case *values.Boolean:
					if !v.Value {
						timeout = nil
					}
				default:
					return values.WrapForeignErrorf(values.ErrNotANumber, "mutex-unlock!: expected time or number for timeout, got %T", timeoutArg)
				}
			}
		}
	}

	result := mutex.Unlock(cv, timeout)
	if result {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// =============================================================================
// Condition Variable Primitives
// =============================================================================

// PrimConditionVariableQ tests if an object is a condition variable
// (condition-variable? obj) -> boolean
func PrimConditionVariableQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.ConditionVariable)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimMakeConditionVariable creates a new condition variable
// (make-condition-variable [name]) -> condition-variable
func PrimMakeConditionVariable(_ context.Context, mc *machine.MachineContext) error {
	restVal := mc.Arg(0)

	name := ""
	// Parse optional name from rest list
	if !values.IsEmptyList(restVal) {
		if restList, ok := restVal.(*values.Pair); ok {
			nameVal := restList.Car()
			if s, ok := nameVal.(*values.String); ok {
				name = s.Value
			} else if sym, ok := nameVal.(*values.Symbol); ok {
				name = sym.Key
			}
		}
	}

	cv := values.NewConditionVariable(name)
	mc.SetValue(cv)
	return nil
}

// PrimConditionVariableName returns the condition variable's name
// (condition-variable-name cv) -> string or symbol
func PrimConditionVariableName(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-name: expected condition-variable, got %T", o)
	}
	mc.SetValue(values.NewString(cv.Name()))
	return nil
}

// PrimConditionVariableSpecific returns the condition variable's specific field
// (condition-variable-specific cv) -> value
func PrimConditionVariableSpecific(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-specific: expected condition-variable, got %T", o)
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
func PrimConditionVariableSpecificSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	val := mc.Arg(1)

	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-specific-set!: expected condition-variable, got %T", o)
	}

	cv.SetSpecific(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableSignal signals one waiting thread
// (condition-variable-signal! cv) -> void
func PrimConditionVariableSignal(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-signal!: expected condition-variable, got %T", o)
	}
	cv.Signal()
	mc.SetValue(values.Void)
	return nil
}

// PrimConditionVariableBroadcast signals all waiting threads
// (condition-variable-broadcast! cv) -> void
func PrimConditionVariableBroadcast(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	cv, ok := o.(*values.ConditionVariable)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAConditionVariable, "condition-variable-broadcast!: expected condition-variable, got %T", o)
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
func PrimCurrentTime(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.CurrentTime())
	return nil
}

// PrimTimeQ tests if an object is a time
// (time? obj) -> boolean
func PrimTimeQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	_, ok := o.(*values.Time)
	if ok {
		mc.SetValue(values.TrueValue)
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimTimeToSeconds converts a time to seconds
// (time->seconds time) -> number
func PrimTimeToSeconds(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	t, ok := o.(*values.Time)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotATime, "time->seconds: expected time, got %T", o)
	}
	mc.SetValue(values.NewFloat(t.Seconds()))
	return nil
}

// PrimSecondsToTime converts seconds to a time
// (seconds->time x) -> time
func PrimSecondsToTime(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)

	var seconds float64
	switch v := o.(type) {
	case *values.Integer:
		seconds = float64(v.Value)
	case *values.Float:
		seconds = v.Value
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "seconds->time: expected number, got %T", o)
	}

	mc.SetValue(values.NewTimeFromSeconds(seconds))
	return nil
}
