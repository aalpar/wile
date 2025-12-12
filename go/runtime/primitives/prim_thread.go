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

// SRFI-18 Thread Primitives
// See: https://srfi.schemers.org/srfi-18/srfi-18.html

package primitives

import (
	"context"
	"errors"
	"runtime"
	"time"

	"wile/machine"
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	thunk := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

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

		// Create a new machine context for this thread
		sub := mc.NewSubContext()
		if _, err := sub.Apply(cls); err != nil {
			return nil, err
		}

		// Run the thunk
		err := sub.Run(ctx)
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

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
