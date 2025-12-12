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

// SRFI-18 Mutex Primitives
// See: https://srfi.schemers.org/srfi-18/srfi-18.html

package primitives

import (
	"context"
	"time"

	"wile/machine"
	"wile/values"
)

// PrimMutexQ tests if an object is a mutex
// (mutex? obj) -> boolean
func PrimMutexQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	val := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	mutex, ok := o.(*values.Mutex)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAMutex, "mutex-lock!: expected mutex, got %T", o)
	}

	var timeout *time.Duration
	var owner *values.Thread = currentThread

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
				} else if threadArg != values.FalseValue {
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
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	restVal := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

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
		} else if cvArg != values.FalseValue {
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
