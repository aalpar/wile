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

// Package threads provides SRFI-18 threading primitives.
package threads

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the threads extension.
var Extension = registry.NewExtension("threads", AddToRegistry)

// Builder aggregates all threading registration functions.
var Builder = registry.NewRegistryBuilder(addThreads, addMutexes, addConditionVariables, addTime)

// AddToRegistry registers all threading primitives.
var AddToRegistry = Builder.AddToRegistry

func addThreads(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "current-thread", Impl: PrimCurrentThread,
			Doc: "Returns the current thread object, or the symbol 'primordial for the main thread.", Category: "threads"},
		{Name: "thread?", ParamCount: 1, Impl: PrimThreadQ,
			Doc: "Returns #t if obj is a thread object.", ParamNames: []string{"obj"}, Category: "threads"},
		{Name: "make-thread", ParamCount: 2, IsVariadic: true, Impl: PrimMakeThread,
			Doc: "Creates a new thread that will execute thunk when started. Optional name for debugging.", ParamNames: []string{"thunk", "name"}, Category: "threads"},
		{Name: "thread-name", ParamCount: 1, Impl: PrimThreadName,
			Doc: "Returns the name of a thread as a string, or #f if unnamed.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-specific", ParamCount: 1, Impl: PrimThreadSpecific,
			Doc: "Returns the thread-local specific value associated with thread.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-specific-set!", ParamCount: 2, Impl: PrimThreadSpecificSet,
			Doc: "Sets the thread-local specific value for thread.", ParamNames: []string{"thread", "value"}, Category: "threads"},
		{Name: "thread-start!", ParamCount: 1, Impl: PrimThreadStart,
			Doc: "Starts execution of thread in a new goroutine. Returns the thread object.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-yield!", Impl: PrimThreadYield,
			Doc: "Voluntarily yields the current thread to the Go scheduler.", Category: "threads"},
		{Name: "thread-sleep!", ParamCount: 1, Impl: PrimThreadSleep,
			Doc: "Suspends the current thread for the specified duration. Accepts time objects, integers, or floats (seconds).", ParamNames: []string{"timeout"}, Category: "threads"},
		{Name: "thread-terminate!", ParamCount: 1, Impl: PrimThreadTerminate,
			Doc: "Terminates thread. Abandoned mutexes are automatically released.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-join!", ParamCount: 2, IsVariadic: true, Impl: PrimThreadJoin,
			Doc: "Waits for thread to complete and returns its result. Optional timeout and timeout-val.", ParamNames: []string{"thread", "timeout"}, Category: "threads"},
	}, registry.PhaseRuntime)
	return nil
}

func addMutexes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "mutex?", ParamCount: 1, Impl: PrimMutexQ,
			Doc: "Returns #t if obj is a SRFI-18 mutex.", ParamNames: []string{"obj"}, Category: "mutexes"},
		{Name: "make-mutex", ParamCount: 1, IsVariadic: true, Impl: PrimMakeMutex,
			Doc: "Creates a new mutex. Optional name (string or symbol) for debugging.", ParamNames: []string{"name"}, Category: "mutexes"},
		{Name: "mutex-name", ParamCount: 1, Impl: PrimMutexName,
			Doc: "Returns the name of a mutex, or #f if unnamed.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-specific", ParamCount: 1, Impl: PrimMutexSpecific,
			Doc: "Returns the mutex-local specific value.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-specific-set!", ParamCount: 2, Impl: PrimMutexSpecificSet,
			Doc: "Sets the mutex-local specific value.", ParamNames: []string{"mutex", "value"}, Category: "mutexes"},
		{Name: "mutex-state", ParamCount: 1, Impl: PrimMutexState,
			Doc: "Returns the state of mutex: the symbol not-owned, not-abandoned, abandoned, or the owning thread.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-lock!", ParamCount: 2, IsVariadic: true, Impl: PrimMutexLock,
			Doc: "Locks mutex, blocking until acquired. Optional timeout and owner (thread or #f for unowned).", ParamNames: []string{"mutex", "timeout"}, Category: "mutexes"},
		{Name: "mutex-unlock!", ParamCount: 2, IsVariadic: true, Impl: PrimMutexUnlock,
			Doc: "Unlocks mutex. Optional condition variable to wait on after unlocking, with optional timeout.", ParamNames: []string{"mutex", "condvar"}, Category: "mutexes"},
	}, registry.PhaseRuntime)
	return nil
}

func addConditionVariables(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "condition-variable?", ParamCount: 1, Impl: PrimConditionVariableQ,
			Doc: "Returns #t if obj is a condition variable.", ParamNames: []string{"obj"}, Category: "condvars"},
		{Name: "make-condition-variable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeConditionVariable,
			Doc: "Creates a new condition variable. Optional name for debugging.", ParamNames: []string{"name"}, Category: "condvars"},
		{Name: "condition-variable-name", ParamCount: 1, Impl: PrimConditionVariableName,
			Doc: "Returns the name of a condition variable, or #f if unnamed.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-specific", ParamCount: 1, Impl: PrimConditionVariableSpecific,
			Doc: "Returns the condition-variable-local specific value.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-specific-set!", ParamCount: 2, Impl: PrimConditionVariableSpecificSet,
			Doc: "Sets the condition-variable-local specific value.", ParamNames: []string{"condvar", "value"}, Category: "condvars"},
		{Name: "condition-variable-signal!", ParamCount: 1, Impl: PrimConditionVariableSignal,
			Doc: "Wakes one thread waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-broadcast!", ParamCount: 1, Impl: PrimConditionVariableBroadcast,
			Doc: "Wakes all threads waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars"},
	}, registry.PhaseRuntime)
	return nil
}

func addTime(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "current-time", Impl: PrimCurrentTime,
			Doc: "Returns the current time as a time object.", Category: "time"},
		{Name: "time?", ParamCount: 1, Impl: PrimTimeQ,
			Doc: "Returns #t if obj is a time object.", ParamNames: []string{"obj"}, Category: "time"},
		{Name: "time->seconds", ParamCount: 1, Impl: PrimTimeToSeconds,
			Doc: "Converts a time object to seconds since epoch as an inexact real number.", ParamNames: []string{"time"}, Category: "time"},
		{Name: "seconds->time", ParamCount: 1, Impl: PrimSecondsToTime,
			Doc: "Converts seconds (integer or float) to a time object.", ParamNames: []string{"seconds"}, Category: "time"},
	}, registry.PhaseRuntime)
	return nil
}
