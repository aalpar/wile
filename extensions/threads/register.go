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

package threads

import (
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the threads extension.
var Extension = registry.NewDescribedExtension("threads",
	"Concurrency: SRFI-18 threads, mutexes, condition variables, time objects.",
	AddToRegistry)

// Builder aggregates all threading registration functions.
var Builder = registry.NewRegistryBuilder(addThreads, addMutexes, addConditionVariables, addTime)

// AddToRegistry registers all threading primitives.
var AddToRegistry = Builder.AddToRegistry

func addThreads(r *registry.PrimitiveRegistry) error {
	// One tracker per engine: addThreads runs once per engine (pkg/wile
	// buildRegistry's extension loop), and thread-start! closes over it, so
	// Engine.Close reaps only the threads that engine started. A
	// registry.Closeable hook on the Extension var above could not — that var is
	// package-level and shared by every engine in the process.
	live := newLiveThreads()
	r.AddCloser(live.Close)
	r.AddPrimitives([]registry.PrimitiveSpec{
		// TODO(contracts): *values.Thread, *values.Mutex, *values.ConditionVariable,
		// and *values.Time have no ValueType enum entries. Those argument
		// positions use TypeAny; the impl's helpers.RequireArg still catches
		// type mismatches.
		{Name: "current-thread", Impl: PrimCurrentThread,
			Doc: "Returns the current thread object, or the symbol 'primordial for the main thread.", Category: "threads",
			ReturnType: values.TypeAny},
		{Name: "thread?", ParamCount: 1, Impl: PrimThreadQ,
			Doc: "Returns #t if OBJ is a thread object.", ParamNames: []string{"obj"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "make-thread", InvokesProcedure: true, ParamCount: 2, IsVariadic: true, Impl: PrimMakeThread,
			Doc: "Creates a new thread that will execute THUNK when started. Optional NAME for debugging.", ParamNames: []string{"thunk", "name"}, Category: "threads",
			Keywords:   []string{"spawn", "goroutine", "create thread", "concurrent"},
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeAny}, ReturnType: values.TypeAny},
		// {thread,mutex,condition-variable}-name unconditionally wrap the stored
		// Go-side name (possibly "") via values.NewString — always a string.
		// specific accessors return the stored Value → TypeAny.
		{Name: "thread-name", ParamCount: 1, Impl: PrimThreadName,
			Doc: "Returns the name of THREAD as a string. Unnamed threads have the empty string.", ParamNames: []string{"thread"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeString},
		{Name: "thread-specific", ParamCount: 1, Impl: PrimThreadSpecific,
			Doc: "Returns the thread-local specific value associated with THREAD.", ParamNames: []string{"thread"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "thread-specific-set!", ParamCount: 2, Impl: PrimThreadSpecificSet,
			Doc: "Sets the thread-local specific value for THREAD.", ParamNames: []string{"thread", "value"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "thread-start!", InvokesProcedure: true, ParamCount: 1, Impl: live.threadStartImpl(),
			Doc: "Starts execution of THREAD in a new goroutine. Returns THREAD.", ParamNames: []string{"thread"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "thread-yield!", Impl: PrimThreadYield,
			Doc: "Voluntarily yields the current thread to the Go scheduler.", Category: "threads",
			ReturnType: values.TypeVoid},
		{Name: "thread-sleep!", ParamCount: 1, Impl: PrimThreadSleep,
			Doc: "Suspends the current thread. A number is a duration in seconds; a time object is an absolute wake-up instant (already-past times return immediately).", ParamNames: []string{"timeout"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "thread-terminate!", ParamCount: 1, Impl: PrimThreadTerminate,
			Doc: "Terminates THREAD. Mutexes it still owns are marked abandoned, so a later mutex-lock! on them acquires the mutex and signals an abandoned-mutex exception.", ParamNames: []string{"thread"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		// InvokesProcedure: the SRFI-18 conditions (join-timeout, terminated-thread,
		// uncaught-exception) reach Scheme through machine.RaiseInPlace, which runs the
		// installed handler INLINE on the live chain. Costs a deopt on every call; a
		// blocking join is not a site where frame reclaim pays.
		{Name: "thread-join!", InvokesProcedure: true, ParamCount: 2, IsVariadic: true, Impl: PrimThreadJoin,
			Doc: "Waits for THREAD to complete and returns its result. Optional TIMEOUT and default value. Raises a join-timeout-exception if TIMEOUT is reached and no default was supplied, a terminated-thread-exception if THREAD died via thread-terminate!, or an uncaught-exception (whose uncaught-exception-reason is the original condition) if it died via an uncaught exception. The wait is cancellable: if the JOINING thread is itself terminated, or the engine context is cancelled, the join raises an ordinary error object rather than parking forever; inside a with-timeout it runs the handler.", ParamNames: []string{"thread", "timeout"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "thread-state", ParamCount: 1, Impl: PrimThreadState,
			Doc: "Returns THREAD's state as a symbol: new, runnable, blocked, or terminated. Not SRFI-18; follows Gambit.", ParamNames: []string{"thread"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeSymbol},
		{Name: "join-timeout-exception?", ParamCount: 1, Impl: PrimJoinTimeoutExceptionQ,
			Doc: "Returns #t if OBJ is a join-timeout-exception object (raised by thread-join! when its timeout is reached and no default value was supplied).", ParamNames: []string{"obj"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "terminated-thread-exception?", ParamCount: 1, Impl: PrimTerminatedThreadExceptionQ,
			Doc: "Returns #t if OBJ is a terminated-thread-exception object (raised by thread-join! when the joined thread died via thread-terminate!).", ParamNames: []string{"obj"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "abandoned-mutex-exception?", ParamCount: 1, Impl: PrimAbandonedMutexExceptionQ,
			Doc: "Returns #t if OBJ is an abandoned-mutex-exception object (raised by mutex-lock! when it acquires a mutex whose owner terminated while holding it).", ParamNames: []string{"obj"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "uncaught-exception?", ParamCount: 1, Impl: PrimUncaughtExceptionQ,
			Doc: "Returns #t if OBJ is an uncaught-exception object (raised by thread-join! when a joined thread died via an uncaught exception).", ParamNames: []string{"obj"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "uncaught-exception-reason", ParamCount: 1, Impl: PrimUncaughtExceptionReason,
			Doc: "Returns the original condition object that the joined thread raised.", ParamNames: []string{"uncaught-exception"}, Category: "threads",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
	}, registry.PhaseSetRuntime)
	return nil
}

func addMutexes(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "mutex?", ParamCount: 1, Impl: PrimMutexQ,
			Doc: "Returns #t if OBJ is a SRFI-18 mutex.", ParamNames: []string{"obj"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "make-mutex", ParamCount: 1, IsVariadic: true, Impl: PrimMakeMutex,
			Doc: "Creates a new mutex. Optional NAME (string or symbol) for debugging.", ParamNames: []string{"name"}, Category: "mutexes",
			Keywords:   []string{"lock", "synchronization", "critical section"},
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "mutex-name", ParamCount: 1, Impl: PrimMutexName,
			Doc: "Returns the name of MUTEX as a string. Unnamed mutexes have the empty string.", ParamNames: []string{"mutex"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeString},
		{Name: "mutex-specific", ParamCount: 1, Impl: PrimMutexSpecific,
			Doc: "Returns the mutex-local specific value.", ParamNames: []string{"mutex"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "mutex-specific-set!", ParamCount: 2, Impl: PrimMutexSpecificSet,
			Doc: "Sets the mutex-local specific value.", ParamNames: []string{"mutex", "value"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "mutex-state", ParamCount: 1, Impl: PrimMutexState,
			Doc: "Returns the state of MUTEX (SRFI-18): the owning thread; or the symbol not-owned if it is held with no owning thread; abandoned if its owner terminated while holding it; not-abandoned if it is not held.", ParamNames: []string{"mutex"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		// InvokesProcedure: acquiring an abandoned mutex signals through
		// machine.RaiseInPlace, same live-chain handler run as thread-join! above.
		{Name: "mutex-lock!", InvokesProcedure: true, ParamCount: 2, IsVariadic: true, Impl: PrimMutexLock,
			Doc: "Locks MUTEX, blocking until acquired. Optional TIMEOUT and owner (thread or #f for unowned). Acquiring a mutex abandoned by a terminated owner returns #t and additionally signals an abandoned-mutex exception.", ParamNames: []string{"mutex", "timeout"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "mutex-unlock!", ParamCount: 2, IsVariadic: true, Impl: PrimMutexUnlock,
			Doc: "Unlocks MUTEX. Optional condition variable to wait on after unlocking, with optional timeout.", ParamNames: []string{"mutex", "condvar"}, Category: "mutexes",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeBoolean},
	}, registry.PhaseSetRuntime)
	return nil
}

func addConditionVariables(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "condition-variable?", ParamCount: 1, Impl: PrimConditionVariableQ,
			Doc: "Returns #t if OBJ is a condition variable.", ParamNames: []string{"obj"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "make-condition-variable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeConditionVariable,
			Doc: "Creates a new condition variable. Optional NAME for debugging.", ParamNames: []string{"name"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "condition-variable-name", ParamCount: 1, Impl: PrimConditionVariableName,
			Doc: "Returns the name of CONDVAR as a string. Unnamed condition variables have the empty string.", ParamNames: []string{"condvar"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeString},
		{Name: "condition-variable-specific", ParamCount: 1, Impl: PrimConditionVariableSpecific,
			Doc: "Returns the condition-variable-local specific value.", ParamNames: []string{"condvar"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "condition-variable-specific-set!", ParamCount: 2, Impl: PrimConditionVariableSpecificSet,
			Doc: "Sets the condition-variable-local specific value.", ParamNames: []string{"condvar", "value"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "condition-variable-signal!", ParamCount: 1, Impl: PrimConditionVariableSignal,
			Doc: "Wakes one thread waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
		{Name: "condition-variable-broadcast!", ParamCount: 1, Impl: PrimConditionVariableBroadcast,
			Doc: "Wakes all threads waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeVoid},
	}, registry.PhaseSetRuntime)
	return nil
}

func addTime(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "current-time", Impl: PrimCurrentTime,
			Doc: "Returns the current time as a time object.", Category: "time",
			ReturnType: values.TypeAny},
		{Name: "time?", ParamCount: 1, Impl: PrimTimeQ,
			Doc: "Returns #t if OBJ is a time object.", ParamNames: []string{"obj"}, Category: "time",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeBoolean},
		{Name: "time->seconds", ParamCount: 1, Impl: PrimTimeToSeconds,
			Doc: "Converts TIME to seconds since epoch as an inexact real number.", ParamNames: []string{"time"}, Category: "time",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeFlonum},
		{Name: "seconds->time", ParamCount: 1, Impl: PrimSecondsToTime,
			Doc: "Converts SECONDS (integer or float) to a time object.", ParamNames: []string{"seconds"}, Category: "time",
			ParamTypes: []values.TypeConstraint{values.TypeNumber}, ReturnType: values.TypeAny},
	}, registry.PhaseSetRuntime)
	return nil
}
