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
			Doc: "Returns the current thread.", Category: "threads"},
		{Name: "thread?", ParamCount: 1, Impl: PrimThreadQ,
			Doc: "Returns #t if obj is a thread.", ParamNames: []string{"obj"}, Category: "threads"},
		{Name: "make-thread", ParamCount: 2, IsVariadic: true, Impl: PrimMakeThread,
			Doc: "Creates a new thread from thunk with optional name.", ParamNames: []string{"thunk", "name"}, Category: "threads"},
		{Name: "thread-name", ParamCount: 1, Impl: PrimThreadName,
			Doc: "Returns the name of a thread.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-specific", ParamCount: 1, Impl: PrimThreadSpecific,
			Doc: "Returns the thread-specific value.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-specific-set!", ParamCount: 2, Impl: PrimThreadSpecificSet,
			Doc: "Sets the thread-specific value.", ParamNames: []string{"thread", "value"}, Category: "threads"},
		{Name: "thread-start!", ParamCount: 1, Impl: PrimThreadStart,
			Doc: "Starts a thread and returns it.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-yield!", Impl: PrimThreadYield,
			Doc: "Yields the current thread to the scheduler.", Category: "threads"},
		{Name: "thread-sleep!", ParamCount: 1, Impl: PrimThreadSleep,
			Doc: "Sleeps for the given duration.", ParamNames: []string{"timeout"}, Category: "threads"},
		{Name: "thread-terminate!", ParamCount: 1, Impl: PrimThreadTerminate,
			Doc: "Terminates a thread.", ParamNames: []string{"thread"}, Category: "threads"},
		{Name: "thread-join!", ParamCount: 2, IsVariadic: true, Impl: PrimThreadJoin,
			Doc: "Waits for thread completion with optional timeout.", ParamNames: []string{"thread", "timeout"}, Category: "threads"},
	}, registry.PhaseRuntime)
	return nil
}

func addMutexes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "mutex?", ParamCount: 1, Impl: PrimMutexQ,
			Doc: "Returns #t if obj is a mutex.", ParamNames: []string{"obj"}, Category: "mutexes"},
		{Name: "make-mutex", ParamCount: 1, IsVariadic: true, Impl: PrimMakeMutex,
			Doc: "Creates a new mutex with optional name.", ParamNames: []string{"name"}, Category: "mutexes"},
		{Name: "mutex-name", ParamCount: 1, Impl: PrimMutexName,
			Doc: "Returns the name of a mutex.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-specific", ParamCount: 1, Impl: PrimMutexSpecific,
			Doc: "Returns the mutex-specific value.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-specific-set!", ParamCount: 2, Impl: PrimMutexSpecificSet,
			Doc: "Sets the mutex-specific value.", ParamNames: []string{"mutex", "value"}, Category: "mutexes"},
		{Name: "mutex-state", ParamCount: 1, Impl: PrimMutexState,
			Doc: "Returns the state of a mutex.", ParamNames: []string{"mutex"}, Category: "mutexes"},
		{Name: "mutex-lock!", ParamCount: 2, IsVariadic: true, Impl: PrimMutexLock,
			Doc: "Locks a mutex with optional timeout and owner.", ParamNames: []string{"mutex", "timeout"}, Category: "mutexes"},
		{Name: "mutex-unlock!", ParamCount: 2, IsVariadic: true, Impl: PrimMutexUnlock,
			Doc: "Unlocks a mutex with optional condition variable and timeout.", ParamNames: []string{"mutex", "condvar"}, Category: "mutexes"},
	}, registry.PhaseRuntime)
	return nil
}

func addConditionVariables(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "condition-variable?", ParamCount: 1, Impl: PrimConditionVariableQ,
			Doc: "Returns #t if obj is a condition variable.", ParamNames: []string{"obj"}, Category: "condvars"},
		{Name: "make-condition-variable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeConditionVariable,
			Doc: "Creates a new condition variable with optional name.", ParamNames: []string{"name"}, Category: "condvars"},
		{Name: "condition-variable-name", ParamCount: 1, Impl: PrimConditionVariableName,
			Doc: "Returns the name of a condition variable.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-specific", ParamCount: 1, Impl: PrimConditionVariableSpecific,
			Doc: "Returns the condition-variable-specific value.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-specific-set!", ParamCount: 2, Impl: PrimConditionVariableSpecificSet,
			Doc: "Sets the condition-variable-specific value.", ParamNames: []string{"condvar", "value"}, Category: "condvars"},
		{Name: "condition-variable-signal!", ParamCount: 1, Impl: PrimConditionVariableSignal,
			Doc: "Signals one thread waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars"},
		{Name: "condition-variable-broadcast!", ParamCount: 1, Impl: PrimConditionVariableBroadcast,
			Doc: "Signals all threads waiting on the condition variable.", ParamNames: []string{"condvar"}, Category: "condvars"},
	}, registry.PhaseRuntime)
	return nil
}

func addTime(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "current-time", Impl: PrimCurrentTime,
			Doc: "Returns the current time.", Category: "time"},
		{Name: "time?", ParamCount: 1, Impl: PrimTimeQ,
			Doc: "Returns #t if obj is a time object.", ParamNames: []string{"obj"}, Category: "time"},
		{Name: "time->seconds", ParamCount: 1, Impl: PrimTimeToSeconds,
			Doc: "Converts a time object to seconds.", ParamNames: []string{"time"}, Category: "time"},
		{Name: "seconds->time", ParamCount: 1, Impl: PrimSecondsToTime,
			Doc: "Converts seconds to a time object.", ParamNames: []string{"seconds"}, Category: "time"},
	}, registry.PhaseRuntime)
	return nil
}
