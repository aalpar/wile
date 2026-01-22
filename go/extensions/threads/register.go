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
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package threads

import (
	"wile/registry"
	"wile/runtime/primitives"
)

// Extension is the threads extension.
var Extension = registry.NewExtension("threads", AddToRegistry)

// Builder aggregates all threading registration functions.
var Builder = registry.NewRegistryBuilder(addThreads, addMutexes, addConditionVariables, addTime)

// AddToRegistry registers all threading primitives.
var AddToRegistry = Builder.AddToRegistry

func addThreads(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"current-thread", 0, false, primitives.PrimCurrentThread},
		{"thread?", 1, false, primitives.PrimThreadQ},
		{"make-thread", 2, true, primitives.PrimMakeThread},
		{"thread-name", 1, false, primitives.PrimThreadName},
		{"thread-specific", 1, false, primitives.PrimThreadSpecific},
		{"thread-specific-set!", 2, false, primitives.PrimThreadSpecificSet},
		{"thread-start!", 1, false, primitives.PrimThreadStart},
		{"thread-yield!", 0, false, primitives.PrimThreadYield},
		{"thread-sleep!", 1, false, primitives.PrimThreadSleep},
		{"thread-terminate!", 1, false, primitives.PrimThreadTerminate},
		{"thread-join!", 2, true, primitives.PrimThreadJoin},
	}, registry.PhaseRuntime)
	return nil
}

func addMutexes(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"mutex?", 1, false, primitives.PrimMutexQ},
		{"make-mutex", 1, true, primitives.PrimMakeMutex},
		{"mutex-name", 1, false, primitives.PrimMutexName},
		{"mutex-specific", 1, false, primitives.PrimMutexSpecific},
		{"mutex-specific-set!", 2, false, primitives.PrimMutexSpecificSet},
		{"mutex-state", 1, false, primitives.PrimMutexState},
		{"mutex-lock!", 2, true, primitives.PrimMutexLock},
		{"mutex-unlock!", 2, true, primitives.PrimMutexUnlock},
	}, registry.PhaseRuntime)
	return nil
}

func addConditionVariables(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"condition-variable?", 1, false, primitives.PrimConditionVariableQ},
		{"make-condition-variable", 1, true, primitives.PrimMakeConditionVariable},
		{"condition-variable-name", 1, false, primitives.PrimConditionVariableName},
		{"condition-variable-specific", 1, false, primitives.PrimConditionVariableSpecific},
		{"condition-variable-specific-set!", 2, false, primitives.PrimConditionVariableSpecificSet},
		{"condition-variable-signal!", 1, false, primitives.PrimConditionVariableSignal},
		{"condition-variable-broadcast!", 1, false, primitives.PrimConditionVariableBroadcast},
	}, registry.PhaseRuntime)
	return nil
}

func addTime(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"current-time", 0, false, primitives.PrimCurrentTime},
		{"time?", 1, false, primitives.PrimTimeQ},
		{"time->seconds", 1, false, primitives.PrimTimeToSeconds},
		{"seconds->time", 1, false, primitives.PrimSecondsToTime},
	}, registry.PhaseRuntime)
	return nil
}
