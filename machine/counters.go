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

package machine

import "fmt"

// VMCounters holds performance counters for a single MachineContext execution.
// All counters are plain uint64 — no atomics needed because each MachineContext
// is single-goroutine. Sub-contexts have their own counters (not aggregated
// into the parent).
type VMCounters struct {
	OpsExecuted              uint64
	ClosuresApplied          uint64
	EnvsCopied               uint64
	BindingsCopied           uint64
	ContinuationsSaved       uint64
	ContinuationsRestored    uint64
	StackPopAlls             uint64
	StackElementsCopied      uint64
	ForeignCalls             uint64
	SubContextsCreated       uint64
	StackPoolReleases        uint64
	SubContextPoolReleases   uint64
	ContinuationPoolReleases uint64
	KeysShared               uint64
}

// String returns a tabular summary of all counters.
func (c VMCounters) String() string {
	return fmt.Sprintf(
		"ops_executed:                 %d\n"+
			"closures_applied:             %d\n"+
			"envs_copied:                  %d\n"+
			"bindings_copied:              %d\n"+
			"continuations_saved:          %d\n"+
			"continuations_restored:       %d\n"+
			"stack_pop_alls:               %d\n"+
			"stack_elements_copied:        %d\n"+
			"foreign_calls:                %d\n"+
			"sub_contexts_created:         %d\n"+
			"stack_pool_releases:          %d\n"+
			"sub_context_pool_releases:    %d\n"+
			"continuation_pool_releases:   %d\n"+
			"keys_shared:                  %d",
		c.OpsExecuted,
		c.ClosuresApplied,
		c.EnvsCopied,
		c.BindingsCopied,
		c.ContinuationsSaved,
		c.ContinuationsRestored,
		c.StackPopAlls,
		c.StackElementsCopied,
		c.ForeignCalls,
		c.SubContextsCreated,
		c.StackPoolReleases,
		c.SubContextPoolReleases,
		c.ContinuationPoolReleases,
		c.KeysShared,
	)
}
