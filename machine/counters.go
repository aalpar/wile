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
	OpsExecuted            uint64
	ClosuresApplied        uint64
	EnvsCopied             uint64
	BindingsCopied         uint64
	ContinuationsSaved     uint64
	ContinuationsRestored  uint64
	StackPopAlls           uint64
	StackElementsCopied    uint64
	ForeignCalls           uint64
	SubContextsCreated     uint64
	StackPoolReleases      uint64
	SubContextPoolReleases uint64
	// Pool effectiveness under call/cc:
	//   ratio = SharedFrameRestores / (SharedFrameRestores + ContinuationPoolReleases)
	//   0.0 = no call/cc impact (all frames recycled via pool)
	//   1.0 = all frames shared (no recycling, pure GC pressure)
	//   > 0.5 = pool losing more than it saves; consider profiling GC pauses
	ContinuationPoolReleases uint64
	EnvFramePoolReleases     uint64
	SharedFrameRestores      uint64
	KeysShared               uint64
	NoCopyApplies            uint64
	NoCopyBindingsSaved      uint64

	// Stack depth instrumentation (ongoing monitoring; prior cap-tuning investigation
	// showed cap-8 is sufficient for observed workloads)
	StackMaxDepth   uint64
	StackDepth0to2  uint64 // depth 0-2: fits trivially
	StackDepth3to4  uint64 // depth 3-4: typical calls
	StackDepth5to8  uint64 // depth 5-8: fits in pool cap 8
	StackDepth9to16 uint64 // depth 9-16: requires 1 growth from cap 8
	StackDepth17p   uint64 // depth 17+: requires 2+ growths
}

// RecordStackDepth updates the depth histogram and max tracker.
func (c *VMCounters) RecordStackDepth(n int) {
	if uint64(n) > c.StackMaxDepth {
		c.StackMaxDepth = uint64(n)
	}
	switch {
	case n <= 2:
		c.StackDepth0to2++
	case n <= 4:
		c.StackDepth3to4++
	case n <= 8:
		c.StackDepth5to8++
	case n <= 16:
		c.StackDepth9to16++
	default:
		c.StackDepth17p++
	}
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
			"env_frame_pool_releases:      %d\n"+
			"shared_frame_restores:        %d\n"+
			"keys_shared:                  %d\n"+
			"no_copy_applies:              %d\n"+
			"no_copy_bindings_saved:       %d\n"+
			"stack_max_depth:              %d\n"+
			"stack_depth_0to2:             %d\n"+
			"stack_depth_3to4:             %d\n"+
			"stack_depth_5to8:             %d\n"+
			"stack_depth_9to16:            %d\n"+
			"stack_depth_17p:              %d",
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
		c.EnvFramePoolReleases,
		c.SharedFrameRestores,
		c.KeysShared,
		c.NoCopyApplies,
		c.NoCopyBindingsSaved,
		c.StackMaxDepth,
		c.StackDepth0to2,
		c.StackDepth3to4,
		c.StackDepth5to8,
		c.StackDepth9to16,
		c.StackDepth17p,
	)
}
