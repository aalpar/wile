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

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
)

// VMCounters holds performance counters for a single MachineContext execution.
// All counters are plain uint64 — no atomics needed because each MachineContext
// is single-goroutine. Sub-contexts have their own counters (not aggregated
// into the parent).
type VMCounters struct {
	OpsExecuted            uint64
	opcodeHits             *[opCount]uint64
	ClosuresApplied        uint64
	EnvsCopied             uint64
	BindingsCopied         uint64
	ContinuationsSaved     uint64
	ContinuationsRestored  uint64
	StackDrains            uint64
	StackElementsDrained   uint64
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
	InlineEvalsSaved         uint64 // SaveContinuation used inline slots instead of stack pool

	// Stack depth instrumentation (ongoing monitoring; prior cap-tuning investigation
	// showed cap-8 is sufficient for observed workloads)
	StackMaxDepth   uint64
	StackDepth0to2  uint64 // depth 0-2: fits trivially
	StackDepth3to4  uint64 // depth 3-4: typical calls
	StackDepth5to8  uint64 // depth 5-8: fits in pool cap 8
	StackDepth9to16 uint64 // depth 9-16: requires 1 growth from cap 8
	StackDepth17p   uint64 // depth 17+: requires 2+ growths
}

var opcodeHitsEnabled = sync.OnceValue(func() bool {
	return os.Getenv("WILE_OPCODE_HITS") != ""
})

// newOpcodeHits returns a hits array if WILE_OPCODE_HITS is set, nil otherwise.
func newOpcodeHits() *[opCount]uint64 {
	if opcodeHitsEnabled() {
		return new([opCount]uint64)
	}
	return nil
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
			"stack_drains:                 %d\n"+
			"stack_elements_drained:       %d\n"+
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
			"inline_evals_saved:           %d\n"+
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
		c.StackDrains,
		c.StackElementsDrained,
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
		c.InlineEvalsSaved,
		c.StackMaxDepth,
		c.StackDepth0to2,
		c.StackDepth3to4,
		c.StackDepth5to8,
		c.StackDepth9to16,
		c.StackDepth17p,
	)
}

// OpcodeHistogram returns a formatted histogram of opcode hit counts,
// sorted by frequency (descending). Only opcodes with non-zero hits
// are included.
func (c VMCounters) OpcodeHistogram() string {
	if c.opcodeHits == nil {
		return ""
	}
	type entry struct {
		name  string
		count uint64
	}
	var entries []entry
	for i := range opCount {
		if c.opcodeHits[i] > 0 {
			entries = append(entries, entry{
				name:  i.String(),
				count: c.opcodeHits[i],
			})
		}
	}
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].count > entries[j].count
	})

	var b strings.Builder
	for _, e := range entries {
		pct := float64(e.count) / float64(c.OpsExecuted) * 100
		fmt.Fprintf(&b, "  %-24s %10d  (%5.1f%%)\n", e.name, e.count, pct)
	}
	return b.String()
}
