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
	"sync/atomic"
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
	InlineEvalsSaved         uint64 // SaveContinuation used inline slots instead of stack pool

	// Per-callee call counting. Tracks both ForeignClosure (Go primitives) and
	// named MachineClosure (Scheme-defined procedures) calls. Gated by
	// WILE_OPCODE_HITS (same as opcodeHits). nil when profiling is disabled.
	callCounts map[string]uint64

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

// callCountingForced enables per-callee call counting independently of the
// process-global WILE_OPCODE_HITS gate. WILE_OPCODE_HITS also turns on the
// per-opcode hot-path histogram (a write on every dispatched instruction); a
// profiler that only wants the call distribution should not pay that. This flag
// lets such a caller enable just the cheap per-callee map via SetCallCounting.
var callCountingForced atomic.Bool

// SetCallCounting enables or disables per-callee call counting for MachineContexts
// created AFTER this call, independently of WILE_OPCODE_HITS. It is intended for
// measurement harnesses and embedders profiling call distribution; the counts
// are exposed via VMCounters.CallCounts. Existing contexts are unaffected.
func SetCallCounting(on bool) {
	callCountingForced.Store(on)
}

// newCallCounts returns a map for per-callee call counting when WILE_OPCODE_HITS
// is set OR call counting was force-enabled via SetCallCounting; nil otherwise.
func newCallCounts() map[string]uint64 {
	if opcodeHitsEnabled() || callCountingForced.Load() {
		return make(map[string]uint64)
	}
	return nil
}

// RecordCall increments the call count for the named callee (foreign primitive
// or Scheme-defined procedure). No-op when profiling is disabled (nil map).
func (p *VMCounters) RecordCall(name string) {
	if p.callCounts != nil {
		p.callCounts[name]++
	}
}

// RecordStackDepth updates the depth histogram and max tracker.
func (p *VMCounters) RecordStackDepth(n int) {
	if uint64(n) > p.StackMaxDepth {
		p.StackMaxDepth = uint64(n)
	}
	switch {
	case n <= 2:
		p.StackDepth0to2++
	case n <= 4:
		p.StackDepth3to4++
	case n <= 8:
		p.StackDepth5to8++
	case n <= 16:
		p.StackDepth9to16++
	default:
		p.StackDepth17p++
	}
}

// String returns a tabular summary of all counters.
func (p VMCounters) String() string {
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
			"inline_evals_saved:           %d\n"+
			"stack_max_depth:              %d\n"+
			"stack_depth_0to2:             %d\n"+
			"stack_depth_3to4:             %d\n"+
			"stack_depth_5to8:             %d\n"+
			"stack_depth_9to16:            %d\n"+
			"stack_depth_17p:              %d",
		p.OpsExecuted,
		p.ClosuresApplied,
		p.EnvsCopied,
		p.BindingsCopied,
		p.ContinuationsSaved,
		p.ContinuationsRestored,
		p.StackDrains,
		p.StackElementsDrained,
		p.ForeignCalls,
		p.SubContextsCreated,
		p.StackPoolReleases,
		p.SubContextPoolReleases,
		p.ContinuationPoolReleases,
		p.EnvFramePoolReleases,
		p.SharedFrameRestores,
		p.KeysShared,
		p.InlineEvalsSaved,
		p.StackMaxDepth,
		p.StackDepth0to2,
		p.StackDepth3to4,
		p.StackDepth5to8,
		p.StackDepth9to16,
		p.StackDepth17p,
	)
}

// OpcodeHistogram returns a formatted histogram of opcode hit counts,
// sorted by frequency (descending). Only opcodes with non-zero hits
// are included.
func (p VMCounters) OpcodeHistogram() string {
	if p.opcodeHits == nil {
		return ""
	}
	type entry struct {
		name  string
		count uint64
	}
	var entries []entry
	for i := range opCount {
		if p.opcodeHits[i] > 0 {
			entries = append(entries, entry{
				name:  i.String(),
				count: p.opcodeHits[i],
			})
		}
	}
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].count > entries[j].count
	})

	var b strings.Builder
	for _, e := range entries {
		pct := float64(e.count) / float64(p.OpsExecuted) * 100
		fmt.Fprintf(&b, "  %-24s %10d  (%5.1f%%)\n", e.name, e.count, pct)
	}
	return b.String()
}

// CallCounts returns the per-callee call-count map for this context, or nil when
// call counting was disabled. Keys are foreign primitive names and named Scheme
// procedures (NativeTemplate.Name); values are invocation counts. The map is the
// live counter map, not a copy — read it after the run completes.
func (p VMCounters) CallCounts() map[string]uint64 {
	return p.callCounts
}

// CallHistogram returns a formatted histogram of per-callee call counts
// (both foreign primitives and named Scheme procedures), sorted by frequency
// (descending). Returns empty string when profiling is disabled.
func (p VMCounters) CallHistogram() string {
	if p.callCounts == nil {
		return ""
	}
	type entry struct {
		name  string
		count uint64
	}
	entries := make([]entry, 0, len(p.callCounts))
	var total uint64
	for name, count := range p.callCounts {
		entries = append(entries, entry{name: name, count: count})
		total += count
	}
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].count > entries[j].count
	})

	var b strings.Builder
	for _, e := range entries {
		pct := float64(e.count) / float64(total) * 100
		fmt.Fprintf(&b, "  %-24s %10d  (%5.1f%%)\n", e.name, e.count, pct)
	}
	return b.String()
}
