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
	"slices"
	"sync/atomic"
)

// DynamicWindFrame represents a single dynamic-wind extent.
// Each frame tracks the before/after thunks for one dynamic-wind call.
//
// R7RS §6.10: dynamic-wind establishes a dynamic extent during which
// the before and after thunks are called whenever control enters or exits.
//
// Continuation-wind interaction (Friedman & Haynes 1985, Clinger et al.
// 1999). The winding stack W is separate from continuation chain K.
//
//	W = [w₁, w₂, ...wₙ] where wᵢ = (before_i, after_i, id_i)
//
//	On continuation invocation from Wsrc to Wtgt:
//	  prefix = FindCommonWindingPrefix(Wsrc, Wtgt)
//	  UnwindTo(prefix):  call after_n, after_{n-1}, ... (innermost first)
//	  RewindTo(Wtgt):    call before_{prefix+1}, ... (outermost first)
//
//	Invariant: W is captured by value at call/cc time (copied into
//	  ComposableContinuation), NOT stored per continuation frame.
//	  W belongs to the dynamic extent, not the lexical continuation.
//	Constrains: RestoreWithWindingFrom (must compute prefix and run
//	  thunks in correct order), sub-contexts (must explicitly inherit W
//	  via SetWindingStack or thunks are silently skipped).
//	Constrained by: CESK model (W is NOT part of K — it is orthogonal
//	  state). PushWind/PopWind opcodes maintain W during normal execution.
//
// See BIBLIOGRAPHY.md "Dynamic-Wind" and "Continuation-Wind Interaction".
type DynamicWindFrame struct {
	Before Closure // Called when entering this extent
	After  Closure // Called when exiting this extent
	ID     uint64  // Unique identifier for extent matching
}

// Global counter for generating unique frame IDs
var nextWindingID atomic.Uint64

// NewDynamicWindFrame creates a new winding frame with a unique ID.
func NewDynamicWindFrame(before, after Closure) *DynamicWindFrame {
	id := nextWindingID.Add(1)
	return &DynamicWindFrame{
		Before: before,
		After:  after,
		ID:     id,
	}
}

// WindingStack tracks the current dynamic-wind context.
// It's a slice of frames from outermost to innermost.
type WindingStack []*DynamicWindFrame

// Copy creates a shallow copy of the winding stack.
func (p WindingStack) Copy() WindingStack {
	return slices.Clone(p)
}

// Depth returns the number of active dynamic-wind frames.
func (p WindingStack) Depth() int {
	return len(p)
}

// Push adds a frame to the winding stack.
func (p *WindingStack) Push(frame *DynamicWindFrame) {
	*p = append(*p, frame)
}

// Pop removes the innermost frame from the winding stack.
func (p *WindingStack) Pop() *DynamicWindFrame {
	if len(*p) == 0 {
		return nil
	}
	n := len(*p) - 1
	frame := (*p)[n]
	*p = (*p)[:n]
	return frame
}

// FindCommonWindingPrefix finds the longest common prefix of two winding stacks.
// Returns the index where they diverge (0 means no common frames).
func FindCommonWindingPrefix(current, target WindingStack) int {
	minLen := min(len(current), len(target))
	for i := range minLen {
		if current[i].ID != target[i].ID {
			return i
		}
	}
	return minLen
}
