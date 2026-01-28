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
type DynamicWindFrame struct {
	Before *MachineClosure // Called when entering this extent
	After  *MachineClosure // Called when exiting this extent
	ID     uint64          // Unique identifier for extent matching
}

// Global counter for generating unique frame IDs
var nextWindingID uint64

// NewDynamicWindFrame creates a new winding frame with a unique ID.
func NewDynamicWindFrame(before, after *MachineClosure) *DynamicWindFrame {
	id := atomic.AddUint64(&nextWindingID, 1)
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
func (w WindingStack) Copy() WindingStack {
	return slices.Clone(w)
}

// Depth returns the number of active dynamic-wind frames.
func (w WindingStack) Depth() int {
	return len(w)
}

// Push adds a frame to the winding stack.
func (w *WindingStack) Push(frame *DynamicWindFrame) {
	*w = append(*w, frame)
}

// Pop removes the innermost frame from the winding stack.
func (w *WindingStack) Pop() *DynamicWindFrame {
	if len(*w) == 0 {
		return nil
	}
	n := len(*w) - 1
	frame := (*w)[n]
	*w = (*w)[:n]
	return frame
}

// FindCommonPrefix finds the longest common prefix of two winding stacks.
// Returns the index where they diverge (0 means no common frames).
func FindCommonWindingPrefix(current, target WindingStack) int {
	minLen := min(len(current), len(target))
	for i := 0; i < minLen; i++ {
		if current[i].ID != target[i].ID {
			return i
		}
	}
	return minLen
}
