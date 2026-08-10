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

	"github.com/aalpar/wile/pkg/values"
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
//	  thunks in correct order). Sub-contexts inherit W from their parent
//	  via NewSubContext; NewSubContextWithWinding overrides for truncated
//	  stacks during unwind/exception cleanup.
//	Constrained by: CESK model (W is NOT part of K — it is orthogonal
//	  state). PushWind/PopWind opcodes maintain W during normal execution.
//
// See BIBLIOGRAPHY.md "Dynamic-Wind" and "Continuation-Wind Interaction".
type DynamicWindFrame struct {
	// Before/After are values.Callable, not Closure: both are only ever applied,
	// via ApplyCallable, which dispatches six types where Closure has two. A
	// case-lambda, a parameter object or a continuation is a procedure by
	// procedure?'s own answer and must be accepted here.
	//
	// Assign only untyped nil. A typed nil stored in an interface field is not
	// nil, and the winding reconcile tests `frame.Before != nil`.
	Before values.Callable // Called when entering this extent
	After  values.Callable // Called when exiting this extent
	ID     uint64          // Unique identifier for extent matching
	// entryMarks is the snapshot of reachable continuation marks at the dynamic-wind
	// call site, taken when the frame is pushed. R7RS §6.10 runs the before/after
	// thunks in the dynamic environment of the dynamic-wind call, NOT the body's — so a
	// parameterize (marks-based) established inside the body must be invisible to the
	// after thunk. The winding reconcile runs after thunks BEFORE the captured chain is
	// restored, so without this the after thunk would resolve parameters against the
	// still-live body marks (the parameterize value), not the entry environment. nil
	// when no marks were reachable at entry (the common case — no extra cost).
	entryMarks []markEntry
}

// nextWindingID is the global counter for generating unique frame IDs.
var nextWindingID atomic.Uint64

// NewDynamicWindFrame creates a new winding frame with a unique ID.
func NewDynamicWindFrame(before, after values.Callable) DynamicWindFrame {
	id := nextWindingID.Add(1)
	return DynamicWindFrame{
		Before: before,
		After:  after,
		ID:     id,
	}
}

// WindingStack tracks the current dynamic-wind context.
// It's a slice of frames from outermost to innermost.
//
// Frames are stored BY VALUE, so a push into retained capacity costs nothing:
// Pop reslices without dropping capacity, and dynamic-wind at a steady depth
// therefore allocates a spine once instead of a frame per extent.
//
// A WindingStack does NOT own its backing array, and three rules follow from
// that one property:
//
//   - Copy clones the spine, so a captured stack and the live stack no longer
//     share frame objects. Extent identity is ID, which is what
//     FindCommonWindingPrefix has always compared, so that is not a semantic
//     change.
//   - No one may retain &stack[i] across a push. Take the element by value, as
//     unwindStackTo and RewindTo do.
//   - A header handed to another context must be capped or copied. Retained
//     capacity means len < cap after any completed extent, so a bare header
//     gives the recipient a writable alias of slots this stack will reuse.
//     Across a goroutine boundary that is a data race on a 64-byte struct that
//     cannot be written atomically; see CaptureSubContextParams.
//
// The struct is deliberately not ==-comparable (entryMarks is a slice), so
// reach for ID rather than a map[DynamicWindFrame] or qt.Equals.
type WindingStack []DynamicWindFrame

// Copy clones the spine. Frames are copied by value; the Closure interfaces and
// the entryMarks backing array they hold are shared, which is safe only because
// entryMarks is treated as a read-only snapshot.
func (p WindingStack) Copy() WindingStack {
	return slices.Clone(p)
}

// Depth returns the number of active dynamic-wind frames.
func (p WindingStack) Depth() int {
	return len(p)
}

// Push adds a frame to the winding stack.
func (p *WindingStack) Push(frame DynamicWindFrame) {
	*p = append(*p, frame)
}

// Pop removes the innermost frame from the winding stack, reporting false when
// the stack was already empty. The reslice retains capacity, so the next Push
// at this depth reuses the vacated slot.
func (p *WindingStack) Pop() (DynamicWindFrame, bool) {
	if len(*p) == 0 {
		return DynamicWindFrame{}, false
	}
	n := len(*p) - 1
	frame := (*p)[n]
	*p = (*p)[:n]
	return frame, true
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
