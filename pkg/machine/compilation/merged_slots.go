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

package compilation

// merged_slots.go implements `let`-slot merging: a `let` keeps its compile-time
// frame, so hygiene and every pkg/internal/validate predicate see exactly what
// they saw before, but its slots are allocated in the ENCLOSING lambda's
// parameter frame and it emits no OpPushEnv/OpPopEnv at all.
//
// WHY THE COMPILE-TIME FRAME STAYS. Appending the binders to the enclosing
// frame directly is the obvious formulation and it is wrong twice.
// MaybeCreateLocalBinding dedups on scope-set EQUALITY, so `(lambda (x) (let ((x 1)) …))`
// would find the parameter's slot and alias it — duplicate-parameter detection
// and `let` shadowing are the same call. And one frame is one scope: two
// same-named binders in it with identical (commonly empty) scope sets are an
// equal-cardinality incomparable tie, which resolveLocal answers by panicking
// with ErrAmbiguousBinding. There is no "innermost wins" in this model, and
// adding one would be a hygiene change rather than an allocation change.
//
// The merged frame costs nothing at run time — it is never instantiated — and
// absoluteSlot, boxedSlots, freeSlotOf and collectFreeVars keep reading the
// compile-time index. Only the operation operand is translated, at the six sites
// that build one.

import (
	"github.com/aalpar/wile/pkg/environment"
)

// ensureMergeState allocates the shared merge maps on first use.
//
// Allocated lazily but shared eagerly, mirroring ensureBoxedSlots: compileBody
// calls this before handing the maps to the child continuation, so one pair
// serves a whole compile even when the outermost body merges nothing.
func (p *CompileTimeContinuation) ensureMergeState() {
	if p.mergedShape == nil {
		p.mergedShape = map[*environment.EnvironmentFrame]*environment.EnvironmentFrame{}
	}
	if p.mergedSlot == nil {
		p.mergedSlot = map[boxedSlotKey]int{}
	}
}

// canMergeLet reports whether a `let` compiled at p.env may take its slots from
// p.shape instead of pushing a frame.
//
// Every compile-time frame between the `let` and the shape frame must itself be
// merged. Otherwise a real runtime frame sits in between, and a slot in the
// shape frame is not reachable at the depth the merged arithmetic assumes.
//
// The two populations this refuses are exactly the two that must be refused:
//
//   - THE TOP LEVEL. p.shape is nil on the program, library and transformer
//     compilers, so a top-level `let` still pushes. There is no enclosing frame.
//   - A SYNTAX-CASE CLAUSE BODY. BindPatternVars pushes a pattern-variable frame
//     at run time between the parameter frame and the body, and its width is
//     fixed by the pattern rather than by this compiler. The walk stops at that
//     body frame, which is not merged.
func (p *CompileTimeContinuation) canMergeLet() bool {
	if p.shape == nil {
		return false
	}
	env := p.env
	for env != nil {
		if env == p.shape {
			return true
		}
		if p.mergedShape[env] == nil {
			return false
		}
		env = env.Parent()
	}
	return false
}

// markMerged records that frame's slots live in p.shape and that frame therefore
// does not exist at run time.
func (p *CompileTimeContinuation) markMerged(frame *environment.EnvironmentFrame) {
	p.ensureMergeState()
	p.mergedShape[frame] = p.shape
}

// shapeSlotFor returns the slot that frame's local slot occupies in the frame it
// was merged into, allocating one on first use.
//
// ALLOCATED LAZILY, ONE SLOT AT A TIME, and that is not a micro-optimization: a
// merged frame's binding count is not fixed when its `let` starts.
// predeclareBodyDefines runs before the body, and withBoxOnDeclare's safety net
// can create a slot at any point during it. A scheme that reserved a contiguous
// block [base, base+n) per `let` would be corrupted the moment an outer `let`
// grew after an inner one had taken its base — a silent slot collision. Per-slot
// allocation removes the ordering constraint entirely.
//
// The shape frame is looked up per merged frame rather than read off p.shape,
// because a continuation may translate an index owned by an OUTER lambda's
// merged `let`: a body that retains its lexical env (bodyReadsThroughFrameChain)
// reaches such a slot with a real depth rather than through its free vector.
func (p *CompileTimeContinuation) shapeSlotFor(frame *environment.EnvironmentFrame, slot int) (int, bool) {
	k := boxedSlotKey{frame: frame, slot: slot}
	s, ok := p.mergedSlot[k]
	if ok {
		return s, true
	}
	shape := p.mergedShape[frame]
	if shape == nil {
		return 0, false
	}
	lenv := shape.LocalEnvironment()
	if lenv == nil {
		return 0, false
	}
	s = lenv.AppendAnonymousSlot()
	p.mergedSlot[k] = s
	return s, true
}

// runtimeIndex converts a compile-time (slot, depth) into the (slot, depth) the
// emitted instruction carries: merged frames are skipped when counting depth,
// and a slot owned by one is renamed to its slot in the frame it merged into.
//
// It is applied ONLY where an operation operand is built. Every analysis that
// keys on a slot — localIsBoxed, freeSlotOf, absoluteSlot — must keep seeing the
// compile-time index, or the binder's verdict and the reference's lookup stop
// agreeing.
func (p *CompileTimeContinuation) runtimeIndex(li *environment.LocalIndex) *environment.LocalIndex {
	if len(p.mergedShape) == 0 || li == nil || p.env == nil {
		return li
	}
	env := p.env
	depth := 0
	for range li.Up() {
		// Leaving a frame that exists at run time costs one level; leaving a
		// merged one costs nothing, because it and its shape are one frame.
		if p.mergedShape[env] == nil {
			depth++
		}
		env = env.Parent()
		if env == nil {
			// The index did not come from this chain. Emitting it unchanged
			// reproduces the pre-merge behaviour rather than inventing a slot.
			return li
		}
	}
	slot, merged := p.shapeSlotFor(env, li.Over())
	if !merged {
		if depth == li.Up() {
			return li
		}
		return environment.NewLocalIndex(li.Over(), depth)
	}
	return environment.NewLocalIndex(slot, depth)
}
