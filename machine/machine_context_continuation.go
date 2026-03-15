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
	"github.com/aalpar/wile/werr"
)

// cloneMarks returns a shallow copy of marks, or nil if marks is nil/empty.
func cloneMarks(marks []markEntry) []markEntry {
	if len(marks) == 0 {
		return nil
	}
	q := make([]markEntry, len(marks))
	copy(q, marks)
	return q
}

func (p *MachineContext) Restore(cont *MachineContinuation) {
	p.counters.ContinuationsRestored++
	p.env = cont.env
	p.template = cont.template
	p.cont = cont.parent
	p.pc = cont.pc
	// Restore callDepth from the continuation's cached value.
	// Each continuation stores its ancestor count at creation time, which
	// equals the chain length of cont.parent (which is now p.cont).
	// This replaces an O(d) chain walk with an O(1) field read.
	p.callDepth = cont.callDepth
	// Restore always comes from a shared/composable continuation (call/cc
	// re-entry, composable continuation invocation). The old mc.env is NOT
	// released because it may be referenced by continuation frames still in
	// the chain (e.g., SaveContinuation frames created before the composable
	// continuation was invoked). Let GC collect it naturally.
	p.envPooled = false
	p.marks = cloneMarks(cont.marks)

	if cont.evals == nil {
		// Inline: restore from inline slots, reuse mc.evals.
		restoreInlineEvals(p.evals, cont)
	} else {
		// Must copy evals to avoid corrupting the continuation's saved stack.
		// Without copying, modifications to p.evals after restoration would mutate
		// cont.evals, breaking re-invocation of the continuation.
		p.counters.StackPoolReleases++
		oldEvals := p.evals
		p.evals = cont.evals.Copy()
		releaseStack(oldEvals)
	}
}

// RestoreAndRelease is the fast path for normal function return. It transfers
// the continuation's state into the MachineContext (like Restore) but avoids
// copying evals — instead it transfers ownership directly and pools the
// consumed frame. This is safe because normal return consumes the frame
// exactly once; call/cc and escape paths must use Restore (which copies).
//
// Shared frames (marked by MarkChainShared during call/cc capture) cannot be
// pooled because a captured continuation may re-invoke them. For shared frames,
// evals are copied (like Restore) and the frame is left for GC instead of pooling.
//
// The sequence for unshared frames:
//  1. Release mc's current evals to the stack pool (it's dead after restore)
//  2. Transfer cont's evals directly to mc (no copy)
//  3. Nil cont.evals so releaseContinuation won't double-release it
//  4. Pool the consumed continuation frame
func (p *MachineContext) RestoreAndRelease(cont *MachineContinuation) {
	p.counters.ContinuationsRestored++

	oldEnv := p.env
	oldEnvPooled := p.envPooled

	p.env = cont.env
	p.template = cont.template
	p.cont = cont.parent
	p.pc = cont.pc
	p.callDepth = cont.callDepth

	if cont.shared {
		// Shared frame: copy evals (preserve for re-invocation), don't pool
		// the continuation. The restored env must not be released on future
		// overwrites because the shared continuation may be re-invoked.
		p.counters.SharedFrameRestores++
		if cont.evals == nil {
			// Inline + shared: restore from inline slots, reuse mc.evals.
			// Don't nil inline values — shared frame may be re-invoked.
			restoreInlineEvals(p.evals, cont)
		} else {
			p.counters.StackPoolReleases++
			oldEvals := p.evals
			p.evals = cont.evals.Copy()
			releaseStack(oldEvals)
		}
		// envPooled: shared continuation may be re-invoked; env must not be recycled.
		p.envPooled = false
		p.marks = cloneMarks(cont.marks)
		if oldEnvPooled && oldEnv != p.env {
			p.counters.EnvFramePoolReleases++
			releaseEnvFrame(oldEnv)
		}
		return
	}

	// Unshared frame: transfer evals ownership and pool the frame.
	p.counters.ContinuationPoolReleases++
	p.envPooled = cont.envPooled
	p.marks = cont.marks

	if cont.evals == nil {
		// Inline + unshared: restore from inline slots, reuse mc.evals.
		restoreInlineEvals(p.evals, cont)
		// Break GC references in inline slots before pooling.
		for i := uint8(0); i < cont.inlineEvalsLen; i++ {
			cont.inlineEvals[i] = nil
		}
		cont.inlineEvalsLen = 0
	} else {
		// Standard path: transfer stack, release mc's old stack.
		p.counters.StackPoolReleases++
		oldEvals := p.evals
		p.evals = cont.evals // transfer, not copy
		releaseStack(oldEvals)
		// Break the evals reference before pooling so the transferred stack
		// (now p.evals) is not released again inside releaseContinuation.
		cont.evals = nil
	}

	// Only release the old env if it differs from the restored env. When no
	// Apply occurred between SaveContinuation and RestoreContinuation (e.g.,
	// a foreign function call), oldEnv == cont.env and releasing would corrupt
	// the live env.
	if oldEnvPooled && oldEnv != p.env {
		p.counters.EnvFramePoolReleases++
		releaseEnvFrame(oldEnv)
	}
	releaseContinuation(cont)
}

// PopContinuation pops the current continuation from the machine context and returns it.
// It restores the machine context to the state saved in the popped continuation.
// Returns ErrContinuationUnderflow if callDepth would go below zero (compiler bug).
//
// Note: Unlike Restore(), we do NOT copy evals here because PopContinuation is used
// for normal function return where the continuation is consumed once. Restore() is
// used for continuation re-entry (call/cc) where the same continuation may be invoked
// multiple times, requiring the copy to prevent stack corruption.
func (p *MachineContext) PopContinuation() (*MachineContinuation, error) {
	p.callDepth--
	if p.callDepth < 0 {
		p.callDepth = 0
		return nil, werr.WrapForeignErrorf(werr.ErrContinuationUnderflow,
			"callDepth underflow in PopContinuation")
	}
	q := p.cont
	p.template = q.template
	p.env = q.env
	p.cont = q.parent
	p.pc = q.pc
	p.singleValue = q.singleValue
	p.multiValues = q.multiValues
	// envPooled: restore caller's ownership state. Caller (releaseContinuation
	// in Run loop) handles release of the old env via the popped frame.
	p.envPooled = q.envPooled
	p.marks = q.marks

	if q.evals == nil {
		// Inline: restore from inline slots, reuse mc.evals.
		restoreInlineEvals(p.evals, q)
	} else {
		p.evals = q.evals
	}
	return q, nil
}

// SaveContinuation pushes a new continuation onto the machine context with the given offset to the current program counter.
// Returns ErrCallDepthExceeded if the call depth limit has been reached.
//
// Note: callDepth is incremented BEFORE calling NewMachineContinuationFromMachineContext.
// The continuation's own callDepth is derived from mc.cont (the parent pointer), not from
// mc.callDepth, so this pre-increment does not affect the continuation's cached depth.
// See the comment on NewMachineContinuationFromMachineContext for why this matters.
func (p *MachineContext) SaveContinuation(off int) error {
	p.callDepth++
	if p.maxCallDepth > 0 && uint64(p.callDepth) > p.maxCallDepth {
		p.callDepth--
		return werr.WrapForeignErrorf(werr.ErrCallDepthExceeded,
			"call depth %d exceeds limit %d", p.callDepth+1, p.maxCallDepth)
	}
	p.counters.ContinuationsSaved++

	cont := NewMachineContinuationFromMachineContext(p, off)
	// At this point cont.evals and p.evals alias the same Stack.
	// Decide whether to inline the stack values into the continuation.
	n := p.evals.Len()
	if n <= inlineEvalsCap {
		// Inline path: copy values into continuation's inline slots.
		// Reuse mc's existing stack (just clear it for the callee).
		cont.inlineEvalsLen = uint8(n)
		for i := range n {
			cont.inlineEvals[i] = (*p.evals)[i]
		}
		cont.evals = nil // sentinel: values are in inlineEvals
		p.evals.Clear()  // clear for callee; retains backing array
		p.counters.InlineEvalsSaved++
	} else {
		// Standard path: stack transferred to continuation, acquire new for mc.
		p.evals = acquireStack()
	}
	p.cont = cont
	p.marks = nil // callee starts with no marks
	return nil
}

func (p *MachineContext) CurrentContinuation() *MachineContinuation {
	p.cont.MarkChainShared()
	return p.cont
}

// CallDepth returns the depth of the current continuation stack.
func (p *MachineContext) CallDepth() int {
	return p.callDepth
}

// FindPrompt walks the continuation chain to find the nearest frame with
// a matching prompt tag. Also checks the context's own prompt tag (set by
// call-with-continuation-prompt on sub-contexts). Returns the matching
// frame and true, or nil and false.
//
// When the prompt is on the context itself (not a continuation frame), returns
// nil and true — the caller should treat this as "prompt at the boundary of
// this sub-context" and slice the entire continuation chain.
func (p *MachineContext) FindPrompt(tag *PromptTag) (*MachineContinuation, bool) {
	for frame := p.cont; frame != nil; frame = frame.parent {
		if frame.promptTag == tag {
			return frame, true
		}
	}
	if p.promptTag == tag {
		return nil, true
	}
	return nil, false
}

// SliceContinuationAt deep-copies the continuation chain segment from p.cont
// down to (but not including) the prompt frame. The returned chain's bottom
// frame has parent = nil, making it a standalone segment suitable for
// composable continuation capture.
func (p *MachineContext) SliceContinuationAt(prompt *MachineContinuation) *MachineContinuation {
	if p.cont == nil || p.cont == prompt {
		return nil
	}
	// Deep copy frames from p.cont to just before prompt
	top := p.cont.Copy()
	top.parent = nil
	current := top
	src := p.cont.parent
	for src != nil && src != prompt {
		frameCopy := src.Copy()
		frameCopy.parent = nil
		current.parent = frameCopy
		current = frameCopy
		src = src.parent
	}
	return top
}

// GraftContinuation walks the segment chain to its bottom frame and sets
// its parent to target, effectively splicing the segment onto the target chain.
func GraftContinuation(segment, target *MachineContinuation) {
	if segment == nil {
		return
	}
	current := segment
	for current.parent != nil {
		current = current.parent
	}
	current.parent = target
}

// SetPromptTag sets the prompt tag on this context. Used by
// call-with-continuation-prompt to mark sub-contexts as prompt boundaries.
func (p *MachineContext) SetPromptTag(tag *PromptTag) {
	p.promptTag = tag
}

// PromptTag returns the prompt tag for this context, or nil.
func (p *MachineContext) PromptTag() *PromptTag {
	return p.promptTag
}
