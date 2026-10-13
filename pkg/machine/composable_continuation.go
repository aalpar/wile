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

	"github.com/aalpar/wile/pkg/values"
)

var _ values.Callable = (*ComposableContinuation)(nil)

// ComposableContinuation is a callable value wrapping a delimited continuation
// segment (a chain of MachineContinuation frames) plus the captured winding stack.
// When applied, it splices its frames onto the current continuation, effectively
// composing the captured computation with the current one.
//
// This implements Racket-style composable continuations as described in
// Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
type ComposableContinuation struct {
	cont *MachineContinuation
	// windingStack is the dynamic-wind extents the continuation carries: those
	// inside the prompt the capture is delimited at, which a reinstatement re-enters
	// on top of the extents outside its boundary (reinstallSegment). The extents
	// below that prompt are the capture site's surroundings, not the continuation's.
	windingStack WindingStack
	// promptWinding is, for each prompt frame in the segment, the extents above the
	// delimiting prompt it recorded when it was pushed. A prompt frame records its
	// winding stack absolutely, as the capture site saw it; reinstated under
	// another invoker, the record is rebased onto the extents outside the new
	// boundary (rebasePrompts), or an abort to that prompt, or a resume at it,
	// reconciles to the capture site's surroundings.
	promptWinding []promptWinding
	// inPlace is the boundary a continuation is reinstated in place at: a debugger
	// break's, whose resolution leaves the live winding stack as it was (the break's
	// extents stay entered while the debugger has control). Its reinstatement keeps
	// the extents outside that boundary, which the stack still holds, instead of the
	// invoker's whole stack, or the carried extents would be entered a second time.
	// nil for every other continuation.
	inPlace      *MachineContinuation
	threadID     uint64               // SRFI-18: thread that captured this continuation (0 = primordial)
	barrierValid *BarrierToken        // barrier context at capture time; nil = no active barrier
	bottom       *MachineContinuation // bottom frame of segment; where a re-invocation's copy stops
	consumed     bool                 // true after first AcquireSegment call
	// boundary names the context boundary this segment was delimited at,
	// when call/cc found no prompt frame to delimit it at; nil otherwise. The
	// driver of the context holding the same token resolves a resume of it
	// (resumePrompt). See boundaryToken.
	boundary *boundaryToken
	// root names the root of the context chain the segment was captured on,
	// for every call/cc capture; nil on a composable continuation. See
	// boundaryToken.
	root *boundaryToken
	// capturedMarks is the snapshot of the reachable parameter/handler mark
	// environment at capture time — including the marks ABOVE any sub-context
	// boundary that the captured cont chain does not itself carry. It is re-installed
	// as a findParameterInMarks fallback on resume (applyComposableContinuation), so a
	// continuation captured inside a call-with-values producer (etc.) restores the
	// outer handler/parameter values. See collectReachableMarks.
	capturedMarks []markEntry
	// escalatorArms are the non-continuable finalizer frames' arms carried by this
	// segment, precomputed at capture. Nil for essentially every capture, which is
	// what keeps the resume path's added cost to one length test.
	escalatorArms []*escalatorArm
}

// bottomOfChain walks a continuation chain to its terminal frame (parent == nil).
func bottomOfChain(head *MachineContinuation) *MachineContinuation {
	if head == nil {
		return nil
	}
	current := head
	for current.parent != nil {
		current = current.parent
	}
	return current
}

// collectEscalatorArms gathers the escalator arms of every frame in a chain.
// Deliberately a second walk rather than a return value bolted onto bottomOfChain:
// composable_continuation_test.go pins that helper's signature.
func collectEscalatorArms(head *MachineContinuation) []*escalatorArm {
	var q []*escalatorArm
	for current := head; current != nil; current = current.parent {
		if current.escalatorArm != nil {
			q = append(q, current.escalatorArm)
		}
	}
	return q
}

// NewComposableContinuation creates a composable continuation from a
// continuation chain segment and the winding stack captured at the point
// of capture. barrierValid is mc.BarrierValid() at capture time; nil means
// the capture happened outside any with-continuation-barrier.
//
// windingStack is the capture site's whole stack and windingBase the depth of the
// delimiting prompt on it (MachineContext.WindingDepthAt); the continuation keeps
// the extents above that depth.
func NewComposableContinuation(cont *MachineContinuation, windingStack WindingStack, windingBase int, threadID uint64, barrierValid *BarrierToken) *ComposableContinuation {
	base := min(max(windingBase, 0), len(windingStack))
	q := &ComposableContinuation{
		cont:          cont,
		windingStack:  windingStack[base:],
		promptWinding: collectPromptWinding(cont, base),
		threadID:      threadID,
		barrierValid:  barrierValid,
		bottom:        bottomOfChain(cont),
		escalatorArms: collectEscalatorArms(cont),
	}
	return q
}

// promptWinding is one prompt frame's winding record relative to the capture's
// base: the frame's index in the segment, counted from its top, and the extents
// it recorded above the base.
type promptWinding struct {
	index   int
	carried WindingStack
}

// collectPromptWinding records the winding of every prompt frame in a segment
// relative to base (ComposableContinuation.promptWinding). Nil for a segment with
// no prompt frame, which is what keeps the resume path's added cost to one length
// test.
func collectPromptWinding(head *MachineContinuation, base int) []promptWinding {
	var q []promptWinding
	i := 0
	for current := head; current != nil; current = current.parent {
		if current.promptTag != nil {
			var carried WindingStack
			if len(current.windingStack) > base {
				carried = current.windingStack[base:]
			}
			q = append(q, promptWinding{index: i, carried: carried})
		}
		i++
	}
	return q
}

// rebasePrompts gives the prompt frames of segment, a copy of this continuation's
// frames or the frames themselves, the winding stack they have once reinstated on
// top of outside: outside, then what each recorded above the capture's base,
// composed as the continuation's own extents are (composeWinding).
func (p *ComposableContinuation) rebasePrompts(segment *MachineContinuation, outside WindingStack, shared bool) {
	if len(p.promptWinding) == 0 {
		return
	}
	next := 0
	i := 0
	for current := segment; current != nil && next < len(p.promptWinding); current = current.parent {
		if p.promptWinding[next].index == i {
			current.windingStack = composeWinding(outside, p.promptWinding[next].carried, shared)
			next++
		}
		i++
	}
}

// composeWinding returns the winding stack of extents carried reinstated on top
// of outside: a fresh stack, outside then carried. With shared, the reinstatement
// is at a context's own boundary, not a prompt the program wrote (a thread root,
// eval, a winding thunk's sub-context): an extent the context inherited that the
// continuation also carries (ResumeWindingDepth delimits a capture inside eval at
// the driver's prompt above) is the same extent, kept once rather than re-entered
// on top of itself.
func composeWinding(outside, carried WindingStack, shared bool) WindingStack {
	q := slices.Concat(outside, carried)
	if !shared || len(outside) == 0 {
		return q
	}
	q = q[:len(outside)]
	for _, frame := range carried {
		if !outside.holds(frame.ID) {
			q = append(q, frame)
		}
	}
	return q
}

// Cont returns the continuation segment captured by this composable continuation.
func (p *ComposableContinuation) Cont() *MachineContinuation {
	return p.cont
}

// ReinstateInPlace marks this continuation as one reinstated in place at boundary
// (ComposableContinuation.inPlace): a debugger break's resumable.
func (p *ComposableContinuation) ReinstateInPlace(boundary *MachineContinuation) {
	p.inPlace = boundary
}

// WindingStack returns the dynamic-wind extents this composable continuation
// carries: those inside the prompt its capture is delimited at.
func (p *ComposableContinuation) WindingStack() WindingStack {
	return p.windingStack
}

// BarrierValid returns the barrier identity token captured when this continuation
// was created. Used by applyComposableContinuation to detect barrier crossings.
func (p *ComposableContinuation) BarrierValid() *BarrierToken {
	return p.barrierValid
}

// AcquireSegment returns the continuation segment for grafting.
//
// First invocation marks the segment shared and returns it directly, avoiding
// a DeepCopy. Shared marking ensures RestoreAndRelease preserves frame evals
// for potential re-invocation.
//
// Subsequent invocations deep-copy the preserved shared frames down to the
// bottom frame. The copy stops at bottom rather than at a nil parent because
// GraftContinuation hung the original bottom onto the first invocation's chain,
// and that invocation may still be running: re-invoking from inside it must not
// write the edge its return goes through.
//
// This optimizes one-shot continuations (the common case in Schelog-style
// backtracking), eliminating O(depth) frame allocations per invocation.
func (p *ComposableContinuation) AcquireSegment() *MachineContinuation {
	if p.cont == nil {
		return nil
	}
	if !p.consumed {
		p.consumed = true
		p.cont.MarkChainShared()
		return p.cont
	}
	return p.cont.DeepCopyThrough(p.bottom)
}

// AcceptsArity reports whether this composable continuation can be called with
// n arguments. A continuation accepts ANY number of values (R7RS §6.10): it
// resumes with however many values it is invoked with — zero, one, or several.
// Always true; there is no rejecting arity (n is an argument count, never negative).
func (*ComposableContinuation) AcceptsArity(int) bool {
	return true
}

// SchemeString returns a string representation of this composable continuation.
func (p *ComposableContinuation) SchemeString() string {
	return "#<composable-continuation>"
}

// IsVoid reports whether this composable continuation is void.
func (p *ComposableContinuation) IsVoid() bool {
	return p == nil
}

// EqualTo reports whether this composable continuation is equal to another.
func (p *ComposableContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*ComposableContinuation)
	if !ok {
		return false
	}
	return p == v
}
