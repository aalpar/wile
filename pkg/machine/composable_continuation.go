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
	cont         *MachineContinuation
	windingStack WindingStack
	threadID     uint64               // SRFI-18: thread that captured this continuation (0 = primordial)
	barrierValid *BarrierToken        // barrier context at capture time; nil = no active barrier
	bottom       *MachineContinuation // bottom frame of segment; for parent reset on re-invocation
	consumed     bool                 // true after first AcquireSegment call
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

// NewComposableContinuation creates a composable continuation from a
// continuation chain segment and the winding stack captured at the point
// of capture. barrierValid is mc.BarrierValid() at capture time; nil means
// the capture happened outside any with-continuation-barrier.
func NewComposableContinuation(cont *MachineContinuation, windingStack WindingStack, threadID uint64, barrierValid *BarrierToken) *ComposableContinuation {
	q := &ComposableContinuation{
		cont:         cont,
		windingStack: windingStack,
		threadID:     threadID,
		barrierValid: barrierValid,
		bottom:       bottomOfChain(cont),
	}
	return q
}

func (p *ComposableContinuation) Cont() *MachineContinuation {
	return p.cont
}

func (p *ComposableContinuation) WindingStack() WindingStack {
	return p.windingStack
}

func (p *ComposableContinuation) ThreadID() uint64 {
	return p.threadID
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
// Subsequent invocations reset the bottom frame's parent to nil (undoing
// GraftContinuation's mutation from the prior invocation) and deep-copy
// from the preserved shared frames.
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
	// Re-invocation: undo GraftContinuation's parent mutation so
	// DeepCopy produces a self-contained segment (bottom.parent == nil).
	if p.bottom != nil {
		p.bottom.parent = nil
	}
	return p.cont.DeepCopy()
}

// AcceptsArity reports whether this composable continuation can be called with
// n arguments. Composable continuations accept exactly 1 argument — the value
// to resume with.
func (p *ComposableContinuation) AcceptsArity(n int) bool {
	return n == 1
}

func (p *ComposableContinuation) SchemeString() string {
	return "#<composable-continuation>"
}

func (p *ComposableContinuation) IsVoid() bool {
	return p == nil
}

func (p *ComposableContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*ComposableContinuation)
	if !ok {
		return false
	}
	return p == v
}
