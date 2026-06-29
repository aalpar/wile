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
	"github.com/aalpar/wile/pkg/werr"
)

var _ values.Callable = (*CapturedContinuation)(nil)

// CapturedContinuation is the value returned by call/cc to Scheme code.
// It is both callable (invoking it escapes to the captured point) and
// introspectable (continuation-marks can extract marks from its chain).
//
// This replaces the opaque ForeignClosure that call/cc previously returned.
// The escape logic (thread check, barrier check, compose-then-abort) now
// lives in applyCapturedContinuation rather than inside a Go closure.
type CapturedContinuation struct {
	cc           *ComposableContinuation
	threadID     uint64
	barrierValid *BarrierToken
}

// NewCapturedContinuation creates a captured continuation value from a
// composable continuation segment and the safety context at capture time.
func NewCapturedContinuation(
	cc *ComposableContinuation,
	threadID uint64,
	barrierValid *BarrierToken,
) *CapturedContinuation {
	return &CapturedContinuation{
		cc:           cc,
		threadID:     threadID,
		barrierValid: barrierValid,
	}
}

// ComposableContinuation returns the underlying composable continuation,
// which carries the MachineContinuation chain for mark extraction.
func (p *CapturedContinuation) ComposableContinuation() *ComposableContinuation {
	return p.cc
}

// AcceptsArity reports whether this continuation can be called with n
// arguments. A continuation accepts ANY number of values (R7RS §6.10): it
// resumes the captured computation with however many values it is invoked with
// — zero, one, or several. Always true; there is no rejecting arity (n is an
// argument count, never negative).
func (*CapturedContinuation) AcceptsArity(int) bool {
	return true
}

func (p *CapturedContinuation) SchemeString() string {
	return "#<continuation>"
}

func (p *CapturedContinuation) IsVoid() bool {
	return p == nil
}

func (p *CapturedContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*CapturedContinuation)
	if !ok {
		return false
	}
	return p == v
}

// applyCapturedContinuation implements the escape logic of a call/cc continuation.
// It checks thread identity and barrier validity, then returns an
// ErrResumeContinuation control signal that the nearest DefaultPromptTag driver
// (RunResumable) reinstalls into the live continuation and keeps looping — the
// trampoline. It does NOT run the captured chain here.
//
// This is the flip that resolves the continuation-resume cluster. The previous form
// ran the captured chain in a fresh sub-context (sub.Run()) and aborted to
// DefaultPromptTag with the result. That nests O(live continuation depth) Go frames
// per resume — the ctak / -race stack overflow — and reconciles dynamic-wind winding
// twice on an escape-out (ReinstallSegment AND the abort catch = the double-fire).
// Returning the segment UNRUN and letting the driver reinstall it on ITSELF (boundary
// = nil, the abortive replace) runs the resumed chain on the driver's own Run loop:
// O(1) Go frames, and a SINGLE winding reconcile carrying the source winding from this
// (k v) site (fixing the deeper-sub after-thunk skip). Because every continuation
// boundary is now a chain frame, a full k aborts to DefaultPromptTag discarding the
// chain-resident consumer/exit/prompt frames (escape-past preserved), while a k
// captured inside a producer spans them (truncation fixed).
func (p *MachineContext) applyCapturedContinuation(
	capt *CapturedContinuation,
	args []values.Value,
) (*MachineContext, error) {
	// Thread confinement: a continuation captured in one thread cannot be
	// invoked from another. Load-bearing for the per-thread allocation pool
	// design — it guarantees no goroutine ever releases a frame allocated by
	// another. Do not relax without reworking allocation; see
	// plans/2026-06-08-per-thread-pools-invariant.md and the regression test
	// TestCrossThreadContinuationIsAllocatorInvariant.
	if p.ThreadID() != capt.threadID {
		return p, werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
			"call/cc: continuation captured in thread %d, invoked from thread %d",
			capt.threadID, p.ThreadID())
	}
	if capt.barrierValid != p.BarrierValid() {
		return p, werr.WrapForeignErrorf(werr.ErrContinuationBarrier,
			"call/cc: continuation cannot cross continuation barrier")
	}

	// Copy the invocation values off the eval stack before the trampoline unwinds:
	// when the caller used Drain (zero-copy) args shares the eval-stack backing array,
	// which the resume's Restore recycles. Forward ALL of them (R7RS §6.10): the
	// captured continuation resumes with however many values it was called with.
	vals := make([]values.Value, len(args))
	copy(vals, args)

	// This signal is emitted only for call/cc resume, which always isolates marks on
	// reinstall (the captured snapshot in capt.cc, taken at capture time, restores the
	// outer parameter/handler environment). The driver passes isolate=true to
	// ReinstallSegment accordingly. Composable resume composes the invoker's marks
	// instead and bypasses this signal (it calls ReinstallSegment directly).
	return p, &ErrResumeContinuation{
		Tag:           DefaultPromptTag,
		Segment:       capt.cc,
		Values:        vals,
		SourceWinding: p.windingStack.Copy(),
	}
}
