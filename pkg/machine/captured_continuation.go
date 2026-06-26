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

// applyCapturedContinuation implements the escape logic previously in
// newComposeAbortEscapeClosure. It checks thread identity and barrier
// validity, applies the composable continuation in a sub-context, then
// aborts to DefaultPromptTag with the result.
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

	cc := capt.cc

	// Bound the LIVE Go-stack nesting of continuation re-invocation. Restoring the
	// captured chain runs it in a fresh sub-context whose Run() frame stays live on
	// the Go stack until that chain completes; a continuation that re-invokes itself
	// without converging (a call/cc loop) nests Go frames without bound until the
	// runtime aborts the process with a stack overflow, bypassing the eval-stack
	// maxCallDepth gate in SaveContinuation (each restored chain resets to the
	// captured, shallow callDepth).
	//
	// This bound is SEPARATE from maxCallDepth (see maxContinuationDepth): the two
	// guard different resources. maxCallDepth bounds eval-stack call depth, a memory
	// budget (~500 B/frame). Continuation re-invocation nests on the Go goroutine
	// stack, whose ceiling is far higher (~650k live frames before Go's 1 GB fatal
	// overflow). The counter is contNestDepth on the per-thread pools: it tracks the
	// frames CURRENTLY live (incremented here, decremented by the defer below when
	// this re-invocation unwinds), so a converging program that resumes-then-unwinds
	// — e.g. ctak, whose tree of resumes peaks ~100k live frames but mostly unwinds —
	// is measured by its true Go-stack depth, not the cumulative resume count. A
	// cumulative count over-counts such programs and trips on legitimate deep-but-
	// converging continuation code; the live count clears them while still tripping a
	// true runaway loop (which never unwinds) before the Go fatal overflow.
	//
	// pools is nil only for rootless cold/expand-time contexts, which never invoke
	// runtime captured continuations; the guard keeps those panic-free.
	if p.pools != nil {
		p.pools.contNestDepth++
		defer func() {
			p.pools.contNestDepth--
		}()
		if p.maxContinuationDepth > 0 && p.pools.contNestDepth > p.maxContinuationDepth {
			return p, werr.WrapForeignErrorf(werr.ErrCallDepthExceeded,
				"call/cc: continuation re-invocation depth %d exceeds limit %d",
				p.pools.contNestDepth, p.maxContinuationDepth)
		}
	}

	sub := p.NewSubContext()
	defer ReleaseSubContext(sub)
	// The composable continuation installs its own continuation chain via
	// Restore, replacing whatever marks this sub-context might inherit.
	// Prevent the parent's stale marks from bleeding through findParameterInMarks.
	sub.isolatedMarks = true
	// Forward ALL invocation values (R7RS §6.10): the captured continuation
	// resumes with however many values it was called with. applyComposableContinuation
	// copies args before any Restore, so passing the slice through is safe.
	_, err := sub.ApplyCallable(cc, args...)
	if err != nil {
		return p, err
	}
	err = sub.Run()
	if err != nil {
		return p, err
	}

	return p, &ErrPromptAbort{
		Tag:    DefaultPromptTag,
		Values: sub.GetValues(),
	}
}
