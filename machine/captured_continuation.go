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
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
// arguments. Escape continuations accept exactly 1 argument — the value
// to resume with.
func (p *CapturedContinuation) AcceptsArity(n int) bool {
	return n == 1
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
	if p.ThreadID() != capt.threadID {
		return p, werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
			"call/cc: continuation captured in thread %d, invoked from thread %d",
			capt.threadID, p.ThreadID())
	}
	if capt.barrierValid != p.BarrierValid() {
		return p, werr.WrapForeignErrorf(werr.ErrContinuationBarrier,
			"call/cc: continuation cannot cross continuation barrier")
	}

	val := args[0]
	cc := capt.cc

	sub := p.NewSubContext()
	defer ReleaseSubContext(sub)
	// The composable continuation installs its own continuation chain via
	// Restore, replacing whatever marks this sub-context might inherit.
	// Prevent the parent's stale marks from bleeding through findParameterInMarks.
	sub.isolatedMarks = true
	_, err := sub.ApplyCallable(cc, val)
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
