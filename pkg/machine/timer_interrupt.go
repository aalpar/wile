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
	"context"
	"errors"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrTimerExpired is what a with-timeout expiry's context cause matches with
// errors.Is: the cause itself is a timerExpiry naming the timer, which unwraps to
// this sentinel. A host can tell a timer expiry from an external cancellation
// (e.g. Ctrl+C) by it. A context cancelled with the bare sentinel names no timer,
// so no with-timeout answers it.
var ErrTimerExpired = werr.NewStaticError("timer expired")

// timerExpiry is the cancellation cause of one with-timeout's timer. It names the
// timer, so every ctx the expiry cancels answers which timer fired (expiredTimer):
// the timer's own, a nested with-timeout's below it, a started thread's derived
// from it. errors.Is(cause, ErrTimerExpired) still holds through Unwrap.
type timerExpiry struct {
	timer *timerState
}

func (p *timerExpiry) Error() string {
	return ErrTimerExpired.Error()
}

func (p *timerExpiry) Unwrap() error {
	return ErrTimerExpired
}

// expiredTimer returns the timer whose expiry cancelled ctx; nil when ctx is live
// or something else cancelled it.
func expiredTimer(ctx context.Context) *timerState {
	var expiry *timerExpiry
	if !errors.As(context.Cause(ctx), &expiry) {
		return nil
	}
	return expiry.timer
}

// firedTimer returns the timer an expiry on p's ctx names when p arms it, the
// timer whose handler answers the expiry; nil when no timer cancelled the ctx, or
// when the one that did is not p's (an enclosing context's, a starting thread's),
// so that its owner answers it.
func (p *MachineContext) firedTimer() *timerState {
	t := expiredTimer(p.ctx)
	if t == nil || !p.armsTimer(t) {
		return nil
	}
	return t
}

// ErrTimerInterrupt signals that a wall-clock timer has expired.
// It propagates through the Go error return path and is resolved by the nearest
// driver (RunResumable at the top level, or RunWithinBoundary in a surviving
// sub-context), which locates the with-timeout's finalizer frame via FindPrompt(Tag).
//
// This is a signal, not an exception. It is NOT caught by Scheme exception
// handlers — only by the VM infrastructure that installed the timer.
type ErrTimerInterrupt struct {
	Handler values.Callable
	// Tag identifies the with-timeout boundary (its finalizer frame) on the
	// continuation chain. A driver does FindPrompt(Tag) to locate that frame on the
	// chain the with-timeout actually installed — its OWN chain, even when the
	// with-timeout sits inside a surviving sub-context — and resolves the timer there.
	Tag *PromptTag
}

func (p *ErrTimerInterrupt) Error() string {
	return "timer interrupt"
}

// supersedingTimerInterrupt returns the interrupt for the timer whose expiry
// cancelled p's ctx when p arms that timer (firedTimer) and the timeout supersedes
// err (timerClaimable), and nil otherwise. bridgeForeignError asks it before
// turning err into a Scheme condition, and the drivers ask it before dispatching a
// failure that reached them some other way (an uncaught condition, a winding
// thunk's failure during a reconcile).
//
// A failure a primitive returns after the timer fired belongs to the abandoned rest
// of the thunk, so the handler runs instead of a guard seeing it. (A Scheme raise
// is not returned: it reaches its handler in place, and below the owner that
// handler runs before anything notices the expiry.) This is how a
// cancelled blocking wait becomes the timeout: every wait raises
// ErrOperationCancelled, and the context that arms the timer turns the raise into
// the interrupt. A wait in a sub-context that does not arm it (eval, load, a
// rootless call/cc, a winding thunk) has its failure passed up undispatched instead
// (timerAbove), so no guard between the wait and the timer's owner sees it either,
// and it reaches the owner when the failure returns through whatever started the
// sub-context. So an interrupt is only ever emitted by the context that arms its
// timer, as the poll and the eager rechecks do, and only that context's driver
// resolves it.
//
// The expiry names its timer (timerExpiry), so the with-timeout whose timer fired
// answers it, not the innermost one armed. A started thread's ctx derives from its
// starter's, and its root has no parentMC: its starter's expiry names a timer the
// thread does not arm, and reaches it as an ordinary cancellation. So does code on
// any other root that is not linked to the chain but runs for it synchronously: a
// procedure a host function runs through Engine.Call, a macro transformer at
// expansion time, a library body being instantiated. A handler on such a root
// sees the failure first; it becomes the timeout when it returns to the owner.
func (p *MachineContext) supersedingTimerInterrupt(err error) error {
	t := p.firedTimer()
	if t == nil || !timerClaimable(err) {
		return nil
	}
	return &ErrTimerInterrupt{Handler: t.handler, Tag: t.tag}
}

// FailureClaimedByTimer reports whether the timer rule claims err on p: a
// with-timeout timer on p's chain fired and err is a failure the timeout
// supersedes, so bridgeForeignError would turn it into the interrupt (p owns the
// timer) or pass it up undispatched (a parent owns it). thread-join! and
// mutex-lock!, which raise their SRFI-18 conditions in place (RaiseInPlace)
// instead of returning a failure, ask this first and return the failure when it is
// claimed, so no handler between them and the timer's owner sees it, as for a
// returned failure. raise, raise-continuable and error raise in place without
// asking.
func (p *MachineContext) FailureClaimedByTimer(err error) bool {
	if p.supersedingTimerInterrupt(err) != nil {
		return true
	}
	return p.timerAbove() && timerClaimable(err)
}

// timerClaimable reports whether err is a failure a timeout supersedes, on a
// context whose timer fired. Not claimable: a control signal other than an
// uncaught-condition escape (a continuation jump, an abort, a timer interrupt, a
// debugger break already carry their own control flow); and a security denial,
// which must reach the embedder however the program is written.
func timerClaimable(err error) bool {
	var escape *ErrExceptionEscape
	if isControlSignal(err) && !errors.As(err, &escape) {
		return false
	}
	return !errors.Is(err, security.ErrAccessDenied)
}

// timerAbove reports whether a synchronous parent of p, one of the contexts a
// failure on p returns through, arms the timer whose expiry cancelled p's ctx: the
// owner that will turn the failure into the timeout. A thread's root has no
// parentMC, so a timer in the thread that started it is not above it.
func (p *MachineContext) timerAbove() bool {
	t := expiredTimer(p.ctx)
	if t == nil {
		return false
	}
	for c := p.parentMC; c != nil; c = c.parentMC {
		if c.armsTimer(t) {
			return true
		}
	}
	return false
}
