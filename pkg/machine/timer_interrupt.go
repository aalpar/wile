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

// ErrTimerExpired is the context cause set by with-timeout via
// context.WithTimeoutCause. Interrupt check sites compare against this
// to distinguish timer expiry from external cancellation (e.g. Ctrl+C).
var ErrTimerExpired = werr.NewStaticError("timer expired")

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

// supersedingTimerInterrupt returns the interrupt for p's own with-timeout timer
// when that timer has fired and claims err (timerClaims), and nil otherwise.
// bridgeForeignError asks it before turning err into a Scheme condition, and
// RunResumable asks it before dispatching a failure that reached it some other
// way (an uncaught condition, an after thunk's failure during a reconcile).
//
// A failure a primitive returns after the timer fired belongs to the abandoned rest
// of the thunk, so the handler runs instead of a guard seeing it. (A Scheme raise
// is not returned: it reaches its handler in place, and below the owner that
// handler runs before anything notices the expiry.) This is how a
// cancelled blocking wait becomes the timeout: every wait raises
// ErrOperationCancelled, and the context that owns the timer turns the raise into
// the interrupt. A wait in a sub-context with no timer (eval, load, a rootless
// call/cc, a winding thunk) has its failure passed up undispatched instead
// (timerAbove), so no guard between the wait and the timer's owner sees it either,
// and it reaches the owner when the failure returns through whatever started the
// sub-context. So an interrupt is only ever emitted by the context that owns its
// timer, as the poll and the eager rechecks do, and only that context's driver
// resolves it.
//
// Which timer fired is not recorded: every with-timeout cancels with the one
// ErrTimerExpired cause, and a started thread's ctx derives from its starter's, so
// the owning context's innermost timer answers any expiry that reaches it,
// including an outer timer's and a starter thread's. A started thread with no
// with-timeout of its own owns no timer, and its root has no parentMC, so its
// starter's expiry reaches it as an ordinary cancellation. So does code on any
// other root that is not linked to the chain but runs for it synchronously: a
// procedure a host function runs through Engine.Call, a macro transformer at
// expansion time, a library body being instantiated. A handler on such a root
// sees the failure first; it becomes the timeout when it returns to the owner.
//
// timerClaims is asked first: it reads only p's ctx, so the chain walk in
// liveFiredTimer is paid only once a timer has fired, not on every failure and
// control transfer under an armed one.
func (p *MachineContext) supersedingTimerInterrupt(err error) error {
	if !p.timerClaims(err) {
		return nil
	}
	t := p.liveFiredTimer()
	if t == nil {
		return nil
	}
	return &ErrTimerInterrupt{Handler: t.handler, Tag: t.tag}
}

// liveFiredTimer returns p's innermost live with-timeout timer, the innermost one
// whose boundary is still on p's chain, when its context has been cancelled by a
// timer expiry; nil otherwise. An escape past a with-timeout discards the boundary
// without disarming the timer (filed in TODO.md). Such a stale timer is skipped:
// it claims nothing, so a failure after it alone fires is reported as itself, as it
// was before this rule, rather than as an interrupt no driver can resolve; and it
// does not hide a live timer outside it, which still claims a failure once it
// fires. That timer is not p.timer, so resolveTimerInterrupt tears down to the
// timer the interrupt names.
func (p *MachineContext) liveFiredTimer() *timerState {
	for t := p.timer; t != nil; t = t.parent {
		_, found := p.FindPrompt(t.tag)
		if !found {
			continue
		}
		if !errors.Is(context.Cause(t.ctx), ErrTimerExpired) {
			return nil
		}
		return t
	}
	return nil
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
	return p.timerClaims(err) && p.timerAbove()
}

// timerClaims reports whether a with-timeout expiry claims err: p's context was
// cancelled by a timer (cause ErrTimerExpired), err is a failure the timeout
// supersedes (timerClaimable), and p has not already judged it its own
// (judgedUnclaimed).
func (p *MachineContext) timerClaims(err error) bool {
	if !errors.Is(context.Cause(p.ctx), ErrTimerExpired) {
		return false
	}
	return timerClaimable(err) && !p.judgedUnclaimed(err)
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

// judgedUnclaimed reports whether err carries a mark (unclaimedFailure) made
// under one of p's armed timers: p judged the failure outside them. A mark made
// on another context (a thread p joins, a root a host function ran) says nothing
// about p's timers, so a failure reaching p after p's timer fired is still the
// timeout.
func (p *MachineContext) judgedUnclaimed(err error) bool {
	var unclaimed *unclaimedFailure
	for errors.As(err, &unclaimed) {
		if p.armsTimer(unclaimed.timer) {
			return true
		}
		err = unclaimed.err
	}
	return false
}

// unclaimedFailure carries a winding thunk's failure that the timer rule does not
// claim, out through the context that would (windingThunkFailure). timer is that
// context's innermost timer at the time, so the mark answers for that context's
// timers alone (judgedUnclaimed). It adds no text and no meaning of its own: Error
// and Unwrap are the failure's.
type unclaimedFailure struct {
	err   error
	timer *timerState
}

func (p *unclaimedFailure) Error() string {
	return p.err.Error()
}

func (p *unclaimedFailure) Unwrap() error {
	return p.err
}

// timerAbove reports whether a synchronous parent of p, one of the contexts a
// failure on p returns through, owns a live with-timeout timer that has fired:
// the owner that will turn the failure into the timeout. A timer that has not
// fired owns no cancellation, even when p's ctx is cancelled by some other timer
// (a stale one below it), so it must not draw a failure past p's handlers. A stale
// timer (its with-timeout escaped, the timer left armed) owns nothing, here or
// above, so it neither claims a failure nor stops one passing up to a live owner
// (liveFiredTimer). A thread's root has no parentMC, so a timer in the thread that
// started it is not above it.
func (p *MachineContext) timerAbove() bool {
	for c := p.parentMC; c != nil; c = c.parentMC {
		if c.liveFiredTimer() != nil {
			return true
		}
	}
	return false
}
