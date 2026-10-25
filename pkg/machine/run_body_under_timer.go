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
	"time"

	"github.com/aalpar/wile/pkg/values"
)

// RunBodyUnderTimer reifies with-timeout. It installs a wall-clock timer for the
// duration of thunk and runs thunk INLINE on the live chain under a finalizer prompt
// frame (no sub-context), so a continuation captured inside thunk spans the finalizer
// frame and the rest of the program (the sub-context truncation is fixed). The frame
// carries a fresh tag so that a fired timer is routed to THIS with-timeout's frame by a
// driver's FindPrompt (resolveTimerInterrupt) — on whichever chain the with-timeout
// sits, top level or inside a surviving sub-context.
//
// timerState is pushed as a LINKED stack (parent pointer) for nesting, and carries the
// tag plus the saved outer ctx. The finalizer leaves the extent the way an escape
// does (exitTimer): it cancels the timer and restores the outer timer and ctx only
// when the finalizing context still arms it, then forwards the body's 0/1/N values.
// So it is right on a normal return; after the timer arm already tore the fired timer
// down; on a return through a re-entered extent whose timer is over, which must not
// displace the timers armed now; and on one re-entered from inside itself, whose
// timer is still live while an earlier frame for it remains. This helper installs p.timer directly: nesting
// requires pushing a child timer while the parent is still live, via the linked
// parent pointer.
func (p *MachineContext) RunBodyUnderTimer(d time.Duration, thunk values.Value, handler values.Callable) (*MachineContext, error) {
	parentCtx := p.ctx
	tag := NewPromptTag("with-timeout")
	timer := &timerState{
		handler:  handler,
		tag:      tag,
		parent:   p.timer,
		savedCtx: parentCtx,
	}
	// The cause names the timer (timerExpiry), so an expiry is answered by the
	// with-timeout whose timer fired, on whichever context arms it.
	timerCtx, cancel := p.newTimerContext(parentCtx, d, &timerExpiry{timer: timer})
	// The marker lets a thread started anywhere under this ctx, on whichever
	// context, find the timer (ThreadParentContext).
	timerCtx = context.WithValue(timerCtx, timerCtxKey{}, timer)
	timer.cancel = cancel
	timer.ctx = timerCtx
	p.timer = timer
	p.ctx = timerCtx

	closureEnv := p.ClosureEnv()
	finalizer := NewForeignClosure(closureEnv, 1, true, func(finCC CallContext) error {
		finMC, ferr := RequireMachineContext(finCC, "with-timeout")
		if ferr != nil {
			return ferr
		}
		// A normal return leaves the extent: pop its winding frame. After the timer
		// arm's hard suspension the frame is not on top and there is nothing to pop.
		ws := finMC.windingStack
		if len(ws) > 0 && ws[len(ws)-1].timer == timer {
			finMC.PopWindingFrame()
		}
		// A re-entry of the extent from inside it leaves another frame for the
		// timer below: the extent, and the timer, are still live.
		if !finMC.windingStack.holdsTimer(timer) {
			finMC.exitTimer(timer)
		}
		// Forward 0/1/N values: Arg(0) is the variadic rest-list of the body's values,
		// collected under the ctx outside the extent (the body's may be cancelled).
		vals, cerr := values.CollectList(finMC.ctx, finMC.Arg(0), "with-timeout")
		if cerr != nil {
			return cerr
		}
		finMC.SetValues(vals...)
		return nil
	})
	// The boundary frame records the winding stack outside the extent; the timer's
	// own frame goes on above it, so the timer arm's hard suspension, which restores
	// the boundary's stack, drops it, and an escape unwinds through it.
	p.enterBodyFrame(p.newApplyFrame(finalizer, tag))
	p.PushWindingFrame(newTimerFrame(timer))
	return p.ApplyCallable(thunk)
}

// timerClockKey keys a context value that replaces with-timeout's wall clock: a
// timerClock found on the ctx a with-timeout is armed under builds the timer's
// ctx instead of context.WithTimeoutCause. A test installs one to choose the
// instruction a timer fires at, so an interleaving the wall clock reaches only by
// chance is replayed exactly (timer_clock_test.go).
type timerClockKey struct{}

// timerClock builds the ctx of a with-timeout timer armed on p for d. Its expiry
// must cancel the ctx with cause, which names the timer (firedTimer).
type timerClock func(p *MachineContext, parent context.Context, d time.Duration, cause error) (context.Context, context.CancelFunc)

// newTimerContext returns the ctx of a with-timeout timer armed on p for d: the
// wall clock's, unless parent carries a timerClock.
func (p *MachineContext) newTimerContext(parent context.Context, d time.Duration, cause error) (context.Context, context.CancelFunc) {
	clock, ok := parent.Value(timerClockKey{}).(timerClock)
	if ok {
		return clock(p, parent, d, cause)
	}
	return context.WithTimeoutCause(parent, d, cause)
}

// timerCtxKey keys the *timerState a with-timeout's ctx carries, so every ctx
// derived from it (a sub-context's, an Engine.Call root's) leads back to it.
type timerCtxKey struct{}

// ThreadParentContext returns the ctx a thread started under ctx is to run
// beneath. A with-timeout bounds its thunk's computation, not a thread the thunk
// starts: the thread outlives the thunk's normal return and any escape from it,
// and is cancelled when one of the timers armed around its start expires, with
// that expiry as the cause, or when the ctx below them all ends (the engine's,
// the host's, a starting thread's own). The values of ctx are kept. With no timer
// armed, ctx is returned unchanged.
//
// Deriving the thread from ctx itself let the finalizer's cancel of a normal
// return kill the thread with a plain context.Canceled.
func ThreadParentContext(ctx context.Context) context.Context {
	var timers []context.Context
	base := ctx
	for {
		t, ok := base.Value(timerCtxKey{}).(*timerState)
		if !ok {
			break
		}
		timers = append(timers, t.ctx)
		base = t.savedCtx
	}
	if len(timers) == 0 {
		return ctx
	}
	q, cancel := context.WithCancelCause(context.WithoutCancel(ctx))
	context.AfterFunc(base, func() {
		cancel(context.Cause(base))
	})
	for _, tc := range timers {
		// An outer expiry reaches an inner timer's ctx with its own cause, so
		// each check sees whichever expiry ended that timer.
		context.AfterFunc(tc, func() {
			if expiredTimer(tc) != nil {
				cancel(context.Cause(tc))
			}
		})
	}
	return q
}
