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
// tag plus the saved outer ctx. The finalizer does an IDEMPOTENT ABSOLUTE teardown —
// cancel the timer, restore the outer timer + ctx, forward the body's 0/1/N values — so
// it is correct on normal completion AND, run a second time, after the timer arm already
// tore the fired timer down (resolveTimerInterrupt restores the same outer state). This
// helper installs and restores p.timer directly: nesting requires pushing a child timer
// while the parent is still live, via the linked parent pointer.
func (p *MachineContext) RunBodyUnderTimer(d time.Duration, thunk values.Value, handler values.Callable) (*MachineContext, error) {
	parentCtx := p.ctx
	savedTimer := p.timer
	tag := NewPromptTag("with-timeout")
	timer := &timerState{
		handler:  handler,
		tag:      tag,
		parent:   savedTimer,
		savedCtx: parentCtx,
	}
	// The cause names the timer (timerExpiry), so an expiry is answered by the
	// with-timeout whose timer fired, on whichever context arms it.
	timerCtx, cancel := context.WithTimeoutCause(parentCtx, d, &timerExpiry{timer: timer})
	timer.cancel = cancel
	p.timer = timer
	p.ctx = timerCtx

	closureEnv := p.ClosureEnv()
	finalizer := NewForeignClosure(closureEnv, 1, true, func(finCC CallContext) error {
		finMC, ferr := RequireMachineContext(finCC, "with-timeout")
		if ferr != nil {
			return ferr
		}
		cancel()
		finMC.timer = savedTimer
		finMC.ctx = parentCtx
		// A normal return leaves the extent: pop its winding frame. After the timer
		// arm's hard suspension, or on a re-entry that never pushed it, the frame is
		// not on top and there is nothing to pop.
		ws := finMC.windingStack
		if len(ws) > 0 && ws[len(ws)-1].timer == timer {
			finMC.PopWindingFrame()
		}
		// Forward 0/1/N values: Arg(0) is the variadic rest-list of the body's values.
		// Collected under parentCtx, which finMC.ctx was just restored to — the
		// body's timer context may already be cancelled.
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
