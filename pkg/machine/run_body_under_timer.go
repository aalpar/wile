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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
func (p *MachineContext) RunBodyUnderTimer(
	timerCtx context.Context, cancel context.CancelFunc,
	thunk values.Value, handler values.Callable,
) (*MachineContext, error) {
	parentCtx := p.ctx
	savedTimer := p.timer
	tag := NewPromptTag("with-timeout")
	p.timer = &timerState{
		handler:  handler,
		cancel:   cancel,
		tag:      tag,
		parent:   savedTimer,
		savedCtx: parentCtx,
	}
	p.ctx = timerCtx

	closureEnv := p.env.MutableRuntimeOrNil()
	if closureEnv == nil {
		closureEnv = p.env
	}
	finalizer := NewForeignClosure(closureEnv, 1, true, func(finCC CallContext) error {
		finMC, ferr := RequireMachineContext(finCC, "with-timeout")
		if ferr != nil {
			return ferr
		}
		cancel()
		finMC.timer = savedTimer
		finMC.ctx = parentCtx
		// Forward 0/1/N values: Arg(0) is the variadic rest-list of the body's values.
		var vals []values.Value
		current := finMC.Arg(0)
		for !values.IsEmptyList(current) {
			tuple, ok := current.(values.Tuple)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAList,
					"with-timeout: improper finalizer argument list")
			}
			vals = append(vals, tuple.Car())
			current = tuple.Cdr()
		}
		finMC.SetValues(vals...)
		return nil
	})
	return p.runBodyUnderApplyFrame(thunk, finalizer, tag)
}
