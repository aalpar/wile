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

package core

import (
	"context"
	"errors"
	"time"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimWithTimeout implements (with-timeout ms handler thunk).
//
// Runs thunk with a wall-clock timeout of ms milliseconds. If the thunk
// completes normally, returns its result. If the timer fires before the
// thunk finishes, the thunk is suspended and handler is called with a
// composable continuation that can resume the computation.
//
// The sub-context pattern follows call-with-continuation-barrier
// (prim_barrier.go): a fresh sub-context isolates the thunk's execution
// while inheriting the parent's environment and winding stack.
func PrimWithTimeout(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)

	msVal, err := helpers.RequireType[*values.Integer](
		mc.Arg(0), werr.ErrNotAnInteger, "with-timeout",
	)
	if err != nil {
		return err
	}

	handlerVal, err := helpers.RequireType[machine.Closure](
		mc.Arg(1), werr.ErrNotAProcedure, "with-timeout",
	)
	if err != nil {
		return err
	}

	thunkVal, err := helpers.RequireType[machine.Closure](
		mc.Arg(2), werr.ErrNotAProcedure, "with-timeout",
	)
	if err != nil {
		return err
	}

	ms := msVal.Value
	if ms < 0 {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"with-timeout: milliseconds must be non-negative, got %d", ms,
		)
	}
	duration := time.Duration(ms) * time.Millisecond

	// Create a child context with the timeout deadline.
	timerCtx, timerCancel := context.WithTimeout(mc.Context(), duration)

	// Run the thunk in a sub-context with the timer installed.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetContext(timerCtx)
	sub.SetTimerHandler(handlerVal)
	sub.SetTimerCancel(timerCancel)

	_, err = sub.ApplyCallable(thunkVal)
	if err != nil {
		timerCancel()
		return err
	}
	err = sub.Run()

	// Always cancel the timer to release resources.
	timerCancel()

	if err != nil {
		var timerErr *machine.ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			// Timer expired. Capture the sub-context's full execution state
			// as a composable continuation.
			segment := sub.CaptureInterruptContinuation()
			windingCopy := sub.WindingStack().Copy()
			resumable := machine.NewComposableContinuation(
				segment, windingCopy, mc.ThreadID(), mc.BarrierValid(),
			)

			// Call the handler with the resumable continuation.
			// ApplyCallable sets mc.template/pc to the handler's code.
			// When the primitive returns nil, the VM loop executes the handler.
			_, applyErr := mc.ApplyCallable(timerErr.Handler, resumable)
			if applyErr != nil {
				return applyErr
			}
			return nil
		}
		return err
	}

	// Normal completion — propagate the thunk's result.
	mc.SetValues(sub.GetValues()...)
	return nil
}
