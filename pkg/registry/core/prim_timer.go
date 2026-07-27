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
	"time"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimWithTimeout implements (with-timeout ms handler thunk).
//
// Runs thunk with a wall-clock timeout of ms milliseconds. If the thunk
// completes normally, returns its result. If the timer fires before the
// thunk finishes, the thunk is suspended and handler is called with a
// composable continuation that can resume the computation.
//
// The thunk runs inline on the live continuation chain under a finalizer prompt
// frame (RunBodyUnderTimer), mirroring call-with-continuation-barrier
// (prim_barrier.go): a continuation captured inside the thunk spans the
// finalizer frame and the rest of the program rather than being truncated at a
// sub-context boundary.
func PrimWithTimeout(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "with-timeout")
	if err != nil {
		return err
	}

	msVal, err := helpers.RequireType[*values.Integer](
		mc.Arg(0), werr.ErrNotAnInteger, "with-timeout",
	)
	if err != nil {
		return err
	}

	handlerVal, err := helpers.RequireType[values.Callable](
		mc.Arg(1), werr.ErrNotAProcedure, "with-timeout",
	)
	if err != nil {
		return err
	}

	thunkVal, err := helpers.RequireType[values.Callable](
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
	// time.Duration is int64 nanoseconds; multiplying by time.Millisecond
	// (1e6) overflows for ms > ~9.2e12, wrapping negative and causing an
	// immediate spurious timeout. Cap at the representable maximum.
	const maxMilliseconds = int64(time.Duration(1<<63-1) / time.Millisecond)
	if ms > maxMilliseconds {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"with-timeout: milliseconds %d exceeds maximum representable duration", ms,
		)
	}
	duration := time.Duration(ms) * time.Millisecond

	// Create a child context with a cause-tagged timeout deadline.
	// The cause (ErrTimerExpired) lets interrupt check sites distinguish
	// timer expiry from external cancellation (e.g. Ctrl+C).
	timerCtx, timerCancel := context.WithTimeoutCause(mc.Context(), duration, machine.ErrTimerExpired)

	// Reify with-timeout: run the thunk INLINE on the live chain under a finalizer
	// prompt frame carrying a fresh tag (RunBodyUnderTimer), not in a sub-context, so a
	// continuation captured inside the thunk spans the finalizer frame and the rest of
	// the program (the sub-context truncation is fixed). A fired timer is routed to this
	// frame by the driver's FindPrompt (resolveTimerInterrupt — present in BOTH the top
	// RunResumable and the sub-context RunWithinBoundary, so with-timeout works at top
	// level and nested inside a surviving sub-context); it hard-suspends the thunk and
	// runs the handler with a resumable composable continuation, and the finalizer tears
	// the timer down and forwards values on every exit. Fixes the with-timeout case of
	// continuation_subcontext_truncation_red_test.go.
	_, err = mc.RunBodyUnderTimer(timerCtx, timerCancel, thunkVal, handlerVal)
	return err
}
