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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCallWithContinuationBarrier implements (call-with-continuation-barrier thunk).
//
// Calls thunk with no arguments and returns its result. Establishes a continuation
// barrier: any attempt to invoke a continuation that would cross the barrier boundary
// signals an error. Barriers cannot be re-entered — they return exactly once.
//
// Specifically, any call/cc captured continuation or composable continuation captured inside
// the barrier will fail with a barrier violation if invoked from outside the barrier
// (after the barrier has returned), or from a different barrier context. Similarly,
// continuations captured outside the barrier cannot be invoked from inside it to
// jump out.
//
// Exceptions, prompt aborts, and call-with-exit escapes propagate normally through
// the barrier, since they are upward-only unwinds that do not cross boundaries.
//
// Semantics and cases are pinned by prim_barrier_test.go and
// continuation_subcontext_truncation_red_test.go.
func PrimCallWithContinuationBarrier(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-continuation-barrier")
	if err != nil {
		return err
	}
	thunk := mc.Arg(0)

	thunkCls, err := helpers.RequireType[machine.Closure](thunk, werr.ErrNotAProcedure, "call-with-continuation-barrier")
	if err != nil {
		return err
	}

	// Reify the barrier: push a transparent chain frame carrying the OUTER barrier and
	// flip the live barrier to a fresh token for the thunk, run INLINE on the live chain
	// (not a sub-context). barrierValid now rides the continuation (vmState), so a
	// continuation captured inside the thunk records this barrier and crossing detection
	// still fires at the (k v) site; normal return and resume-out revert to the outer
	// barrier via the frame's restore. This fixes the sub-context truncation
	// (continuation_subcontext_truncation_red_test.go, call-with-continuation-barrier).
	_, err = mc.RunBodyUnderBarrier(thunkCls, machine.NewBarrierToken())
	return err
}
