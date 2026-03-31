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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/werr"
)

// PrimCallWithContinuationBarrier implements (call-with-continuation-barrier thunk).
//
// Calls thunk with no arguments and returns its result. Establishes a continuation
// barrier: any attempt to invoke a continuation that would cross the barrier boundary
// signals an error. Barriers cannot be re-entered — they return exactly once.
//
// Specifically, any call/cc escape closure or composable continuation captured inside
// the barrier will fail with a barrier violation if invoked from outside the barrier
// (after the barrier has returned), or from a different barrier context. Similarly,
// continuations captured outside the barrier cannot be invoked from inside it to
// jump out.
//
// Exceptions, prompt aborts, and call-with-exit escapes propagate normally through
// the barrier, since they are upward-only unwinds that do not cross boundaries.
//
// See plans/CALL_WITH_EXIT_AND_WITH_BAFFLE.md for full semantics and test cases.
func PrimCallWithContinuationBarrier(mc *machine.MachineContext) error {
	thunk := mc.Arg(0)

	thunkCls, err := helpers.RequireType[machine.Closure](thunk, werr.ErrNotAProcedure, "call-with-continuation-barrier")
	if err != nil {
		return err
	}

	// Create a fresh barrier token. The pointer identity serves as the barrier's
	// unique identity: comparing mc.BarrierValid() pointers at capture vs
	// invocation time detects crossing.
	barrierValid := machine.NewBarrierToken()

	// Run the thunk in a sub-context with the barrier flag set.
	// NewSubContext inherits the parent's barrierValid by default; we override it here
	// to establish a new barrier scope. All further sub-contexts created during the
	// thunk's execution will inherit this barrierValid, so continuations captured
	// inside carry the same pointer and can be compared against it.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetBarrierValid(barrierValid)

	_, err = sub.ApplyCallable(thunkCls)
	if err != nil {
		return err
	}
	err = sub.Run()

	if err != nil {
		return err
	}

	mc.SetValues(sub.GetValues()...)
	return nil
}
