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
	"sync/atomic"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimCallWithExit implements (call-with-exit proc).
//
// Calls proc with an escape procedure. If the escape procedure is called with a
// value while its extent is live, call-with-exit immediately returns that value
// (after running any dynamic-wind after thunks). If proc returns normally,
// call-with-exit returns that value.
//
// Unlike call/cc, call-with-exit does NOT capture a reified continuation: the
// escape procedure is a lightweight upward escape, valid while its extent is
// live, that is, while call-with-exit's frame is on the continuation of the
// call. Calling it after the extent was left signals an error; a continuation
// that re-enters the extent makes it valid again. This is Racket's call/ec,
// except that a dynamic-wind before thunk a re-entry runs cannot escape to an
// extent only that re-entry enters, and past an expansion-time, Engine.Call or
// nested-Eval root a re-entered extent stays expired (PromptOnAbortPath).
//
// Inspired by S7 Scheme's call-with-exit and Guile's call-with-escape-continuation.
func PrimCallWithExit(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "call-with-exit")
	if err != nil {
		return err
	}
	proc := mc.Arg(0)

	procCls, err := helpers.RequireCallable(proc, "call-with-exit")
	if err != nil {
		return err
	}

	// A runtime env for the exit/finalizer closures, robust to a detached frame:
	// when call-with-exit is invoked inside a reified call-with-values producer the
	// live procedure frame has a nil namespace, so MutableRuntime() would panic. The
	// closures are pure Go; any valid frame serves for apply-time InitApplyFrame.
	closureEnv := mc.ClosureEnv()

	tag := machine.NewPromptTag("exit")
	// left records that this call's own frame was left (normal return or its
	// escape). It is the answer only where the live chain cannot be read, past a
	// root with no link to its caller (PromptOnAbortPath's opaque: an
	// expansion-time root, an Engine.Call root, a nested Eval's root). There a
	// continuation re-entering the extent does not clear it.
	left := &atomic.Bool{}
	capturingThreadID := mc.ThreadID()

	// Build the exit closure. It is valid while the exit frame is on the chain the
	// escape's abort travels (PromptOnAbortPath): a continuation that re-enters the
	// extent makes it valid again; one that jumps out past the frame expires it.
	exitFn := func(innerCC machine.CallContext) error {
		innerMC, err := machine.RequireMachineContext(innerCC, "call-with-exit")
		if err != nil {
			return err
		}
		// Another thread cannot read this thread's chain, and no escape crosses
		// threads: refused whether or not the extent is live.
		if innerMC.ThreadID() != capturingThreadID {
			return werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
				"call-with-exit: exit procedure called from different thread")
		}
		live, opaque := innerMC.PromptOnAbortPath(tag)
		if !live && (!opaque || left.Load()) {
			return werr.WrapForeignErrorf(werr.ErrExpiredEscape,
				"call-with-exit: exit procedure called outside dynamic extent")
		}
		val := innerMC.Arg(0)
		// Marked left BEFORE the abort, for the opaque fallback: an after thunk that
		// re-invokes it from a transformer body is rejected. On the readable chain an
		// after thunk run by this abort still continues the frame, so (as Racket) it
		// may escape to it again. No UnwindTo here: the driver's abort arm
		// (RunResumable / RunWithinBoundary) runs the dynamic-wind after thunks ONCE,
		// against SourceWinding — the winding live HERE at the escape point, which may
		// be a deeper sub-context than the driver holding the exit frame. Unwinding
		// here too would double-fire (C2); reconciling from the driver's own winding
		// instead of SourceWinding would skip a deeper sub's after-thunks (C3).
		left.Store(true)
		return &machine.ErrPromptAbort{
			Tag:           tag,
			Values:        []values.Value{val},
			SourceWinding: innerMC.WindingStack().Copy(),
		}
	}
	exitClosure := machine.NewForeignClosure(closureEnv, 1, false, exitFn)

	// The finalizer runs on BOTH exit paths via the reified exit frame's template:
	//   - normal return: proc's value(s) are in the register; OpPush spreads them
	//     onto the eval stack and OpApply applies the finalizer to them.
	//   - (exit v): the driver delivers v then re-enters this finalizer template.
	// Either way it marks the frame left (the opaque fallback's answer) and forwards
	// the value(s) unchanged. ParamCount 1 + variadic accepts 0/1/N values (AcceptsArity n >= 0).
	finalizerFn := func(finCC machine.CallContext) error {
		left.Store(true)
		finMC, err := machine.RequireMachineContext(finCC, "call-with-exit")
		if err != nil {
			return err
		}
		vals, err := values.CollectList(finMC.Context(), finMC.Arg(0), "call-with-exit")
		if err != nil {
			return err
		}
		finMC.SetValues(vals...)
		return nil
	}
	finalizer := machine.NewForeignClosure(closureEnv, 1, true, finalizerFn)

	// Reify the boundary: push a non-transparent prompt frame carrying tag (so an
	// (exit v) abort routes to it via FindPrompt) whose template marks the frame left
	// and forwards the value(s), then inline-apply proc on the live chain. A
	// continuation captured inside proc now spans the exit frame and everything below
	// it — re-entry replays them instead of truncating.
	_, err = mc.RunBodyUnderExitFrame(procCls, tag, finalizer, exitClosure)
	return err
}
