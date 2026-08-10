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
// Calls proc with a single-use escape procedure. If the escape procedure is
// called with a value during proc's dynamic extent, call-with-exit immediately
// returns that value (after running any dynamic-wind after thunks). If proc
// returns normally, call-with-exit returns that value and the escape procedure
// is invalidated.
//
// Unlike call/cc, call-with-exit does NOT capture a reified continuation —
// the escape procedure is a lightweight one-shot upward escape. Calling the
// escape after call-with-exit has returned signals an error.
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
	closureEnv := mc.EnvironmentFrame().MutableRuntimeOrNil()
	if closureEnv == nil {
		closureEnv = mc.EnvironmentFrame()
	}

	tag := machine.NewPromptTag("exit")
	valid := &atomic.Bool{}
	valid.Store(true)
	capturingThreadID := mc.ThreadID()

	// Build the exit closure. It is valid only during the dynamic extent of proc.
	// Checking valid before thread is intentional: a cross-thread call to an expired
	// exit procedure gets the "outside dynamic extent" error, which is more informative.
	exitFn := func(innerCC machine.CallContext) error {
		innerMC, err := machine.RequireMachineContext(innerCC, "call-with-exit")
		if err != nil {
			return err
		}
		if !valid.Load() {
			return werr.WrapForeignErrorf(werr.ErrExpiredEscape,
				"call-with-exit: exit procedure called outside dynamic extent")
		}
		if innerMC.ThreadID() != capturingThreadID {
			return werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
				"call-with-exit: exit procedure called from different thread")
		}
		val := innerMC.Arg(0)
		// Invalidate the one-shot BEFORE the abort so an after-thunk that re-invokes
		// it is rejected (expired). No UnwindTo here: the driver's abort arm
		// (RunResumable / RunWithinBoundary) runs the dynamic-wind after thunks ONCE,
		// against SourceWinding — the winding live HERE at the escape point, which may
		// be a deeper sub-context than the driver holding the exit frame. Unwinding
		// here too would double-fire (C2); reconciling from the driver's own winding
		// instead of SourceWinding would skip a deeper sub's after-thunks (C3).
		valid.Store(false)
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
	// Either way it clears the one-shot (so a later stale call gets the catchable
	// ErrExpiredEscape, not the driver's "no prompt found") and forwards the value(s)
	// unchanged. ParamCount 1 + variadic accepts 0/1/N values (AcceptsArity n >= 0).
	finalizerFn := func(finCC machine.CallContext) error {
		valid.Store(false)
		finMC, err := machine.RequireMachineContext(finCC, "call-with-exit")
		if err != nil {
			return err
		}
		var vals []values.Value
		current := finMC.Arg(0)
		for !values.IsEmptyList(current) {
			tuple, ok := current.(values.Tuple)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAList,
					"call-with-exit: improper finalizer argument list")
			}
			vals = append(vals, tuple.Car())
			current = tuple.Cdr()
		}
		finMC.SetValues(vals...)
		return nil
	}
	finalizer := machine.NewForeignClosure(closureEnv, 1, true, finalizerFn)

	// Reify the boundary: push a non-transparent prompt frame carrying tag (so an
	// (exit v) abort routes to it via FindPrompt) whose template clears the one-shot
	// and forwards the value(s), then inline-apply proc on the live chain. A
	// continuation captured inside proc now spans the exit frame and everything below
	// it — re-entry replays them instead of truncating.
	_, err = mc.RunBodyUnderExitFrame(procCls, tag, finalizer, exitClosure)
	return err
}
