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
	"errors"
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

	procCls, err := helpers.RequireType[machine.Closure](proc, werr.ErrNotAProcedure, "call-with-exit")
	if err != nil {
		return err
	}

	tag := machine.NewPromptTag("exit")
	valid := &atomic.Bool{}
	valid.Store(true)
	capturingThreadID := mc.ThreadID()
	// Winding depth at entry. An escape must run the dynamic-wind after thunks
	// accumulated between here and the escape point. The unwind happens at the
	// escape point (innerMC, below) where those frames are visible — call-with-exit's
	// own sub does NOT see frames pushed in deeper sub-contexts (a call-with-values
	// producer, or an in-place exception handler), so the unwind cannot wait for the
	// catch to do it against this sub's winding.
	entryDepth := mc.WindingStack().Depth()

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
		// Run dynamic-wind after thunks from the escape point down to the
		// call-with-exit entry depth, here where the frames are visible (innerMC is
		// the context invoking the exit procedure — possibly a deeper sub-context than
		// call-with-exit's own sub). This is what call/cc's RestoreWithWindingFrom
		// does against the invoker's winding; call-with-exit must do the same so an
		// exception escaping a dynamic-wind via guard still runs its after thunk.
		if innerMC.WindingStack().Depth() > entryDepth {
			unwindErr := innerMC.UnwindTo(entryDepth)
			if unwindErr != nil {
				return unwindErr
			}
		}
		return &machine.ErrPromptAbort{
			Tag:    tag,
			Values: []values.Value{val},
		}
	}
	exitClosure := machine.NewForeignClosure(mc.EnvironmentFrame().MutableRuntime(), 1, false, exitFn)

	// Run proc in a sub-context with the exit closure as its argument.
	// The sub-context inherits the current winding stack so dynamic-wind
	// after thunks run when unwinding past any dynamic-wind frames on escape.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	_, err = sub.ApplyCallable(procCls, exitClosure)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var abortErr *machine.ErrPromptAbort
		if errors.As(err, &abortErr) && abortErr.Tag == tag {
			// Escape matched our tag. The exit procedure already ran the dynamic-wind
			// after thunks at the escape point (see exitFn) — call-with-exit's own sub
			// cannot see frames pushed in deeper sub-contexts, so unwinding here would
			// miss them. Just invalidate the procedure and deliver the value.
			valid.Store(false)
			mc.SetValue(abortErr.Values[0])
			return nil
		}
		return err
	}

	// Normal return: invalidate the exit procedure and forward proc's value(s).
	valid.Store(false)
	mc.SetValues(sub.GetValues()...)
	return nil
}
