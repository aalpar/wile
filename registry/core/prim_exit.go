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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
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
func PrimCallWithExit(mc *machine.MachineContext) error {
	proc := mc.Arg(0)

	procCls, err := helpers.RequireType[machine.Closure](proc, values.ErrNotAProcedure, "call-with-exit")
	if err != nil {
		return err
	}

	tag := machine.NewExitTag()
	valid := &atomic.Bool{}
	valid.Store(true)
	capturingThreadID := mc.ThreadID()

	// Build the exit closure. It is valid only during the dynamic extent of proc.
	// Checking valid before thread is intentional: a cross-thread call to an expired
	// exit procedure gets the "outside dynamic extent" error, which is more informative.
	exitFn := func(innerMC *machine.MachineContext) error {
		if !valid.Load() {
			return values.WrapForeignErrorf(values.ErrExpiredEscape,
				"call-with-exit: exit procedure called outside dynamic extent")
		}
		if innerMC.ThreadID() != capturingThreadID {
			return values.WrapForeignErrorf(values.ErrCrossThreadContinuation,
				"call-with-exit: exit procedure called from different thread")
		}
		val := innerMC.Arg(0)
		return machine.NewErrExitEscape(tag, val)
	}
	exitClosure := machine.NewForeignClosure(mc.EnvironmentFrame().TopLevel(), 1, false, exitFn)

	// Run proc in a sub-context with the exit closure as its argument.
	// The sub-context inherits the current winding stack so dynamic-wind
	// after thunks run when unwinding past any dynamic-wind frames on escape.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetWindingStack(mc.WindingStack())
	_, err = sub.ApplyCallable(procCls, exitClosure)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		var exitErr *machine.ErrExitEscape
		if errors.As(err, &exitErr) && exitErr.Tag() == tag {
			// Escape matched our tag. Invalidate the exit procedure, then unwind
			// any dynamic-wind frames accumulated above our entry point.
			valid.Store(false)
			if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
				unwindErr := sub.UnwindTo(mc.WindingStack().Depth())
				if unwindErr != nil {
					return unwindErr
				}
			}
			mc.SetValue(exitErr.Value)
			return nil
		}
		return err
	}

	// Normal return: invalidate the exit procedure and forward proc's value(s).
	valid.Store(false)
	mc.SetValues(sub.GetValues()...)
	return nil
}
