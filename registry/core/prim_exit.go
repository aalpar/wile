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
	"github.com/aalpar/wile/werr"
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
	mc := cc.(*machine.MachineContext)
	proc := mc.Arg(0)

	procCls, err := helpers.RequireType[machine.Closure](proc, werr.ErrNotAProcedure, "a procedure", "call-with-exit")
	if err != nil {
		return err
	}

	tag := machine.NewPromptTag("exit")
	valid := &atomic.Bool{}
	valid.Store(true)
	capturingThreadID := mc.ThreadID()

	// Build the exit closure. It is valid only during the dynamic extent of proc.
	// Checking valid before thread is intentional: a cross-thread call to an expired
	// exit procedure gets the "outside dynamic extent" error, which is more informative.
	exitFn := func(innerCC machine.CallContext) error {
		innerMC := innerCC.(*machine.MachineContext)
		if !valid.Load() {
			return werr.WrapForeignErrorf(werr.ErrExpiredEscape,
				"call-with-exit: exit procedure called outside dynamic extent")
		}
		if innerMC.ThreadID() != capturingThreadID {
			return werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
				"call-with-exit: exit procedure called from different thread")
		}
		val := innerMC.Arg(0)
		return &machine.ErrPromptAbort{
			Tag:    tag,
			Values: []values.Value{val},
		}
	}
	exitClosure := machine.NewForeignClosure(mc.EnvironmentFrame().TopLevel(), 1, false, exitFn)

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
			// Escape matched our tag. Invalidate the exit procedure, then unwind
			// any dynamic-wind frames accumulated above our entry point.
			valid.Store(false)
			if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
				unwindErr := sub.UnwindTo(mc.WindingStack().Depth())
				if unwindErr != nil {
					return unwindErr
				}
			}
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
