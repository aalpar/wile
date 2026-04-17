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
	"github.com/aalpar/wile/values"
)

// callForeignCached executes a *ForeignClosure resolved from cachedBindings[instr.Arg].
// This is the fast path for peephole-optimized primitive calls.
//
// For non-tail calls (tail=false): the bytecode retains SaveContinuation for
// stack isolation. After the call, RestoreAndRelease recovers the caller's
// evals, env, template, and pc.
// For tail calls (tail=true): calls returnImmediate() to pop to the caller's caller.
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	callable := mc.template.cachedBindings[instr.Arg].Value()

	// Fast path: the binding still holds the *ForeignClosure that the
	// peephole optimizer saw at compile time.
	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		// Slow path: binding was reassigned at runtime (e.g., set! on a
		// primitive). For non-tail, SaveContinuation is in the bytecode
		// and provides stack isolation; ApplyCallable will consume the
		// saved continuation via RestoreContinuation or returnImmediate.
		// For tail, no continuation is needed (tail call semantics).
		return callForeignCachedReassigned(mc, callable)
	}

	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	l := fcls.paramCount

	err := checkArity(l, fcls.isVariadic, len(vs))
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	mc.counters.ClosuresApplied++
	mc.counters.ForeignCalls++
	mc.counters.RecordCall(fcls.name)

	// Acquire a fresh frame to prevent concurrent SRFI-18 threads from
	// racing on shared binding slots when calling the same ForeignClosure.
	env := acquireEnvFrame()
	fcls.env.InitApplyFrame(env)
	bnds := env.LocalEnvironment().Bindings()
	mc.envPooled = true
	mc.counters.EnvsCopied++
	mc.counters.BindingsCopied += uint64(len(bnds))
	mc.counters.KeysShared++

	bindArgs(bnds, vs, l, fcls.isVariadic, mc.buildRestArg)

	mc.env = env

	if fcls.validate != nil {
		err = fcls.validate(mc)
		if err != nil {
			return nil, applyCallableError(mc, err)
		}
	}

	savedTemplate := mc.template
	savedCont := mc.cont
	err = fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	// Immediate timeout check after foreign call returns successfully.
	if mc.timerHandler != nil {
		select {
		case <-mc.ctx.Done():
			return nil, &ErrTimerInterrupt{Handler: mc.timerHandler}
		default:
		}
	}

	// If the foreign function changed the template (e.g., PrimCallCC inline
	// mode calling Apply on a MachineClosure), let the VM continue from
	// wherever it pointed — the closure's RestoreContinuation will handle
	// the SaveContinuation frame.
	if mc.template != savedTemplate {
		return mc, nil
	}

	if tail {
		mc = mc.returnImmediate()
	} else if mc.cont == savedCont {
		// Non-tail: SaveContinuation is in the bytecode. Restore it
		// to recover the caller's evals, env, template, and pc.
		//
		// mc.cont is always non-nil here: the bytecode preceding
		// CallForeignCached always includes SaveContinuation, which
		// pushes a frame onto mc.cont. (Unlike applyForeign, which
		// can be called from sub-contexts where cont is nil.)
		//
		// Guard: if the foreign function already consumed the continuation
		// (e.g., PrimCallCC inline mode calling ApplyCallable with a
		// ForeignClosure, where applyForeign does its own RestoreAndRelease),
		// mc.cont has already advanced past savedCont. Restoring again would
		// double-restore from the wrong frame.
		mc.RestoreAndRelease(mc.cont)
	}
	return mc, nil
}

// callForeignCachedReassigned handles the case where a cached binding no longer
// holds a *ForeignClosure at runtime (e.g., the binding was reassigned via
// set!). The bytecode retains SaveContinuation for non-tail calls, providing
// stack isolation (Drain only takes args, not outer state) and return dispatch
// (ApplyCallable consumers the saved continuation automatically).
func callForeignCachedReassigned(mc *MachineContext, callable values.Value) (*MachineContext, error) {
	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	result, err := mc.ApplyCallable(callable, vs...)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}
	return result, nil
}
