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

// callForeignCached executes a non-variadic *ForeignClosure resolved from
// cachedBindings. The Arg field encodes both the binding index and paramCount
// (via EncodeForeignCallArg); the paramCount is trusted at compile time so
// arity checking is replaced by a guard against set!-reassignment.
//
// For non-tail calls (tail=false): the bytecode retains SaveContinuation for
// stack isolation. After the call, RestoreAndRelease recovers the caller's
// evals, env, template, and pc.
// For tail calls (tail=true): calls returnImmediate() to pop to the caller's caller.
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
	callable := mc.template.cachedBindings[bindingIdx].Value()

	// Fast path: the binding still holds the *ForeignClosure that the
	// peephole optimizer saw at compile time.
	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		// Slow path: binding was reassigned to a non-ForeignClosure value
		// at runtime (e.g., set! to a Scheme closure). For non-tail,
		// SaveContinuation is in the bytecode and provides stack isolation;
		// ApplyCallable will consume the saved continuation via
		// RestoreContinuation or returnImmediate.
		// For tail, no continuation is needed (tail call semantics).
		return callForeignCachedReassigned(mc, callable)
	}

	// Guard: the binding still holds a ForeignClosure but with a different
	// paramCount than compiled (set! replaced it with another foreign closure).
	if fcls.paramCount != paramCount {
		return callForeignCachedMismatch(mc, fcls, tail)
	}

	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	// Arg count mismatch: the peephole guarantees argCount == paramCount
	// for well-formed code, but hand-constructed bytecode or wrong-arity
	// calls (e.g., (get-environment-variable 1 2)) can violate this.
	if len(vs) != paramCount {
		return nil, applyCallableError(mc, checkArity(paramCount, false, len(vs)))
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	// Bind args into closure's own env (noCopyApply by construction).
	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	switch paramCount {
	case 0:
		// no args
	case 1:
		bnds[0].SetValue(vs[0])
	case 2:
		bnds[0].SetValue(vs[0])
		bnds[1].SetValue(vs[1])
	case 3:
		bnds[0].SetValue(vs[0])
		bnds[1].SetValue(vs[1])
		bnds[2].SetValue(vs[2])
	default:
		for i := range bnds[:paramCount] {
			bnds[i].SetValue(vs[i])
		}
	}

	mc.env = env
	mc.envPooled = false

	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		// Non-tail: SaveContinuation is in the bytecode. Restore it
		// to recover the caller's evals, env, template, and pc.
		mc.RestoreAndRelease(mc.cont)
	}
	return mc, nil
}

// callForeignCachedVar executes a variadic *ForeignClosure resolved from
// cachedBindings. Same structure as callForeignCached but uses bindArgs for
// variadic argument binding (variadic closures don't benefit from unrolling).
func callForeignCachedVar(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
	callable := mc.template.cachedBindings[bindingIdx].Value()

	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		return callForeignCachedReassigned(mc, callable)
	}

	if fcls.paramCount != paramCount {
		return callForeignCachedMismatch(mc, fcls, tail)
	}

	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	// Variadic arity check: need at least paramCount-1 args.
	if len(vs) < paramCount-1 {
		return nil, applyCallableError(mc, checkArity(paramCount, true, len(vs)))
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	bindArgs(bnds, vs, paramCount, true, mc.buildRestArg)

	mc.env = env
	mc.envPooled = false

	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		mc.RestoreAndRelease(mc.cont)
	}
	return mc, nil
}

// callForeignCachedMismatch handles the case where a cached binding still
// holds a *ForeignClosure but with a different paramCount than was compiled
// (e.g., set! replaced it with a different foreign closure). Falls back to
// full arity checking and generic bindArgs.
func callForeignCachedMismatch(mc *MachineContext, fcls *ForeignClosure, tail bool) (*MachineContext, error) {
	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	err := checkArity(fcls.paramCount, fcls.isVariadic, len(vs))
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	bindArgs(bnds, vs, fcls.paramCount, fcls.isVariadic, mc.buildRestArg)

	mc.env = env
	mc.envPooled = false

	err = fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
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
