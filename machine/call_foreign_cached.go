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
	"github.com/aalpar/wile/werr"
)

// callForeignCached executes a *ForeignClosure resolved from cachedBindings[instr.Arg].
// This is the fast path for peephole-optimized primitive calls, bypassing
// SaveContinuation/RestoreAndRelease entirely.
//
// For non-tail calls (tail=false): advances mc.pc after the call.
// For tail calls (tail=true): calls returnImmediate() to pop to the caller's caller.
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	callable := mc.template.cachedBindings[instr.Arg].Value()

	// Fast path: the binding still holds the *ForeignClosure that the
	// peephole optimizer saw at compile time.
	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		// Slow path: binding was reassigned at runtime (e.g., set! on a
		// primitive). Fall back to the generic ApplyCallable path.
		return callForeignCachedFallback(mc, callable, tail)
	}

	vs := mc.evals.PopAll()
	mc.counters.StackPopAlls++
	mc.counters.StackElementsCopied += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	l := fcls.paramCount

	// Arity check.
	if !fcls.isVariadic {
		if len(vs) != l {
			return nil, applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"expected %d arguments, got %d", l, len(vs)))
		}
	} else {
		if len(vs) < l-1 {
			return nil, applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"expected at least %d arguments, got %d", l-1, len(vs)))
		}
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	// Bind args into closure's own env (noCopyApply by construction).
	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	if !fcls.isVariadic {
		for i := range bnds[:l] {
			bnds[i].SetValue(vs[i])
		}
	} else {
		for i := range bnds[:l-1] {
			bnds[i].SetValue(vs[i])
		}
		bnds[l-1].SetValue(mc.buildRestArg(vs, l-1))
	}

	mc.env = env
	mc.envPooled = false

	savedTemplate := mc.template
	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	// If the foreign function changed the template (defensive — no current
	// ForeignClosure does this), let the VM continue from wherever it pointed.
	if mc.template != savedTemplate {
		return mc, nil
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		mc.pc++
	}
	return mc, nil
}

// callForeignCachedFallback handles the case where a cached binding no longer
// holds a *ForeignClosure at runtime (e.g., the binding was reassigned via
// set!). It reconstructs the original PullApply semantics: pop all args from
// the eval stack and dispatch through the generic ApplyCallable path.
func callForeignCachedFallback(mc *MachineContext, callable values.Value, tail bool) (*MachineContext, error) {
	vs := mc.evals.PopAll()
	mc.counters.StackPopAlls++
	mc.counters.StackElementsCopied += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	if !tail {
		// Non-tail: the peephole optimizer removed SaveContinuation, so we
		// must save one now to return to the instruction after this one.
		// off=1 means the saved PC will be mc.pc + 1 (the next instruction).
		err := mc.SaveContinuation(1)
		if err != nil {
			return nil, err
		}
	}

	result, err := mc.ApplyCallable(callable, vs...)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}
	return result, nil
}
