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
	"errors"
	"fmt"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func (p *MachineContext) Apply(mcls *MachineClosure, vs ...values.Value) (*MachineContext, error) {
	tpl := mcls.Template()
	l := tpl.ParameterCount()

	// Check arity before copying environment (fast-fail path).
	// Wrong-arity calls are common enough (dynamic typing, variadic dispatch)
	// that avoiding the copy overhead is worthwhile.
	err := checkArity(l, tpl.IsVariadic(), len(vs))
	if err != nil {
		return nil, err
	}

	p.counters.ClosuresApplied++
	name := tpl.Name()
	if name != "" {
		p.counters.RecordCall(name)
	}

	// Acquire a fresh frame from the pool. This prevents two correctness
	// issues: (1) recursive calls sharing bindings when SaveContinuation
	// captures mc.env, and (2) concurrent SRFI-18 threads sharing binding
	// slots when calling the same closure.
	//
	// Exception: closures whose env has no parent (e.g., thunks from
	// compile, top-level wrappers) cannot be copied via InitApplyFrame.
	// These are safe to reuse directly — they have no local parameter
	// bindings for concurrent callers to race on.
	//
	// Invariant: Parent() == nil implies parameterCount == 0. All closures
	// with parameters are constructed with NewEnvironmentFrameWithParent,
	// which guarantees a non-nil parent.
	var env *environment.EnvironmentFrame
	var bnds []environment.Binding

	if mcls.env.Parent() == nil {
		env = mcls.env
		lenv := env.LocalEnvironment()
		if lenv != nil {
			bnds = lenv.Bindings()
		}
		p.envPooled = false
	} else {
		env = acquireEnvFrame()
		mcls.env.InitApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		p.envPooled = true
		p.counters.EnvsCopied++
		p.counters.BindingsCopied += uint64(len(bnds))
		p.counters.KeysShared++
	}

	bindArgs(bnds, vs, l, tpl.IsVariadic(), nil)

	p.template = tpl
	p.env = env
	p.pc = 0
	return p, nil
}

// applyForeign calls a foreign closure directly, bypassing the bytecode VM.
// This is the fast path for Go-implemented primitives: arity check, bind args,
// call the function, restore continuation. No template, no opcodes, no VM loop.
func (p *MachineContext) applyForeign(fcls *ForeignClosure, vs ...values.Value) (*MachineContext, error) {
	l := fcls.paramCount

	err := checkArity(l, fcls.isVariadic, len(vs))
	if err != nil {
		return nil, err
	}

	p.counters.ClosuresApplied++

	// Acquire a fresh frame to prevent concurrent SRFI-18 threads from
	// racing on shared binding slots when calling the same ForeignClosure.
	env := acquireEnvFrame()
	fcls.env.InitApplyFrame(env)
	bnds := env.LocalEnvironment().Bindings()
	p.envPooled = true
	p.counters.EnvsCopied++
	p.counters.BindingsCopied += uint64(len(bnds))
	p.counters.KeysShared++

	bindArgs(bnds, vs, l, fcls.isVariadic, p.buildRestArg)

	p.env = env

	p.counters.ForeignCalls++
	p.counters.RecordCall(fcls.name)

	if fcls.validate != nil {
		err = fcls.validate(p)
		if err != nil {
			return nil, applyCallableError(p, err)
		}
	}

	// Save the template and continuation pointers before calling the foreign
	// function. Some foreign functions (e.g., PrimCallCC inline mode) call
	// ApplyCallable on the MachineContext, which may change the template (via
	// Apply for MachineClosure) or consume the continuation (via a nested
	// applyForeign for ForeignClosure). We check both after the call to avoid
	// double-restoring from an already-consumed SaveContinuation frame.
	savedTemplate := p.template
	savedCont := p.cont

	err = fcls.fn(p)
	if err != nil {
		// Propagate prompt aborts, exception escapes, and timer interrupts as-is.
		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			return nil, err
		}
		var excErr *ErrExceptionEscape
		if errors.As(err, &excErr) {
			return nil, err
		}
		var timerErr *ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			return nil, err
		}
		return nil, goErrorToSchemeException(p, err)
	}

	// Immediate timeout check after foreign call returns successfully.
	// Closes the latency gap: a foreign function that blocks for seconds
	// triggers the handler immediately, not after 1024 more bytecode ops.
	if p.timerHandler != nil {
		select {
		case <-p.ctx.Done():
			return nil, &ErrTimerInterrupt{Handler: p.timerHandler}
		default:
		}
	}

	// If the foreign function changed the template (e.g., via Apply/ApplyCallable),
	// the VM state is configured for continued execution — do not restore continuation.
	if p.template != savedTemplate {
		return p, nil
	}

	// Restore continuation, but only if the foreign function didn't already
	// consume it (e.g., PrimCallCC inline mode calling ApplyCallable with a
	// ForeignClosure, where the nested applyForeign does its own restore).
	if p.cont == savedCont {
		if p.cont != nil {
			p.RestoreAndRelease(p.cont)
		} else {
			p.template = immediateReturnTemplate
			p.pc = 0
		}
	}
	return p, nil
}

// buildRestArg constructs a variadic rest-arg list in p.restArgBuf, returning
// it as a Tuple. The buffer grows with doubling strategy and is reused across
// calls, amortizing allocations to zero after warmup.
//
// SAFETY: Safe for ForeignClosure calls because the env frame is used
// synchronously during the Go function call and then released to the pool.
// The buffer is per-MachineContext, so concurrent SRFI-18 threads each
// have their own buffer.
func (p *MachineContext) buildRestArg(vs []values.Value, start int) values.Tuple {
	n := len(vs) - start
	if n == 0 {
		return values.EmptyList
	}
	if cap(p.restArgBuf) < n {
		p.restArgBuf = make(values.PairBlock, n*2)
	}
	return p.restArgBuf[:n].LinkWith(vs[start:])
}

// ApplyCaseLambda applies a case-lambda closure by finding the matching clause.
func (p *MachineContext) ApplyCaseLambda(clcls *CaseLambdaClosure, vs ...values.Value) (*MachineContext, error) {
	mcls, ok := clcls.FindMatchingClause(len(vs))
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "no matching clause in case-lambda for %d arguments", len(vs))
	}
	return p.Apply(mcls, vs...)
}

// ApplyCallable dispatches a procedure call to the appropriate handler based
// on the callee's concrete type. This is the unified entry point for all
// Scheme procedure application, symmetric with OperationApply.
//
// Supported callable types:
//   - *MachineClosure: standard Scheme lambda
//   - *ForeignClosure: Go foreign function (direct call, no bytecode)
//   - *CaseLambdaClosure: R7RS case-lambda (§4.2.9)
//   - *Parameter: R7RS parameter object (§4.2.6)
//   - *ComposableContinuation: delimited continuation
//
// Precondition: p.ctx must be set (always true for contexts created via
// NewMachineContext or NewSubContext).
func (p *MachineContext) ApplyCallable(callable values.Value, args ...values.Value) (*MachineContext, error) {
	if callable == nil {
		return p, werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"application: cannot apply nil value")
	}
	switch cls := callable.(type) {
	case *MachineClosure:
		return p.Apply(cls, args...)
	case *ForeignClosure:
		return p.applyForeign(cls, args...)
	case *CaseLambdaClosure:
		return p.ApplyCaseLambda(cls, args...)
	case *Parameter:
		return p.applyParameter(cls, args)
	case *ComposableContinuation:
		return p.applyComposableContinuation(cls, args)
	case *CapturedContinuation:
		return p.applyCapturedContinuation(cls, args)
	default:
		return p, werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"application: expected a procedure, got %s", callable.SchemeString())
	}
}

// drainAndApply drains all arguments from the eval stack, updates counters,
// and applies the callable. This is the common pattern shared by OpApply,
// OpCallLocal, and OpCallCachedBinding.
func (p *MachineContext) drainAndApply(callable values.Value) (*MachineContext, error) {
	vs := p.evals.Drain()
	p.counters.StackDrains++
	p.counters.StackElementsDrained += uint64(len(vs))
	p.counters.RecordStackDepth(len(vs))
	result, err := p.ApplyCallable(callable, vs...)
	if err != nil {
		return nil, applyCallableError(p, err)
	}
	return result, nil
}

// pullDrainAndApply splits the eval stack into callable (position 0) and args
// (positions 1..n) in O(1), then applies. Used by OpPullApply.
//
// Unlike drainAndApply (which takes the callable from the value register),
// this method extracts the callable from the bottom of the eval stack —
// matching the SECD calling convention where the procedure is evaluated
// first and pushed first.
func (p *MachineContext) pullDrainAndApply() (*MachineContext, error) {
	callable, vs := p.evals.PullDrain()
	p.counters.StackDrains++
	// len(vs) counts args only, not the callable — matches drainAndApply
	// where the callable comes from the value register, not the stack.
	p.counters.StackElementsDrained += uint64(len(vs))
	p.counters.RecordStackDepth(len(vs))
	p.SetValue(callable)
	result, err := p.ApplyCallable(callable, vs...)
	if err != nil {
		return nil, applyCallableError(p, err)
	}
	return result, nil
}

// returnImmediate returns control to the caller after a non-bytecode callable
// (e.g., Parameter get/set) has placed its result in the value register.
// If a continuation is saved, it restores it (like RestoreContinuation for
// closures). Otherwise (sub-context, cont == nil) it sets immediateReturnTemplate
// so that Run() returns nil immediately.
func (p *MachineContext) returnImmediate() *MachineContext {
	if p.cont != nil {
		p.RestoreAndRelease(p.cont)
	} else {
		p.template = immediateReturnTemplate
		p.pc = 0
	}
	return p
}

// applyParameter handles calling a parameter object.
// With 0 args: returns the current value (checking continuation marks first).
// With 1 arg: sets the base value (after applying converter if present).
func (p *MachineContext) applyParameter(param *Parameter, args []values.Value) (*MachineContext, error) {
	switch len(args) {
	case 0:
		// Check continuation marks for a parameterize binding.
		// Walk current frame marks, then the continuation chain.
		val := p.findParameterInMarks(param)
		if val != nil {
			p.SetValue(val)
		} else {
			p.SetValue(param.Value())
		}
		return p.returnImmediate(), nil

	case 1:
		newVal := args[0]

		if param.HasConverter() {
			converter := param.Converter()
			sub := p.NewSubContext()
			defer ReleaseSubContext(sub)
			_, err := sub.ApplyCallable(converter, newVal)
			if err != nil {
				wrapErr := p.WrapError(err, "parameter: failed to apply converter")
				return p, wrapErr
			}
			err = sub.Run()
			if err != nil {
				wrapErr := p.WrapError(err, "parameter: converter error")
				return p, wrapErr
			}
			newVal = sub.GetValue()
		}

		param.SetValue(newVal)
		p.SetValue(values.Void)
		return p.returnImmediate(), nil

	default:
		err := p.Error(fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)))
		return p, err
	}
}

// findParameterInMarks walks the continuation mark chain looking for a mark
// keyed by the given parameter (using pointer identity).
// Returns the mark value, or nil if no mark is found.
//
// This is the lookup mechanism for marks-based parameterize: each
// (parameterize ((p val)) ...) sets a continuation mark with key=p, val=converted.
// Parameter reads walk the chain to find the nearest binding.
//
// The walk spans sub-context boundaries via parentMC, mirroring how dynamic-wind
// extents are inherited by NewSubContext. Without this, parameter bindings
// from an outer parameterize would be invisible inside sub-contexts created by
// call-with-continuation-prompt, apply, call-with-values, etc.
func (p *MachineContext) findParameterInMarks(param *Parameter) values.Value {
	mc := p
	for mc != nil {
		// Check current frame marks.
		for _, e := range mc.marks {
			if eqIdentity(e.key, param) {
				return e.val
			}
		}
		// Walk continuation chain.
		for c := mc.cont; c != nil; c = c.parent {
			for _, e := range c.marks {
				if eqIdentity(e.key, param) {
					return e.val
				}
			}
		}
		// Stop if this context has isolated marks (e.g., applyCapturedContinuation
		// sub-context running a different continuation chain).
		if mc.isolatedMarks {
			break
		}
		mc = mc.parentMC
	}
	return nil
}

// ResolveParameterValue returns the effective value of a parameter, checking
// continuation marks (from parameterize) before falling back to the base value.
// This is the Go-side equivalent of calling the parameter with 0 args.
func (p *MachineContext) ResolveParameterValue(param *Parameter) values.Value {
	val := p.findParameterInMarks(param)
	if val != nil {
		return val
	}
	return param.Value()
}

// applyComposableContinuation applies a composable continuation by splicing
// its captured frames onto the current continuation chain. On first invocation,
// the segment is used directly (marked shared for frame preservation). On
// re-invocation, the segment is deep-copied for independence.
//
// See: Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
func (p *MachineContext) applyComposableContinuation(cc *ComposableContinuation, args []values.Value) (*MachineContext, error) {
	if len(args) != 1 {
		err := p.Error(fmt.Sprintf("composable continuation: expected 1 argument, got %d", len(args)))
		return p, err
	}

	// Reject cross-thread composable continuation invocation
	if p.threadID != cc.threadID {
		return p, werr.WrapForeignErrorf(werr.ErrCrossThreadContinuation,
			"composable continuation: captured in thread %d, invoked from thread %d",
			cc.threadID, p.threadID)
	}

	// Reject barrier crossing: pointer inequality means different barrier contexts.
	// nil != non-nil: captured outside, invoked inside (or vice versa).
	// ptr-A != ptr-B: captured inside barrier A, invoked inside barrier B.
	if cc.BarrierValid() != p.barrierValid {
		return p, werr.WrapForeignErrorf(werr.ErrContinuationBarrier,
			"composable continuation: cannot cross continuation barrier")
	}

	// Save the argument value before Restore releases the old eval stack.
	// When the caller used Drain (zero-copy), args shares the stack's
	// backing array. Restore recycles that stack to the pool, clearing
	// the backing array and invalidating args.
	val := args[0]

	// Acquire the segment: first invocation avoids DeepCopy by marking
	// the segment shared; re-invocations deep-copy from preserved frames.
	segment := cc.AcquireSegment()

	// Handle dynamic-wind: unwind current extents not in captured stack,
	// rewind captured extents not in current stack.
	err := p.RestoreWithWindingFrom(nil, p.windingStack, cc.WindingStack())
	if err != nil {
		return p, err
	}

	if segment == nil {
		// Empty composable continuation: captured at a tail-call site with no
		// saved frames above it (e.g., call/cc inside a sub-context where
		// the call was in tail position). Applying it just returns the value.
		p.SetValue(val)
		return p.returnImmediate(), nil
	}

	// Graft the segment's bottom frame onto the current continuation chain
	GraftContinuation(segment, p.cont)

	// Restore from the top of the segment (resume captured computation)
	p.Restore(segment)
	p.SetValue(val)
	return p, nil
}
