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
	"context"
	"errors"
	"fmt"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// errHalt is the internal VM sentinel returned by OperationRestoreContinuation
// when mc.cont == nil (i.e., no more frames to pop — execution is complete).
// Run() catches this and returns nil, so callers never see it.
var errHalt = werr.NewStaticError("machine halt: no more operations to run")

// contextCheckMask gates how often the VM loop checks ctx.Done().
// Amortized batch checking: a non-blocking select is cheap (~15ns) but
// not free; checking every 1024 ops eliminates ~99.9% of them while
// keeping worst-case cancellation latency under 1ms at typical throughput.
// Power of 2 so the check is a single AND instruction.
// See BIBLIOGRAPHY.md "Amortized Batch Checking".
const contextCheckMask = 1023

var ErrMachineDoNotAdvancePC = werr.NewStaticError("machine do not advance PC: operation did not advance program counter")

var ErrInvalidLiteralIndex = werr.NewStaticError("invalid literal index")
var ErrInvalidGlobalIndex = werr.NewStaticError("literal is not a global index")
var ErrBindingNotFound = werr.NewStaticError("binding not found")

// immediateReturnTemplate is an empty NativeTemplate used for callables that
// complete their work during Apply (e.g., Parameter get/set). Setting this as
// the template causes Run() to return nil immediately (0 operations).
//
// INVARIANT: immediateReturnTemplate MUST remain empty and MUST NOT be mutated.
// Do not append, remove, or modify operations on this template. Its emptiness
// is relied upon so that Run() for immediate-return callables performs zero
// operations and returns immediately. If additional behavior is needed, create
// a new NativeTemplate instance instead of changing this one.
var immediateReturnTemplate = &NativeTemplate{}

// MachineContext represents the execution context of a virtual machine.
// It holds the current environment, values, evaluation stack, continuation, and program counter.
// It is created from a MachineContinuation and can be modified during execution.
type MachineContext struct {
	ctx context.Context
	vmState
	cont             *MachineContinuation // current continuation
	expanderCtx      *ExpanderContext     // set during macro transformer execution for syntax-local-* access
	exceptionHandler *ExceptionHandler    // current exception handler chain for R7RS exceptions
	debugger         *Debugger            // optional debugger for breakpoints and stepping
	parentMC         *MachineContext      // parent context for sub-contexts, enables call/cc escape tracking
	escapeCont       *MachineContinuation // escape continuation for sub-contexts: where to continue after sub-context completes
	barrierValid     *BarrierToken        // non-nil when inside a with-continuation-barrier; pointer identity identifies the barrier
	counters         VMCounters           // performance counters (plain uint64, single-goroutine)
	// thread is the SRFI-18 thread object (nil = primordial thread).
	// This is the Scheme-visible half of the thread identity split.
	// The numeric half (threadID) lives in vmState and propagates into
	// continuations. See the comment on vmState.threadID for the full
	// design and invariant.
	thread       *values.Thread
	syntaxCase   *syntaxCaseState // per-context syntax-case expansion state; nil when not in syntax-case
	maxCallDepth uint64           // 0 = unlimited (default), otherwise max continuation depth
	restArgBuf   values.PairBlock // reusable buffer for variadic rest-arg list construction (noCopyApply path only)
}

// NewMachineContext creates a new machine context with the given context and continuation.
// The context enables cancellation/timeout support in the VM loop.
// For callers that don't need cancellation, pass context.Background().
func NewMachineContext(ctx context.Context, cont *MachineContinuation) *MachineContext {
	var evals *Stack
	if cont.evals != nil {
		evals = cont.evals // no copy needed: continuation is consumed once at context creation
	} else {
		// Inline evals: reconstruct a stack from the continuation's inline slots.
		evals = NewStack()
		for i := uint8(0); i < cont.inlineEvalsLen; i++ {
			evals.Push(cont.inlineEvals[i])
		}
	}
	q := &MachineContext{
		ctx: ctx,
		vmState: vmState{
			env:         cont.env,         // cannot copy environment here, it will be copied when pushed onto the stack
			template:    cont.template,    // not needed to copy, templates are immutable
			singleValue: cont.singleValue, // must not copy the values, they are passed between contexts
			multiValues: cont.multiValues,
			evals:       evals,
			pc:          cont.pc,
		},
		cont: cont.parent,
	}
	return q
}

func NewMachineContextFromMachineClosure(ctx context.Context, cls *MachineClosure) *MachineContext {
	return NewMachineContext(ctx, NewMachineContinuation(nil, cls.template, cls.env))
}

func (p *MachineContext) Parent() *MachineContinuation {
	return p.cont
}

// ParentMC returns the parent machine context (for sub-contexts), or nil for top-level contexts.
func (p *MachineContext) ParentMC() *MachineContext {
	return p.parentMC
}

// EscapeCont returns the escape continuation for this context.
// This is set by foreign functions (like dynamic-wind) that need call/cc
// inside their sub-contexts to know where to continue after completion.
func (p *MachineContext) EscapeCont() *MachineContinuation {
	return p.escapeCont
}

// SetEscapeCont sets the escape continuation for this context.
func (p *MachineContext) SetEscapeCont(cont *MachineContinuation) {
	p.escapeCont = cont
}

// BarrierValid returns the barrier validity flag for the current context.
// Non-nil means execution is inside a with-continuation-barrier; the pointer
// identity distinguishes which barrier. Nil means no active barrier.
//
// Escape closures and composable continuations use this to detect barrier
// crossings: if the capture-time pointer differs from the invocation-time
// pointer, the continuation would cross a barrier boundary.
func (p *MachineContext) BarrierValid() *BarrierToken {
	return p.barrierValid
}

// SetBarrierValid sets the barrier identity token on this context.
// Called by PrimCallWithContinuationBarrier to mark the sub-context as inside a barrier.
func (p *MachineContext) SetBarrierValid(v *BarrierToken) {
	p.barrierValid = v
}

func (p *MachineContext) Template() *NativeTemplate {
	return p.template
}

func (p *MachineContext) PC() int {
	return p.pc
}

// SetPC sets the program counter. Used by PrimCallCC for inline lambda execution.
func (p *MachineContext) SetPC(v int) {
	p.pc = v
}

// SetValues sets the value register. For a single value this uses the
// zero-allocation fast path (singleValue); for multiple values it falls
// back to the multiValues slice.
func (p *MachineContext) SetValues(vs ...values.Value) {
	if len(vs) == 1 {
		p.singleValue = vs[0]
		p.multiValues = nil
		return
	}
	p.multiValues = vs
	p.singleValue = nil
}

// SetValue stores a single value in the value register without allocating.
// This is the hot path: every LoadLocal, LoadGlobal, LoadLiteral, Pull, Pop,
// MakeClosure, etc. goes through here.
func (p *MachineContext) SetValue(v values.Value) {
	p.singleValue = v
	p.multiValues = nil
}

// GetValue returns the first (or only) value from the value register.
// Returns Void if the register is empty.
func (p *MachineContext) GetValue() values.Value {
	if p.multiValues != nil {
		if len(p.multiValues) == 0 {
			return values.Void
		}
		return p.multiValues[0]
	}
	if p.singleValue == nil {
		return values.Void
	}
	return p.singleValue
}

// GetValues returns all values from the value register as a MultipleValues
// slice. For the single-value case this allocates a one-element slice; callers
// on the hot path should use GetValue instead.
func (p *MachineContext) GetValues() MultipleValues {
	if p.multiValues != nil {
		return p.multiValues
	}
	if p.singleValue == nil {
		return nil
	}
	return MultipleValues{p.singleValue}
}

func (p *MachineContext) EnvironmentFrame() *environment.EnvironmentFrame {
	return p.env
}

func (p *MachineContext) Arg(index int) values.Value {
	return p.env.GetLocalBindingByIndex(index).Value()
}

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

	var env *environment.EnvironmentFrame
	var bnds []environment.Binding

	if tpl.NoCopyApply() {
		// No-copy path: the template contains no SaveContinuation and no
		// MakeClosure, so mc.env is never captured. Safe to mutate the
		// closure's own bindings in place, eliminating both the
		// EnvironmentFrame and []Binding allocations.
		env = mcls.env
		bnds = env.LocalEnvironment().Bindings()
		// envPooled: closure's own env, not from pool.
		p.envPooled = false
		p.counters.NoCopyApplies++
		p.counters.NoCopyBindingsSaved += uint64(len(bnds))
	} else {
		// Copy path: acquire a frame from the pool and populate it.
		// Critical for recursive functions with SaveContinuation: without
		// copying, all invocations share the same bindings, causing
		// parameter corruption when evaluating arguments like
		// (+ (f (- n 1)) (f (- n 2))).
		env = acquireEnvFrame()
		mcls.env.InitApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		// envPooled: frame from envFramePool; RestoreAndRelease will recycle it.
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
	p.counters.NoCopyApplies++

	// Always reuse the closure's own env (noCopyApply by construction).
	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	p.counters.NoCopyBindingsSaved += uint64(len(bnds))

	bindArgs(bnds, vs, l, fcls.isVariadic, p.buildRestArg)

	p.env = env
	// envPooled: closure's own env, not from pool.
	p.envPooled = false

	p.counters.ForeignCalls++

	// Save the template pointer before calling the foreign function.
	// Some foreign functions (e.g., PrimCallCC inline mode) call Apply()
	// on the MachineContext, changing the template/env/pc to set up the VM
	// for continued execution of a different closure. If the template changes,
	// we must NOT do returnImmediate() — the foreign function has already
	// configured the VM state.
	savedTemplate := p.template

	err = fcls.fn(p)
	if err != nil {
		// Propagate prompt aborts and exception escapes as-is.
		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			return nil, err
		}
		var excErr *ErrExceptionEscape
		if errors.As(err, &excErr) {
			return nil, err
		}
		return nil, goErrorToSchemeException(p, err)
	}

	// If the foreign function changed the template (e.g., via Apply/ApplyCallable),
	// the VM state is configured for continued execution — do not restore continuation.
	if p.template != savedTemplate {
		return p, nil
	}

	// Restore continuation (same as returnImmediate).
	if p.cont != nil {
		p.RestoreAndRelease(p.cont)
	} else {
		p.template = immediateReturnTemplate
		p.pc = 0
	}
	return p, nil
}

// buildRestArg constructs a variadic rest-arg list in p.restArgBuf, returning
// it as a Tuple. The buffer grows with doubling strategy and is reused across
// calls, amortizing allocations to zero after warmup.
//
// SAFETY: Only safe on the noCopyApply path, where the environment is not
// captured (no SaveContinuation/MakeClosure). The buffer is overwritten on
// the next variadic call, so the returned Tuple must not be retained.
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
	default:
		return p, werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"application: expected a procedure, got %s", callable.SchemeString())
	}
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
// With 0 args: returns the current value.
// With 1 arg: sets the value (after applying converter if present).
func (p *MachineContext) applyParameter(param *Parameter, args []values.Value) (*MachineContext, error) {
	switch len(args) {
	case 0:
		p.SetValue(param.Value())
		return p.returnImmediate(), nil

	case 1:
		newVal := args[0]

		if param.HasConverter() {
			converter := param.Converter()
			sub := p.NewSubContext()
			defer ReleaseSubContext(sub)
			sub.SetWindingStack(p.WindingStack())
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

// SetContext sets the context for this machine context.
// This should be called before Run() to enable context cancellation/timeout.
func (p *MachineContext) SetContext(ctx context.Context) {
	p.ctx = ctx
}

// Context returns the context for this machine context.
func (p *MachineContext) Context() context.Context {
	return p.ctx
}

// Run executes the VM loop starting from the current pc.
// The pc is NOT reset here - callers are responsible for ensuring the correct initial pc:
//   - NewMachineContext copies pc from the continuation (typically 0 for fresh execution)
//   - Apply sets pc = 0 for fresh closure invocation
//   - Restore sets pc from the saved continuation for resumption
//
// This design allows continuation resumption (e.g., raise-continuable) to work correctly
// by preserving the pc set by Restore rather than unconditionally resetting to 0.
//
// Dispatch: Run always uses switch-dispatch over integer opcodes.
//
// Context cancellation: The loop checks p.ctx.Done() every 1024 ops, allowing
// preemption via context.WithTimeout or context.WithCancel. This enables:
//   - Test timeouts that actually stop execution
//   - REPL interrupt support (Ctrl+C)
//   - Resource management for long-running computations
//
// Run executes the VM loop using switch-dispatch with integer opcodes.
// Hot-path operations (Wave 1-3) are inlined as switch cases; complex
// operations (closures, macros, FFI) are dispatched via OpComplex to
// the template's sideTable.
//
// Set the context via SetContext() before calling Run().
func (p *MachineContext) Run() error {
	mc := p
	for mc.pc < len(mc.template.code) {
		if mc.counters.OpsExecuted&contextCheckMask == 0 {
			select {
			case <-mc.ctx.Done():
				return mc.ctx.Err()
			default:
			}
		}

		if mc.debugger != nil {
			bp := mc.debugger.CheckBreakpoint(mc)
			if bp != nil {
				mc.debugger.TriggerBreak(mc, bp)
			} else if mc.debugger.ShouldStep(mc) {
				mc.debugger.TriggerBreak(mc, nil)
			}
		}

		instr := mc.template.code[mc.pc]
		mc.counters.OpsExecuted++

		switch instr.Op {
		// --- Wave 1: zero-operand operations ---

		case OpPush:
			if mc.multiValues != nil {
				mc.evals.PushAll(mc.multiValues)
			} else if mc.singleValue != nil {
				mc.evals.Push(mc.singleValue)
			}
			mc.pc++

		case OpPop:
			mc.SetValue(mc.evals.Pop())
			mc.pc++

		case OpPull:
			mc.SetValue(mc.evals.Pull())
			mc.pc++

		case OpLoadVoid:
			mc.SetValue(values.Void)
			mc.pc++

		case OpDrop:
			mc.evals.Pop()
			mc.pc++

		case OpPopEnv:
			parent := mc.env.Parent()
			if parent == nil {
				return werr.WrapForeignErrorf(werr.ErrNilParentEnvironment,
					"PopEnv: cannot pop top-level environment")
			}
			mc.env = parent
			// The parent frame was not acquired from the pool; clear the flag
			// to prevent RestoreAndRelease from releasing it.
			mc.envPooled = false
			mc.pc++

		case OpApply:
			vs := mc.evals.Drain()
			mc.counters.StackDrains++
			mc.counters.StackElementsDrained += uint64(len(vs))
			mc.counters.RecordStackDepth(len(vs))
			result, err := mc.ApplyCallable(mc.GetValue(), vs...)
			if err != nil {
				return applyCallableError(mc, err)
			}
			mc = result

		case OpUnpackListToStack:
			v := mc.GetValue()
			if values.IsEmptyList(v) {
				mc.pc++
				continue
			}
			tup, ok := v.(values.Tuple)
			if !ok {
				return applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrNotAList,
					"apply: final argument must be a list, got %s", v.SchemeString()))
			}
			sentinel, err := tup.ForEach(mc.ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
				mc.evals.Push(elem)
				return nil
			})
			if err != nil {
				return applyCallableError(mc, err)
			}
			if !values.IsEmptyList(sentinel) {
				return applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrNotAList,
					"apply: final argument is an improper list"))
			}
			mc.pc++

		case OpRestoreContinuation:
			if mc.cont == nil {
				return nil
			}
			mc.RestoreAndRelease(mc.cont)

		// --- Wave 2: single-operand operations ---

		case OpBranchOnFalseValue:
			if !values.ValueToBool(mc.GetValue()) {
				mc.pc += int(instr.Arg)
			} else {
				mc.pc++
			}

		case OpBranch:
			mc.pc += int(instr.Arg)

		case OpSaveContinuation:
			err := mc.SaveContinuation(int(instr.Arg))
			if err != nil {
				return err
			}
			mc.pc++

		case OpLoadLiteral:
			o := mc.template.literals[instr.Arg]
			mc.SetValue(o)
			mc.pc++

		case OpLoadGlobal:
			bd, err := mc.resolveGlobalBinding(instr)
			if err != nil {
				return err
			}
			mc.SetValue(bd.Value())
			mc.pc++

		case OpStoreGlobal:
			gi, err := mc.resolveGlobalIndex(instr)
			if err != nil {
				return err
			}
			val := mc.evals.Pop()
			if gi.Env != nil {
				err = gi.Env.SetOwnGlobalValue(gi, val)
			} else {
				err = mc.env.GlobalEnvironment().SetOwnGlobalValue(gi, val)
			}
			if err != nil {
				return mc.WrapError(ErrBindingNotFound,
					fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
			}
			mc.pc++

		case OpPeekK:
			mc.SetValue(mc.evals.PeekK(int(instr.Arg)))
			mc.pc++

		// --- Wave 3: two-operand operations (bit-packed slot|depth) ---

		case OpLoadLocal:
			bd, err := mc.resolveLocalBinding(instr)
			if err != nil {
				return err
			}
			mc.SetValue(bd.Value())
			mc.pc++

		case OpStoreLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			err := mc.env.SetLocalValueBySlotDepth(slot, depth, mc.evals.Pop())
			if err != nil {
				return mc.WrapError(err, "")
			}
			mc.pc++

		// --- Wave 4: fused push operations ---

		case OpPushLiteral:
			mc.evals.Push(mc.template.literals[instr.Arg])
			mc.pc++

		case OpPushGlobal:
			bd, err := mc.resolveGlobalBinding(instr)
			if err != nil {
				return err
			}
			mc.evals.Push(bd.Value())
			mc.pc++

		case OpPushLocal:
			bd, err := mc.resolveLocalBinding(instr)
			if err != nil {
				return err
			}
			mc.evals.Push(bd.Value())
			mc.pc++

		// --- Wave 5: fused call operations ---

		case OpPullApply:
			mc.SetValue(mc.evals.Pull())
			vs := mc.evals.Drain()
			mc.counters.StackDrains++
			mc.counters.StackElementsDrained += uint64(len(vs))
			mc.counters.RecordStackDepth(len(vs))
			result, err := mc.ApplyCallable(mc.GetValue(), vs...)
			if err != nil {
				return applyCallableError(mc, err)
			}
			mc = result

		// --- Wave 5: promoted complex operations ---

		case OpMakeClosure:
			compiletimeEnv, ok := mc.evals.Pop().(*environment.EnvironmentFrame)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotALocalEnvironmentFrame,
					"MakeClosure: expected environment frame on stack")
			}
			tpl, ok := mc.evals.Pop().(*NativeTemplate)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate,
					"MakeClosure: expected native template on stack")
			}
			runtimeEnv := environment.NewEnvironmentFrameWithParent(
				compiletimeEnv.LocalEnvironment(),
				mc.env,
			)
			// The closure now references mc.env through runtimeEnv's parent chain.
			// Mark it non-poolable so RestoreAndRelease won't recycle it while the
			// closure still holds a live reference.
			mc.envPooled = false
			cls := NewClosureWithTemplate(tpl, runtimeEnv)
			mc.SetValue(cls)
			mc.pc++

		// --- Wave 6: cached binding operations ---

		case OpLoadCachedBinding:
			mc.SetValue(mc.template.cachedBindings[instr.Arg].Value())
			mc.pc++

		case OpPushCachedBinding:
			mc.evals.Push(mc.template.cachedBindings[instr.Arg].Value())
			mc.pc++

		// --- Wave 7: direct foreign call operations ---

		case OpCallForeignCached:
			var err error
			mc, err = callForeignCached(mc, instr, false)
			if err != nil {
				return err
			}

		case OpCallForeignCachedTail:
			var err error
			mc, err = callForeignCached(mc, instr, true)
			if err != nil {
				return err
			}

		// --- Fallback: complex operations via side table ---

		case OpComplex:
			var err error
			mc, err = mc.template.sideTable[instr.Arg].Apply(mc)
			if err != nil {
				if errors.Is(err, errHalt) {
					return nil
				}
				return err
			}

		default:
			return werr.WrapForeignErrorf(werr.ErrUnknownOpCode,
				"unimplemented opcode: %s", instr.Op)
		}
	}
	return nil
}

// NewSubContext creates a new MachineContext for running sub-calls (e.g., apply, map, for-each).
// The sub-context shares the global environment but has a fresh call stack, eval stack, and value register.
// This allows foreign functions to call Scheme closures without corrupting the parent context's state.
//
// Note: Sub-contexts have isolated continuation chains (cont = nil). When call/cc captures a
// continuation inside a sub-context, it captures mc.Parent() which refers to the sub-context's
// chain (nil). For continuations to escape back to the outer context, the escape error propagates
// up through the call stack and is handled by RunWithEscapeHandling at the top level.
//
// The parentMC field tracks the parent context, allowing call/cc to find an outer continuation
// for proper R7RS continuation semantics when captured inside sub-contexts.
//
// The escapeCont field is inherited, allowing nested sub-contexts to know where execution
// should continue after their completion (set by dynamic-wind and similar constructs).
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	mc := acquireSubContext()
	mc.ctx = p.ctx
	// envPooled: zero value (false) — sub-context env is top-level, not from pool.
	mc.env = p.env.TopLevel()
	mc.evals = acquireStack()
	mc.threadID = p.threadID
	mc.parentMC = p
	mc.escapeCont = p.escapeCont
	mc.thread = p.thread
	mc.exceptionHandler = p.exceptionHandler
	mc.maxCallDepth = p.maxCallDepth
	mc.barrierValid = p.barrierValid // inherit barrier context
	return mc
}

// SubContextParams holds the parent state needed to create a thread's sub-context.
// This is used to avoid race conditions when creating sub-contexts across goroutine boundaries.
type SubContextParams struct {
	Ctx              context.Context
	Env              *environment.EnvironmentFrame
	ParentMC         *MachineContext
	EscapeCont       *MachineContinuation
	ExceptionHandler *ExceptionHandler
	MaxCallDepth     uint64
}

// CaptureSubContextParams extracts the state needed to create a sub-context in a different goroutine.
// This is used by thread creation to avoid race conditions when accessing the parent MachineContext
// from a child goroutine (T4 from architectural review).
//
// Call this in the parent goroutine before creating the child goroutine, then pass the result
// to NewThreadSubContext in the child goroutine.
func (p *MachineContext) CaptureSubContextParams() SubContextParams {
	return SubContextParams{
		Ctx:              p.ctx,
		Env:              p.env.TopLevel(),
		ParentMC:         p,
		EscapeCont:       p.escapeCont,
		ExceptionHandler: p.exceptionHandler,
		MaxCallDepth:     p.maxCallDepth,
	}
}

// NewThreadSubContext creates a sub-context for a thread using previously captured parent state.
// Unlike NewSubContext, this doesn't access the parent MachineContext fields, making it safe to call
// from a different goroutine. The thread parameter should be the new thread object, which provides
// the thread identity for the new context.
//
// This function is specifically designed for SRFI-18 thread creation. For other uses of sub-contexts
// (like map, for-each, dynamic-wind), use NewSubContext instead.
func NewThreadSubContext(params SubContextParams, thread *values.Thread) *MachineContext {
	sub := &MachineContext{
		ctx: params.Ctx,
		vmState: vmState{
			env:   params.Env,
			evals: NewStack(),
			// threadID will be set by SetThread below
		},
		parentMC:         params.ParentMC,
		escapeCont:       params.EscapeCont,
		exceptionHandler: params.ExceptionHandler,
		maxCallDepth:     params.MaxCallDepth,
		// thread will be set by SetThread below
	}
	sub.SetThread(thread) // Sets both thread object and threadID from thread.ID()
	return sub
}

// SetExpanderContext sets the expander context for this machine context.
// This is called when invoking macro transformers to enable syntax-local-* primitives.
func (p *MachineContext) SetExpanderContext(ctx *ExpanderContext) {
	p.expanderCtx = ctx
}

// ExpanderContext returns the expander context, or nil if not in expansion context.
func (p *MachineContext) ExpanderContext() *ExpanderContext {
	return p.expanderCtx
}

// ExceptionHandler returns the current exception handler chain.
func (p *MachineContext) ExceptionHandler() *ExceptionHandler {
	return p.exceptionHandler
}

// SetExceptionHandler sets the exception handler chain.
func (p *MachineContext) SetExceptionHandler(h *ExceptionHandler) {
	p.exceptionHandler = h
}

// PushExceptionHandler pushes a new exception handler onto the handler stack.
func (p *MachineContext) PushExceptionHandler(handler values.Callable) {
	p.exceptionHandler = NewExceptionHandler(handler, p.exceptionHandler)
}

// PopExceptionHandler pops the current exception handler from the stack and returns it.
// Returns nil if no handler is installed.
func (p *MachineContext) PopExceptionHandler() *ExceptionHandler {
	if p.exceptionHandler == nil {
		return nil
	}
	h := p.exceptionHandler
	p.exceptionHandler = h.parent
	return h
}

// SetDebugger attaches a debugger to this context.
func (p *MachineContext) SetDebugger(d *Debugger) {
	p.debugger = d
}

// Debugger returns the attached debugger, or nil if none.
func (p *MachineContext) Debugger() *Debugger {
	return p.debugger
}

// CurrentSource returns the source location for the current execution point.
// When the current template has no source (e.g., inside a foreign function),
// walks up the continuation chain to find the nearest call site with source info.
// Continuation PCs are return addresses (one past the call), so pc-1 gives the call site.
func (p *MachineContext) CurrentSource() *syntax.SourceContext {
	if p.template != nil {
		src := p.template.SourceAt(p.pc)
		if src != nil {
			return src
		}
	}
	// Walk continuation chain looking for source info
	cont := p.cont
	for cont != nil {
		if cont.template != nil {
			src := cont.template.SourceAt(cont.pc - 1)
			if src != nil {
				return src
			}
		}
		cont = cont.parent
	}
	return nil
}

// CaptureStackTrace walks the continuation chain and builds a stack trace.
func (p *MachineContext) CaptureStackTrace(maxDepth int) StackTrace {
	trace := make(StackTrace, 0, 16)

	// Current frame
	if p.template != nil {
		trace = append(trace, StackFrame{
			FunctionName: p.template.Name(),
			CurrentLoc:   p.template.SourceAt(p.pc),
		})
	}

	// Walk continuation chain.
	// Continuation PCs are return addresses (one past the call instruction),
	// so pc-1 gives the call site source location.
	cont := p.cont
	depth := 1
	for cont != nil && depth < maxDepth {
		frame := StackFrame{}
		if cont.template != nil {
			frame.FunctionName = cont.template.Name()
			frame.CurrentLoc = cont.template.SourceAt(cont.pc - 1)
		}
		trace = append(trace, frame)
		cont = cont.parent
		depth++
	}

	if cont == nil {
		return trace
	}

	remaining := countFrames(cont)
	if remaining > 0 {
		trace = append(trace, StackFrame{
			FunctionName: fmt.Sprintf("... %d more frames ...", remaining),
		})
	}

	return trace
}

// countFrames counts the number of frames in the continuation chain.
func countFrames(cont *MachineContinuation) int {
	count := 0
	for cont != nil {
		count++
		cont = cont.parent
	}
	return count
}

// resolveGlobalIndex extracts and validates the GlobalIndex from the
// instruction's literal slot. Returns ErrInvalidLiteralIndex if the literal
// is nil, or ErrInvalidGlobalIndex if it's not a *GlobalIndex.
func (p *MachineContext) resolveGlobalIndex(instr Instruction) (*environment.GlobalIndex, error) {
	o := p.template.literals[instr.Arg]
	if o == nil {
		return nil, p.WrapError(ErrInvalidLiteralIndex,
			fmt.Sprintf("literal index %v does not exist", instr.Arg))
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, p.WrapError(ErrInvalidGlobalIndex,
			fmt.Sprintf("literal %v is not a global index", o))
	}
	return gi, nil
}

// resolveGlobalBinding extracts the GlobalIndex from the instruction's literal
// and resolves it to a binding. Returns ErrInvalidLiteralIndex if the literal
// is nil, ErrInvalidGlobalIndex if it's not a *GlobalIndex, or
// ErrBindingNotFound if the binding doesn't exist.
func (p *MachineContext) resolveGlobalBinding(instr Instruction) (*environment.Binding, error) {
	gi, err := p.resolveGlobalIndex(instr)
	if err != nil {
		return nil, err
	}
	var bd *environment.Binding
	if gi.Env != nil {
		bd = gi.Env.GetOwnGlobalBinding(gi)
	} else {
		bd = p.env.GetGlobalBinding(gi)
	}
	if bd == nil {
		return nil, p.WrapError(ErrBindingNotFound,
			fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
	}
	return bd, nil
}

// resolveLocalBinding decodes the slot/depth from the instruction and resolves
// the local binding. Returns ErrBindingNotFound if the binding doesn't exist.
func (p *MachineContext) resolveLocalBinding(instr Instruction) (*environment.Binding, error) {
	slot, depth := DecodeLocalIndex(instr.Arg)
	bd := p.env.GetLocalBindingBySlotDepth(slot, depth)
	if bd == nil {
		return nil, p.WrapError(ErrBindingNotFound,
			fmt.Sprintf("no such local binding %d:%d", slot, depth))
	}
	return bd, nil
}

// Error creates a SchemeError with the current source location and stack trace.
func (p *MachineContext) Error(msg string) *SchemeError {
	source := p.CurrentSource()
	trace := p.CaptureStackTrace(20)
	return NewSchemeError(msg, source, trace.String())
}

// WrapError wraps an existing error with the current source location and stack trace.
func (p *MachineContext) WrapError(err error, msg string) *SchemeError {
	source := p.CurrentSource()
	trace := p.CaptureStackTrace(20)
	if msg == "" {
		msg = err.Error()
	}
	return NewSchemeErrorWithCause(msg, source, trace.String(), err)
}

// Counters returns a snapshot of the performance counters for this context.
func (p *MachineContext) Counters() VMCounters {
	return p.counters
}

// ThreadID returns the SRFI-18 thread ID for this context.
// 0 means the primordial thread (main goroutine).
func (p *MachineContext) ThreadID() uint64 {
	return p.threadID
}

// Thread returns the SRFI-18 thread object for this context, or nil for the primordial thread.
func (p *MachineContext) Thread() *values.Thread {
	return p.thread
}

// MaxCallDepth returns the maximum call depth limit. 0 means unlimited.
func (p *MachineContext) MaxCallDepth() uint64 {
	return p.maxCallDepth
}

// SetMaxCallDepth sets the maximum call depth limit. 0 means unlimited.
func (p *MachineContext) SetMaxCallDepth(n uint64) {
	p.maxCallDepth = n
}

// SetThread sets the SRFI-18 thread identity on this context.
// Both the thread object and its ID are stored for efficient comparison.
func (p *MachineContext) SetThread(t *values.Thread) {
	p.thread = t
	if t != nil {
		p.threadID = t.ID()
	} else {
		p.threadID = 0
	}
}

// RunWithEscapeHandling runs the VM loop, handling continuation escapes
// that weren't caught by an enclosing call/cc. This is used at the top level
// (REPL and file execution) to catch continuations invoked outside their
// original dynamic extent.
//
// When a continuation captured inside a foreign function (like dynamic-wind's thunk)
// is invoked from outside, the escape error propagates up. This method catches it
// and restores the continuation with proper dynamic-wind handling.
//
// For continuations captured inside sub-contexts (like dynamic-wind thunks):
//   - Continuation: the inner state (inside the thunk)
//   - EscapeCont: the outer continuation (after the original sub-context would have completed)
//
// After the inner execution completes and unwinds, if there's a pending escape
// continuation, execution continues from there.
//
// When execution completes normally (Run returns nil), any remaining
// frames on the winding stack are unwound (after thunks are called).
func (p *MachineContext) RunWithEscapeHandling() error {
	p.promptTag = DefaultPromptTag // install default prompt for call/cc escapes
	for {
		err := p.Run()

		// Check for successful completion
		if err == nil {
			// Unwind any remaining frames (call after thunks)
			if len(p.windingStack) > 0 {
				unwindErr := p.UnwindTo(0)
				if unwindErr != nil {
					return unwindErr
				}
			}

			return nil
		}

		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			prompt, found := p.FindPrompt(abortErr.Tag)
			if !found {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "abort-current-continuation: no prompt found for tag %s", abortErr.Tag.SchemeString())
			}

			// Unwind dynamic-wind from current to prompt's winding depth.
			// When prompt is nil (context-level prompt), the target winding stack
			// is nil — the context boundary has no saved winding state.
			var targetStack WindingStack
			if prompt != nil {
				targetStack = prompt.windingStack
			}
			restoreErr := p.RestoreWithWindingFrom(nil, p.windingStack, targetStack)
			if restoreErr != nil {
				return restoreErr
			}

			// Restore to the prompt frame (skip past it).
			// When prompt is nil (context-level), there's no frame to restore —
			// the context itself is the boundary.
			if prompt != nil {
				p.Restore(prompt)
			}

			// Invoke the handler with the abort values.
			// Context-level prompts have no handler (prompt is nil).
			if prompt != nil && prompt.PromptHandler() != nil {
				_, applyErr := p.ApplyCallable(prompt.PromptHandler(), abortErr.Values...)
				if applyErr != nil {
					return applyErr
				}
			} else {
				if len(abortErr.Values) > 0 {
					p.SetValues(abortErr.Values...)
				} else {
					p.SetValue(values.Void)
				}
				// Context-level abort (prompt == nil): the composable continuation
				// has already run to completion inside the escape closure's sub-context.
				// The abort value is the final result; p.pc was not advanced (FFC
				// returned error without incrementing), so there is no remaining code
				// to execute. Returning nil avoids re-running the FFC at p.pc.
				if prompt == nil {
					return nil
				}
			}
			continue
		}

		return err
	}
}
