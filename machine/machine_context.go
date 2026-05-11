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
	"github.com/aalpar/wile/security"
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
var ErrInvalidProgramCounter = werr.NewStaticError("invalid program counter")

// Compile-time assertion: *MachineContext must satisfy values.DebugState.
var _ values.DebugState = (*MachineContext)(nil)

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
	expanderCtx      ExpanderCtx          // set during macro transformer execution for syntax-local-* access
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
	thread *values.Thread
	// syntaxCase holds per-context syntax-case expansion state, owned by
	// machine/compilation/. Typed as any because machine/ cannot import
	// compilation/ (one-direction dependency rule). The constraint —
	// "always *compilation.syntaxCaseState or nil" — is enforced by
	// encapsulation: the field is unexported and the accessors are the
	// only entry points; in practice exactly one production package
	// (compilation/) calls them, so type-system gating would only restate
	// what package boundaries already guarantee.
	syntaxCase    any
	maxCallDepth  int              // 0 = unlimited (default); negatives are clamped to 0 by SetMaxCallDepth
	maxStackSize  uint64           // 0 = unlimited (default), otherwise max eval stack entries
	restArgBuf    values.PairBlock // reusable buffer for variadic rest-arg list construction (ForeignClosure calls)
	isolatedMarks bool             // when true, findParameterInMarks does not walk parentMC; set by applyCapturedContinuation

	timerHandler values.Callable    // nil = no timer active
	timerCancel  context.CancelFunc // cancels the child timeout context; nil when no timer
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
		cont:     cont.parent,
		counters: VMCounters{opcodeHits: newOpcodeHits(), callCounts: newCallCounts()},
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

// Evals returns the eval stack for inspection (primarily for testing).
func (p *MachineContext) Evals() *Stack {
	return p.evals
}

func (p *MachineContext) PC() int {
	return p.pc
}

// SetPC sets the program counter. Used by PrimCallCC for inline lambda execution.
func (p *MachineContext) SetPC(v int) {
	p.pc = v
}

// IncrPC increments the program counter by one.
func (p *MachineContext) IncrPC() {
	p.pc++
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

// SetEnvironmentFrame replaces the current environment frame.
// Used by operations that push new scopes (e.g., OperationBindPatternVars).
func (p *MachineContext) SetEnvironmentFrame(env *environment.EnvironmentFrame) {
	p.env = env
}

// SetEnvPooled controls whether the current environment frame will be
// recycled by RestoreAndRelease. Set to false after replacing env with
// a heap-allocated frame that must not be returned to envFramePool.
func (p *MachineContext) SetEnvPooled(v bool) {
	p.envPooled = v
}

// Authorizer returns the security authorizer from this context's namespace,
// or nil if none is set.
func (p *MachineContext) Authorizer() security.Authorizer {
	ns := p.env.Namespace()
	if ns == nil {
		return nil
	}
	return ns.Authorizer()
}

func (p *MachineContext) Arg(index int) values.Value {
	return p.env.GetLocalBindingByIndex(index).Value()
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

// TimerHandler returns the current timer interrupt handler, or nil if no timer is active.
func (p *MachineContext) TimerHandler() values.Callable {
	return p.timerHandler
}

// SetTimerHandler installs or clears the timer interrupt handler.
func (p *MachineContext) SetTimerHandler(h values.Callable) {
	p.timerHandler = h
}

// TimerCancel returns the cancel function for the active timer context, or nil.
func (p *MachineContext) TimerCancel() context.CancelFunc {
	return p.timerCancel
}

// SetTimerCancel installs or clears the timer cancel function.
func (p *MachineContext) SetTimerCancel(cancel context.CancelFunc) {
	p.timerCancel = cancel
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
	if mc.pc < 0 {
		return werr.WrapForeignErrorf(ErrInvalidProgramCounter,
			"Run: negative program counter %d", mc.pc)
	}
	for mc.pc < len(mc.template.code) {
		if mc.counters.OpsExecuted&contextCheckMask == 0 {
			select {
			case <-mc.ctx.Done():
				if mc.timerHandler != nil && errors.Is(context.Cause(mc.ctx), ErrTimerExpired) {
					return &ErrTimerInterrupt{Handler: mc.timerHandler}
				}
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
		if mc.counters.opcodeHits != nil {
			mc.counters.opcodeHits[instr.Op]++
		}

		if mc.template.executed != nil {
			mc.template.executed[mc.pc] = true
		}

		switch instr.Op {
		// --- Wave 1: zero-operand operations ---

		case OpPush:
			if mc.multiValues != nil {
				mc.evals.PushAll(mc.multiValues)
			} else if mc.singleValue != nil {
				mc.evals.Push(mc.singleValue)
			}
			err := mc.checkStackSizeFast()
			if err != nil {
				return err
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
			var err error
			mc, err = mc.drainAndApply(mc.GetValue())
			if err != nil {
				return err
			}

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
			errStack := mc.checkStackSizeFast()
			if errStack != nil {
				return errStack
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

		case OpPushEnv:
			slotCount := int(instr.Arg)
			lenv := environment.NewLocalEnvironment(slotCount)
			mc.env = environment.NewEnvironmentFrameWithParent(lenv, mc.env)
			mc.envPooled = false
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
			err := mc.checkStackSizeFast()
			if err != nil {
				return err
			}
			mc.pc++

		case OpPushGlobal:
			bd, err := mc.resolveGlobalBinding(instr)
			if err != nil {
				return err
			}
			mc.evals.Push(bd.Value())
			errStack := mc.checkStackSizeFast()
			if errStack != nil {
				return errStack
			}
			mc.pc++

		case OpPushLocal:
			bd, err := mc.resolveLocalBinding(instr)
			if err != nil {
				return err
			}
			mc.evals.Push(bd.Value())
			errStack := mc.checkStackSizeFast()
			if errStack != nil {
				return errStack
			}
			mc.pc++

		// --- Wave 5: fused call operations ---

		case OpPullApply:
			var err error
			mc, err = mc.pullDrainAndApply()
			if err != nil {
				return err
			}

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
			err := mc.checkStackSizeFast()
			if err != nil {
				return err
			}
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

		// --- Wave 8: general call fusion ---

		case OpCallLocal:
			bd, err := mc.resolveLocalBinding(instr)
			if err != nil {
				return err
			}
			mc, err = mc.drainAndApply(bd.Value())
			if err != nil {
				return err
			}

		case OpCallCachedBinding:
			var err error
			mc, err = mc.drainAndApply(mc.template.cachedBindings[instr.Arg].Value())
			if err != nil {
				return err
			}

		// --- Wave 9: promoted primitive operations ---

		case OpEqQ:
			var err error
			mc, err = execPromoted(mc, instr, "eq?", 2, false, inlineEq)
			if err != nil {
				return err
			}

		case OpEqQTail:
			var err error
			mc, err = execPromoted(mc, instr, "eq?", 2, true, inlineEq)
			if err != nil {
				return err
			}

		case OpVectorQ:
			var err error
			mc, err = execPromoted(mc, instr, "vector?", 1, false, inlineVectorQ)
			if err != nil {
				return err
			}

		case OpVectorQTail:
			var err error
			mc, err = execPromoted(mc, instr, "vector?", 1, true, inlineVectorQ)
			if err != nil {
				return err
			}

		case OpVectorRef:
			var err error
			mc, err = execPromoted(mc, instr, "vector-ref", 2, false, inlineVectorRef)
			if err != nil {
				return err
			}

		case OpVectorRefTail:
			var err error
			mc, err = execPromoted(mc, instr, "vector-ref", 2, true, inlineVectorRef)
			if err != nil {
				return err
			}

		case OpNullQ:
			var err error
			mc, err = execPromoted(mc, instr, "null?", 1, false, inlineNullQ)
			if err != nil {
				return err
			}

		case OpNullQTail:
			var err error
			mc, err = execPromoted(mc, instr, "null?", 1, true, inlineNullQ)
			if err != nil {
				return err
			}

		case OpPairQ:
			var err error
			mc, err = execPromoted(mc, instr, "pair?", 1, false, inlinePairQ)
			if err != nil {
				return err
			}

		case OpPairQTail:
			var err error
			mc, err = execPromoted(mc, instr, "pair?", 1, true, inlinePairQ)
			if err != nil {
				return err
			}

		case OpCar:
			var err error
			mc, err = execPromoted(mc, instr, "car", 1, false, inlineCar)
			if err != nil {
				return err
			}

		case OpCarTail:
			var err error
			mc, err = execPromoted(mc, instr, "car", 1, true, inlineCar)
			if err != nil {
				return err
			}

		case OpCdr:
			var err error
			mc, err = execPromoted(mc, instr, "cdr", 1, false, inlineCdr)
			if err != nil {
				return err
			}

		case OpCdrTail:
			var err error
			mc, err = execPromoted(mc, instr, "cdr", 1, true, inlineCdr)
			if err != nil {
				return err
			}

		// --- Wave 10: promoted arithmetic operations (2-arg only) ---

		case OpAdd:
			var err error
			mc, err = execPromoted(mc, instr, "+", 2, false, inlineAdd)
			if err != nil {
				return err
			}

		case OpAddTail:
			var err error
			mc, err = execPromoted(mc, instr, "+", 2, true, inlineAdd)
			if err != nil {
				return err
			}

		case OpSub:
			var err error
			mc, err = execPromoted(mc, instr, "-", 2, false, inlineSub)
			if err != nil {
				return err
			}

		case OpSubTail:
			var err error
			mc, err = execPromoted(mc, instr, "-", 2, true, inlineSub)
			if err != nil {
				return err
			}

		case OpNumLt:
			var err error
			mc, err = execPromoted(mc, instr, "<", 2, false, inlineNumLt)
			if err != nil {
				return err
			}

		case OpNumLtTail:
			var err error
			mc, err = execPromoted(mc, instr, "<", 2, true, inlineNumLt)
			if err != nil {
				return err
			}

		case OpNumLe:
			var err error
			mc, err = execPromoted(mc, instr, "<=", 2, false, inlineNumLe)
			if err != nil {
				return err
			}

		case OpNumLeTail:
			var err error
			mc, err = execPromoted(mc, instr, "<=", 2, true, inlineNumLe)
			if err != nil {
				return err
			}

		case OpNumGt:
			var err error
			mc, err = execPromoted(mc, instr, ">", 2, false, inlineNumGt)
			if err != nil {
				return err
			}

		case OpNumGtTail:
			var err error
			mc, err = execPromoted(mc, instr, ">", 2, true, inlineNumGt)
			if err != nil {
				return err
			}

		case OpNumGe:
			var err error
			mc, err = execPromoted(mc, instr, ">=", 2, false, inlineNumGe)
			if err != nil {
				return err
			}

		case OpNumGeTail:
			var err error
			mc, err = execPromoted(mc, instr, ">=", 2, true, inlineNumGe)
			if err != nil {
				return err
			}

		case OpNumEq:
			var err error
			mc, err = execPromoted(mc, instr, "=", 2, false, inlineNumEq)
			if err != nil {
				return err
			}

		case OpNumEqTail:
			var err error
			mc, err = execPromoted(mc, instr, "=", 2, true, inlineNumEq)
			if err != nil {
				return err
			}

		case OpCons:
			var err error
			mc, err = execPromoted(mc, instr, "cons", 2, false, inlineCons)
			if err != nil {
				return err
			}

		case OpConsTail:
			var err error
			mc, err = execPromoted(mc, instr, "cons", 2, true, inlineCons)
			if err != nil {
				return err
			}

		case OpMul:
			var err error
			mc, err = execPromoted(mc, instr, "*", 2, false, inlineMul)
			if err != nil {
				return err
			}

		case OpMulTail:
			var err error
			mc, err = execPromoted(mc, instr, "*", 2, true, inlineMul)
			if err != nil {
				return err
			}

		case OpDiv:
			var err error
			mc, err = execPromoted(mc, instr, "/", 2, false, inlineDiv)
			if err != nil {
				return err
			}

		case OpDivTail:
			var err error
			mc, err = execPromoted(mc, instr, "/", 2, true, inlineDiv)
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

// SetExpanderContext sets the expander context for this machine context.
// This is called when invoking macro transformers to enable syntax-local-* primitives.
func (p *MachineContext) SetExpanderContext(ctx ExpanderCtx) {
	p.expanderCtx = ctx
}

// ExpanderContext returns the expander context, or nil if not in expansion context.
func (p *MachineContext) ExpanderContext() ExpanderCtx {
	return p.expanderCtx
}

// SyntaxCaseState returns the per-context syntax-case expansion state, or nil
// when not in a syntax-case expansion. The concrete type is owned by
// machine/compilation/; callers within that subpackage type-assert to
// *syntaxCaseState. See the comment on the syntaxCase field for why this is
// any-typed.
func (p *MachineContext) SyntaxCaseState() any {
	return p.syntaxCase
}

// SetSyntaxCaseState installs the syntax-case expansion state on the context,
// or nil to clear it. In production, the only legitimate concrete type is
// *compilation.syntaxCaseState; the constraint is enforced by encapsulation
// (unexported field, single-package consumer) rather than the type system.
func (p *MachineContext) SetSyntaxCaseState(v any) {
	p.syntaxCase = v
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

// CurrentLocation returns the current source location as a
// values.DebugLocation, or nil if no source info is available.
// Implements values.DebugState.
func (p *MachineContext) CurrentLocation() *values.DebugLocation {
	src := p.CurrentSource()
	if src == nil {
		return nil
	}
	return &values.DebugLocation{
		File:   src.File,
		Line:   src.Start.Line(),
		Column: src.Start.Column(),
	}
}

// FormatStackTrace returns a human-readable stack trace string.
// Implements values.DebugState.
func (p *MachineContext) FormatStackTrace(maxDepth int) string {
	trace := p.CaptureStackTrace(maxDepth)
	return trace.String()
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
// The stored value is always non-negative; SetMaxCallDepth clamps negatives.
func (p *MachineContext) MaxCallDepth() int {
	return p.maxCallDepth
}

// SetMaxCallDepth sets the maximum call depth limit. 0 means unlimited.
// Negative values are clamped to 0 (also unlimited).
func (p *MachineContext) SetMaxCallDepth(n int) {
	if n < 0 {
		n = 0
	}
	p.maxCallDepth = n
}

// MaxStackSize returns the maximum eval stack size limit. 0 means unlimited.
func (p *MachineContext) MaxStackSize() uint64 {
	return p.maxStackSize
}

// SetMaxStackSize sets the maximum eval stack size limit. 0 means unlimited.
func (p *MachineContext) SetMaxStackSize(n uint64) {
	p.maxStackSize = n
}

// checkStackSizeFast is the inlinable wrapper used at every push-opcode call
// site. The unlimited-stack default (maxStackSize == 0) takes a single
// predicted-not-taken branch with no further work; the limited path tail-calls
// checkStackSizeSlow. Splitting the cold side keeps this function under Go's
// inline budget (PR #636 documented that the unsplit check, at cost 110, was
// over the 80-cost budget and its format args escaped to heap — that was the
// original reason the if-guard was hand-inlined at 6 sites). Finding 5
// (plans/2026-05-06-machine-structural-reduction.md) replaced the 6 hand
// inlined guards with this two-line wrapper call: same coverage, same
// hot path, source-level deduplication.
func (p *MachineContext) checkStackSizeFast() error {
	if p.maxStackSize == 0 {
		return nil
	}
	return p.checkStackSizeSlow()
}

// checkStackSizeSlow returns ErrStackOverflow when the eval stack exceeds
// the configured maximum. The caller (checkStackSizeFast) guarantees
// maxStackSize > 0, so the redundant zero-test is intentionally absent.
func (p *MachineContext) checkStackSizeSlow() error {
	if uint64(p.evals.Len()) > p.maxStackSize {
		return werr.WrapForeignErrorf(werr.ErrStackOverflow,
			"eval stack size %d exceeds limit %d", p.evals.Len(), p.maxStackSize)
	}
	return nil
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

// SetMark sets a continuation mark on the current frame using eq? semantics.
// Updates an existing entry with the same key, or appends a new one.
func (p *MachineContext) SetMark(key, val values.Value) {
	for i := range p.marks {
		if eqIdentity(p.marks[i].key, key) {
			p.marks[i].val = val
			return
		}
	}
	p.marks = append(p.marks, markEntry{key: key, val: val})
}

// GetMark returns the continuation mark for key on the current frame,
// or nil if no mark is set. Uses eq? semantics for key comparison.
// Invariant: nil is used exclusively as the "not found" sentinel. Mark values
// are always non-nil Scheme values (GetValue never returns nil); SetMark must
// never be called with a nil val, as nil would be indistinguishable from absent.
func (p *MachineContext) GetMark(key values.Value) values.Value {
	for _, e := range p.marks {
		if eqIdentity(e.key, key) {
			return e.val
		}
	}
	return nil
}

// GetImmediateMark returns the nearest mark for key, checking the current
// frame first, then the immediately saved continuation frame.
//
// This is the correct lookup for call-with-immediate-continuation-mark:
// in tail position, with-continuation-mark sets the mark on the live frame
// (mc.marks); in non-tail position, SaveContinuation moves mc.marks to
// mc.cont and nils the live frame. Both cases are handled here.
func (p *MachineContext) GetImmediateMark(key values.Value) values.Value {
	val := p.GetMark(key)
	if val != nil {
		return val
	}
	if p.cont != nil {
		for _, e := range p.cont.marks {
			if eqIdentity(e.key, key) {
				return e.val
			}
		}
	}
	return nil
}

// DeleteMark removes the continuation mark for key from the current frame.
// Nils the slice when empty to maintain the "nil = zero-cost" invariant.
// Uses eq? semantics for key comparison.
// Deletion uses swap-with-last for O(1) removal; insertion order is not preserved.
func (p *MachineContext) DeleteMark(key values.Value) {
	for i := range p.marks {
		if eqIdentity(p.marks[i].key, key) {
			p.marks[i] = p.marks[len(p.marks)-1]
			p.marks[len(p.marks)-1] = markEntry{}
			p.marks = p.marks[:len(p.marks)-1]
			if len(p.marks) == 0 {
				p.marks = nil
			}
			return
		}
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

	// freshCancel tracks the cancel function for any recovery context
	// installed after a timer interrupt. Cleaned up on function exit.
	var freshCancel context.CancelFunc
	defer func() {
		if freshCancel != nil {
			freshCancel()
		}
	}()

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

		var timerErr *ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			// Capture the full computation as a composable continuation.
			segment := p.CaptureInterruptContinuation()
			windingCopy := p.WindingStack().Copy()
			resumable := NewComposableContinuation(
				segment, windingCopy, p.threadID, p.barrierValid,
			)

			// Clear timer state (prevent re-entry from stale handler).
			if p.timerCancel != nil {
				p.timerCancel()
			}
			p.timerHandler = nil
			p.timerCancel = nil

			// Install a fresh cancellable context (the timed-out context is done).
			// Cancel any previous recovery context before creating a new one.
			if freshCancel != nil {
				freshCancel()
			}
			ctx, fc := context.WithCancel(context.Background())
			freshCancel = fc
			p.SetContext(ctx)

			// Call the handler with the resumable continuation.
			_, applyErr := p.ApplyCallable(timerErr.Handler, resumable)
			if applyErr != nil {
				freshCancel()
				return applyErr
			}
			continue
		}

		return err
	}
}
