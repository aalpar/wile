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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
	cont      *MachineContinuation // current continuation
	expansion *expansionState      // macro-expansion-time state, nil at runtime; see expansionState
	// capturedMarks is set when this context runs a resumed call/cc continuation
	// (isolatedMarks): it is the capture-time reachable-marks snapshot, consulted by
	// findParameterInMarks at the isolatedMarks break so the continuation's outer
	// parameter/handler marks (from above the sub-context boundary it was captured
	// behind) survive resume. See collectReachableMarks / SnapshotReachableMarksInto.
	capturedMarks []markEntry
	debugger      *Debugger            // optional debugger for breakpoints and stepping
	parentMC      *MachineContext      // parent context for sub-contexts, enables call/cc escape tracking
	escapeCont    *MachineContinuation // escape continuation for sub-contexts: where to continue after sub-context completes
	// barrierValid moved to vmState so it rides the continuation chain (crossing
	// detection survives capture/re-entry); BarrierValid()/SetBarrierValid promote from there.
	counters VMCounters // performance counters (plain uint64, single-goroutine)
	// thread is the SRFI-18 thread object (nil = primordial thread).
	// This is the Scheme-visible half of the thread identity split.
	// The numeric half (threadID) lives in vmState and propagates into
	// continuations. See the comment on vmState.threadID for the full
	// design and invariant.
	thread        *values.Thread
	maxCallDepth  int              // 0 = unlimited (default); negatives are clamped to 0 by SetMaxCallDepth
	maxStackSize  uint64           // 0 = unlimited (default), otherwise max eval stack entries
	restArgBuf    values.PairBlock // reusable buffer for variadic rest-arg list construction (ForeignClosure calls)
	isolatedMarks bool             // when true, findParameterInMarks does not walk parentMC; set by applyCapturedContinuation

	// resumeGeneration counts continuation-segment reinstatements (ReinstallSegment)
	// on this driver. raiseToHandlers snapshots it when it arms a non-continuable
	// handler's escalator frame; escalateFn compares against the snapshot. A change
	// means handler-k was resumed THROUGH the escalator (forward the resumed value)
	// rather than the handler returning naturally (escalate the R7RS §6.11 secondary
	// exception). This replaces the sticky, context-global isolatedMarks gate, which
	// stayed true after ANY prior resume and so swallowed legitimate secondary
	// exceptions once a continuation had been resumed on the driver.
	resumeGeneration uint64

	// reconfigured is set by Apply when it repoints the VM (template/env/pc) to
	// execute a closure in place. The foreign-call dispatchers (applyForeign,
	// callForeignCached) consult it to decide whether a primitive reconfigured
	// the VM for continued execution. It is the reliable signal where the older
	// "template != savedTemplate" inference is a false negative: when a
	// procedure applies *itself* (proc.template == caller.template, e.g.
	// (call/cc f) evaluated inside f, or self-recursive apply), the template is
	// unchanged even though Apply ran. Cleared before each foreign call.
	reconfigured bool

	// pools holds this thread's private allocation freelists, minted at the
	// thread root and inherited by reference through NewSubContext (same
	// goroutine). nil means fall back to the process-global pools. Like the
	// fields above and unlike vmState, it describes the executing context and
	// must NOT be saved into continuations. See threadPools in pool.go.
	pools *threadPools

	timer *timerState // nil = no timer active; both handler and cancel set together
}

// timerState clusters the MachineContext fields that always travel
// together for an active (with-timeout …): the timer interrupt handler,
// the cancel function for the child timeout context, the prompt tag of
// this with-timeout's finalizer frame, the enclosing timer (nesting), and
// the saved outer ctx. The invariant (handler == nil) ⇔ (cancel == nil)
// is load-bearing: the sole production writer is RunBodyUnderTimer, which
// installs a fresh record (pushing the prior one via the parent pointer)
// and whose finalizer — together with resolveTimerInterrupt on a fired
// timer — restores the parent and cancels, as one atomic teardown.
//
// Finding 7 of plans/2026-05-06-machine-structural-reduction.md
// (stage 2 of 3); see plans/2026-05-12-machine-sr-finding7-timer-impl.md.
// The bare install/inspect/clear API (SetTimer/ClearTimer/TimerHandler)
// was retired once with-timeout was reified onto this linked stack: its
// single-timer guard was incompatible with the nesting the parent pointer
// now provides.
type timerState struct {
	handler  values.Callable
	cancel   context.CancelFunc
	tag      *PromptTag      // identifies this with-timeout's finalizer frame on the chain
	parent   *timerState     // enclosing timer (nesting); restored on teardown
	savedCtx context.Context // ctx active before this with-timeout; restored on teardown
}

// expansionState clusters the two MachineContext fields that are nil at
// runtime and only become non-nil during macro expansion: the expander
// context (for syntax-local-* primitive access) and the syntax-case
// state (per-context bindings + matcher). Lazy-allocated on first write
// via SetExpanderContext or SetSyntaxCaseState; nil reads return the
// zero value through the accessors below.
//
// Finding 7 of plans/2026-05-06-machine-structural-reduction.md
// (stage 1 of 3); see plans/2026-05-12-machine-sr-finding7-expansion-impl.md.
//
// The 'expanderCtx' field is typed ExpanderCtx (an interface defined in
// machine/) so the dependency direction (compilation → machine) holds.
// The 'syntaxCase' field is 'any' for the same reason the old field was:
// machine/ cannot import machine/compilation/, so the concrete type
// (*compilation.syntaxCaseState) lives only at the consumer.
type expansionState struct {
	expanderCtx ExpanderCtx
	syntaxCase  any
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
			env:      cont.env,      // cannot copy environment here, it will be copied when pushed onto the stack
			template: cont.template, // not needed to copy, templates are immutable
			evals:    evals,
			pc:       cont.pc,
		},
		cont:     cont.parent,
		counters: VMCounters{opcodeHits: newOpcodeHits(), callCounts: newCallCounts()},
		pools:    newThreadPools(), // thread root: its own freelists
	}
	// Shallow copy: the singleValue / multiValues fields are transferred
	// by reference, not deep-cloned — they're passed between contexts.
	q.copyValueRegisterFrom(&cont.vmState)
	return q
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

// Value-register accessors (SetValue, SetValues, GetValue, GetValues) live
// on *vmState in vm_state.go; method promotion makes them callable here.

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

// ImmutableLiterals returns the engine-scoped immutable-literal set from this
// context's namespace, or nil if none is set. The five list/vector mutators
// consult it to enforce R7RS §4.1.2 constant immutability.
func (p *MachineContext) ImmutableLiterals() *environment.ImmutableLiterals {
	ns := p.env.Namespace()
	if ns == nil {
		return nil
	}
	return ns.ImmutableLiterals()
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
		// Mint through WrapError so this VM-internal failure carries source +
		// backtrace like the resolver failures below — NOT bare werr, which the
		// embedder receives with no provenance. (SourceAt bounds-checks, so a bad
		// pc degrades to nil source, not a panic.) The `return err` pass-throughs
		// in the switch are already condition-enriched via applyCallableError;
		// only fresh mints at a live-MC boundary belong here.
		return mc.WrapError(ErrInvalidProgramCounter,
			fmt.Sprintf("Run: negative program counter %d", mc.pc))
	}
	for mc.pc < len(mc.template.code) {
		if mc.counters.OpsExecuted&contextCheckMask == 0 {
			select {
			case <-mc.ctx.Done():
				if mc.timer != nil && errors.Is(context.Cause(mc.ctx), ErrTimerExpired) {
					return &ErrTimerInterrupt{Handler: mc.timer.handler, Tag: mc.timer.tag}
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
			mc.pushValueRegisterTo(mc.evals)
			err := mc.checkStackSize()
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
				// Fresh VM-internal mint → enrich with source + trace via WrapError.
				return mc.WrapError(werr.ErrNilParentEnvironment,
					"PopEnv: cannot pop top-level environment")
			}
			mc.env = parent
			// The parent frame was not acquired from the pool; clear the flag
			// to prevent RestoreAndRelease from releasing it.
			mc.envPooled = false
			mc.pc++

		case OpReleaseEnvFrame:
			// Release the current frame back to the pool before a reclaimable tail
			// call: it is dead (the call's args are already on the eval stack) and
			// the emit-time proof (no capture, no escaping closure, only capture-safe
			// callees) guarantees no continuation can still reach it. The next
			// acquire (the tail call's own frame) reuses it. envPooled gates release
			// to genuinely pool-owned frames and prevents a double release.
			if mc.envPooled {
				mc.releaseEnvFrame(mc.env)
				mc.envPooled = false
			}
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
				rerr := applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrNotAList,
					"apply: final argument must be a list, got %s", v.SchemeString()))
				if rerr != nil {
					return rerr
				}
				// Bridged to an in-place handler that reconfigured mc; re-dispatch into it.
				continue
			}
			err := values.ForEachProperList(mc.ctx, tup, "apply", func(_ context.Context, _ int, _ bool, elem values.Value) error {
				mc.evals.Push(elem)
				return nil
			})
			if err != nil {
				rerr := applyCallableError(mc, err)
				if rerr != nil {
					return rerr
				}
				continue
			}
			err = mc.checkStackSize()
			if err != nil {
				return err
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

		case OpSelfTailCall:
			// In-place self-recursive tail call: the args were already evaluated
			// onto the eval stack with the OLD slot values intact, so draining then
			// writing them into the current frame's parameter slots is a parallel
			// assignment. No frame acquire, no SaveContinuation — the emit-time proof
			// (bodyIsSelfTailReusable: no capture, no escaping closure, non-variadic,
			// depth-0) guarantees the live frame is reachable only through mc.env.
			argCount := int(instr.Arg)
			if argCount > 0 {
				// DrainN (not Drain) takes EXACTLY argCount from the stack top, in slot
				// order, with no allocation: it leaves any residue below intact rather
				// than silently mis-binding it, and a count/stack mismatch panics with a
				// wrapped ErrStackUnderflow instead of a raw out-of-range index.
				vs := mc.evals.DrainN(argCount)
				bindArgs(mc.env.LocalEnvironment().Bindings(), vs, argCount, false, nil)
			}
			mc.pc = 0

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
			err := mc.checkStackSize()
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
			err = mc.checkStackSize()
			if err != nil {
				return err
			}
			mc.pc++

		case OpPushLocal:
			bd, err := mc.resolveLocalBinding(instr)
			if err != nil {
				return err
			}
			mc.evals.Push(bd.Value())
			err = mc.checkStackSize()
			if err != nil {
				return err
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
			compiletimeEnv, tpl, err := popMakeClosureArgs(mc)
			if err != nil {
				return err
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
			err := mc.checkStackSize()
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
			// Fresh VM-internal mint → WrapError points the source at the
			// offending instruction's pc (best possible provenance here).
			return mc.WrapError(werr.ErrUnknownOpCode,
				fmt.Sprintf("unimplemented opcode: %s", instr.Op))
		}
	}
	return nil
}

// SetExpanderContext sets the expander context for this machine context.
// This is called when invoking macro transformers to enable syntax-local-*
// primitives. Lazy-allocates the expansion sub-record on first write.
func (p *MachineContext) SetExpanderContext(ctx ExpanderCtx) {
	if p.expansion == nil {
		if ctx == nil {
			return // common case: clearing a never-set expander; no allocation
		}
		p.expansion = &expansionState{}
	}
	p.expansion.expanderCtx = ctx
}

// ExpanderContext returns the expander context, or nil if not in expansion context.
func (p *MachineContext) ExpanderContext() ExpanderCtx {
	if p.expansion == nil {
		return nil
	}
	return p.expansion.expanderCtx
}

// SyntaxCaseState returns the per-context syntax-case expansion state, or nil
// when not in a syntax-case expansion. The concrete type is owned by
// machine/compilation/; callers within that subpackage type-assert to
// *syntaxCaseState. See the comment on expansionState for why this is
// any-typed.
func (p *MachineContext) SyntaxCaseState() any {
	if p.expansion == nil {
		return nil
	}
	return p.expansion.syntaxCase
}

// SetSyntaxCaseState installs the syntax-case expansion state on the context,
// or nil to clear it. In production, the only legitimate concrete type is
// *compilation.syntaxCaseState; the constraint is enforced by encapsulation
// (unexported field, single-package consumer) rather than the type system.
// Lazy-allocates the expansion sub-record on first write.
func (p *MachineContext) SetSyntaxCaseState(v any) {
	if p.expansion == nil {
		if v == nil {
			return // common case: clearing a never-set state; no allocation
		}
		p.expansion = &expansionState{}
	}
	p.expansion.syntaxCase = v
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

// foreignBoundaryName labels the synthetic frame CaptureStackTrace emits where
// a Scheme→Go→Scheme call crosses a sub-context boundary (parentMC hop). It
// marks that the frames above and below it are separated by a Go primitive's
// call into Scheme — the breadcrumb that would otherwise be lost when a trace
// is taken inside a sub-context (apply's in-place path excepted, as it creates
// no sub-context).
const foreignBoundaryName = "<foreign-call boundary>"

// defaultBacktraceDepth bounds the number of frames CaptureStackTrace walks when
// building the backtrace attached to a user-facing Scheme error or exception.
// Deep enough to locate the fault, shallow enough to keep error messages
// readable; a presentation choice, not a correctness one.
const defaultBacktraceDepth = 20

// CaptureStackTrace walks the continuation chain and builds a stack trace.
//
// The walk spans sub-context boundaries via parentMC. When a trace is captured
// inside a sub-context — e.g. a parameter converter, eval/load, or an exception
// thunk that runs Scheme via NewSubContext — the inner frames alone would
// truncate at that sub-context's nil-rooted continuation chain. Hopping parentMC
// restores the full logical Scheme stack, inserting a foreignBoundaryName frame
// at each crossing.
//
// This deliberately diverges from SliceContinuationAt, which walks the same
// cont.parent chain for *control* capture and MUST stop at the boundary to keep
// a captured continuation re-runnable (it cannot reproduce the Go residual
// between the inner and outer frames). A stack trace is read-only — it
// re-executes nothing — so spanning the boundary is both safe and desirable.
//
// The walk stops at a context with isolatedMarks set (a re-invoked continuation
// running a grafted chain), mirroring findParameterInMarks: the trace then shows
// the continuation's own extent, not the invoker's.
func (p *MachineContext) CaptureStackTrace(maxDepth int) StackTrace {
	trace := make(StackTrace, 0, 16)
	depth := 0

	for mc := p; mc != nil; mc = mc.parentMC {
		// Synthetic boundary frame between a sub-context and the parent
		// context whose Go primitive bridged them. Never before the innermost
		// context.
		if mc != p {
			if depth >= maxDepth {
				break
			}
			trace = append(trace, StackFrame{FunctionName: foreignBoundaryName})
			depth++
		}

		// Live (current) frame of this context.
		if mc.template != nil && depth < maxDepth {
			trace = append(trace, StackFrame{
				FunctionName: mc.template.Name(),
				CurrentLoc:   mc.template.SourceAt(mc.pc),
			})
			depth++
		}

		// Saved frames. Continuation PCs are return addresses (one past the
		// call instruction), so pc-1 gives the call site source location.
		cont := mc.cont
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

		// Depth budget exhausted within this context's saved chain: report how
		// many frames remain in THIS context and stop. Frames in parent
		// contexts beyond the budget are not counted — this matches the
		// single-context truncation behavior the hop preserves.
		if cont != nil {
			remaining := countFrames(cont)
			if remaining > 0 {
				trace = append(trace, StackFrame{
					FunctionName: fmt.Sprintf("... %d more frames ...", remaining),
				})
			}
			return trace
		}

		// A re-invoked continuation running on a grafted chain must not cross
		// into the invoker's frames.
		if mc.isolatedMarks {
			break
		}
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

// Error creates a SchemeError with the current source location and stack
// trace but no sentinel cause, so errors.Is cannot match the result against
// any error family. Prefer WrapError(sentinel, msg) at any site whose failure
// has a sentinel — every production call site does (R9). Reach for Error only
// when no sentinel genuinely applies.
func (p *MachineContext) Error(msg string) *SchemeError {
	source := p.CurrentSource()
	trace := p.CaptureStackTrace(defaultBacktraceDepth)
	return NewSchemeError(msg, source, trace.String())
}

// WrapError wraps an existing error with the current source location and stack trace.
func (p *MachineContext) WrapError(err error, msg string) *SchemeError {
	source := p.CurrentSource()
	trace := p.CaptureStackTrace(defaultBacktraceDepth)
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

// checkStackSize is the entry point called at every push-opcode site.
// Returns nil immediately in the unlimited-stack default (maxStackSize == 0);
// otherwise delegates to reportStackOverflow. The split keeps this wrapper
// under Go's inline budget — see PR #636 and Finding 5 of
// plans/2026-05-06-machine-structural-reduction.md.
func (p *MachineContext) checkStackSize() error {
	if p.maxStackSize == 0 {
		return nil
	}
	return p.reportStackOverflow()
}

// reportStackOverflow returns ErrStackOverflow when the eval stack exceeds
// the configured maximum. Precondition: maxStackSize > 0. checkStackSize is
// the only legitimate caller; calling directly when maxStackSize == 0 silently
// returns nil and bypasses the intended fast path.
func (p *MachineContext) reportStackOverflow() error {
	if uint64(p.evals.Len()) > p.maxStackSize {
		// Fresh VM-internal mint (reached from OpPush et al. via checkStackSize) →
		// enrich with the push site's source + trace via WrapError.
		return p.WrapError(werr.ErrStackOverflow,
			fmt.Sprintf("eval stack size %d exceeds limit %d", p.evals.Len(), p.maxStackSize))
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
		if values.EqIdentity(p.marks[i].key, key) {
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
		if values.EqIdentity(e.key, key) {
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
			if values.EqIdentity(e.key, key) {
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
		if values.EqIdentity(p.marks[i].key, key) {
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

// RunResumable drives the VM loop under a DefaultPromptTag prompt, catching
// control bounces (ErrPromptAbort and ErrResumeContinuation — the trampoline
// resume signal) and looping. It is the single resume/abort driver shared by
// every DefaultPromptTag owner. It handles continuation escapes that weren't
// caught by an enclosing call/cc — used at the top level (REPL and file
// execution) to catch continuations invoked outside their original dynamic
// extent.
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
func (p *MachineContext) RunResumable() (rerr error) {
	p.promptTag = DefaultPromptTag // install default prompt for call/cc escapes

	// Contain every panic at this VM boundary so it surfaces to the embedder as
	// a returned error instead of unwinding into the host process or the REPL
	// ("embedding is the product"). A panic unwinds the whole goroutine stack,
	// so this single site also catches panics tripped inside nested sub-context
	// Run() calls.
	//
	// Two panic sources land here. VM-invariant guards (the Stack methods on
	// underflow, local-index overflow, edit-plan bounds, ...) panic with a
	// *werr.ForeignError; well-formed bytecode never trips them, so they signal
	// a compiler/VM bug. Anything else — runtime.Error (nil deref, index out of
	// range) or a non-error panic value — is likewise a Go-level bug, not a
	// user-recoverable Scheme condition. Both are wrapped as a *SchemeError (NOT
	// an *ErrExceptionEscape) so they remain uncatchable by guard /
	// with-exception-handler while staying observable to the Go caller. WrapError
	// attaches the VM source location and continuation-chain stack trace — both
	// read heap state that survives the panic unwind — and chains the original
	// error via SchemeError.Unwrap, so sentinel matches (errors.Is, e.g.
	// werr.ErrStackUnderflow) still resolve.
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		// A non-error panic value (panic("...") / panic(42)) carries its text under
		// an internal-error sentinel so it, too, stays within the VM boundary as a
		// matchable, wrapped error.
		err := werr.RecoverAsError(r, werr.ErrInternal, "")
		// Fold the recovered error's text into the message. SchemeError.Error()
		// renders Message + source + trace but NOT Cause, so without this the
		// printed error would read only "recovered panic" and the real detail
		// (e.g. a runtime.Error "index out of range" message) would be reachable
		// only via Unwrap. Cause stays set (WrapError chains it) for errors.Is/As.
		rerr = p.WrapError(err, "RunResumable: recovered panic: "+err.Error())
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
				// Fresh mint (a user abort to a missing tag) → enrich with source +
				// trace via WrapError, not bare werr.
				return p.WrapError(werr.ErrInvalidArgument,
					fmt.Sprintf("abort-current-continuation: no prompt found for tag %s", abortErr.Tag.SchemeString()))
			}

			// Reconcile winding, restore past the prompt, and run its handler or
			// deliver the abort values — the shared abort arm (see resolveAbort).
			done, abErr := p.resolveAbort(abortErr, prompt)
			if abErr != nil {
				return abErr
			}
			if done {
				// Context-level deliver (prompt == nil): the composable continuation
				// already ran to completion in the escape closure's sub-context; the
				// abort value is the final result and p.pc was not advanced.
				return nil
			}
			continue
		}

		var resumeErr *ErrResumeContinuation
		if errors.As(err, &resumeErr) {
			// The trampoline: a call/cc continuation was invoked (applyCapturedContinuation
			// returned this signal instead of nesting a sub.Run()). Reinstall its captured
			// segment at the nearest prompt with the continuation's tag (DefaultPromptTag)
			// on THIS driver's live chain, then keep looping so the resumed chain runs on
			// the driver itself: O(1) Go frames (no ctak overflow), and a SINGLE winding
			// reconcile from the source winding at the (k v) site (no double-fire; deeper-
			// sub after-thunks fire once). The boundary is the matching prompt's frame, or
			// nil at the top-level context boundary: nil REPLACES the whole chain (the
			// escape-past case — a full k discards the chain-resident consumer/exit/prompt
			// frames), while an inner call-with-continuation-prompt frame receives the
			// delimited segment's result and the chain above it is discarded.
			boundary, _ := p.FindPrompt(resumeErr.Tag)
			// isolate=true: this signal is emitted only for call/cc resume, which restores
			// the captured mark snapshot (composable resume composes marks and bypasses
			// this signal — it calls ReinstallSegment directly).
			wasEmpty, reErr := p.ReinstallSegment(
				resumeErr.Segment, boundary, resumeErr.SourceWinding, resumeErr.Values, true)
			if reErr != nil {
				return reErr
			}
			if wasEmpty {
				// Empty captured continuation (the call/cc sat in tail position, e.g.
				// captured inside a force/delay thunk): ReinstallSegment delivered the
				// values and reconciled winding but installed no frames. At the top-level
				// context boundary the delivered values are the final result. At a
				// delimited inner prompt the values must flow THROUGH it: restore the
				// prompt frame so the (transparent) prompt delivers them to its parent
				// (e.g. an outer call-with-continuation-prompt → the rest of the program).
				if boundary == nil {
					return nil
				}
				p.Restore(boundary)
			}
			continue
		}

		var timerErr *ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			boundary, found := p.FindPrompt(timerErr.Tag)
			if !found {
				// Every with-timeout pushes its finalizer frame before the thunk runs, so
				// a fired timer reaching the top driver MUST have a boundary on the chain.
				return werr.WrapForeignErrorf(werr.ErrInternal,
					"with-timeout: no boundary frame found for fired timer")
			}
			applyErr := p.resolveTimerInterrupt(timerErr, boundary)
			if applyErr != nil {
				return applyErr
			}
			continue
		}

		return err
	}
}

// RunWithEscapeHandling is the top-level entry point that runs the VM under the
// DefaultPromptTag prompt. It delegates to RunResumable, the shared abort/resume
// driver every DefaultPromptTag owner routes through.
func (p *MachineContext) RunWithEscapeHandling() error {
	return p.RunResumable()
}

// RunWithinBoundary drives this sub-context like Run, but resolves an
// ErrPromptAbort whose tag names a prompt ON THIS CONTEXT'S OWN CHAIN inline — the
// way RunResumable's abort arm does at the top level — instead of letting it escape
// to a Go-stack errors.As catch in the calling primitive.
//
// It exists because the continuation cluster reifies call-with-exit /
// call-with-continuation-prompt as continuation-CHAIN prompt frames run inline. A
// reified boundary that appears INSIDE a surviving sub-context (a
// with-continuation-barrier thunk, a RaiseInPlace handler, a dynamic-wind thunk, a
// parameter converter, ...) lands its prompt frame on THAT sub-context's chain.
// When it aborts, FindPrompt(tag) on the sub's own chain finds the frame and this
// driver routes the abort here; without it the abort escapes to the top
// RunResumable, whose FindPrompt walks the PARENT chain and cannot see the frame
// ("no prompt found for tag").
//
// It is a strict superset of Run for any NewSubContext: such a context has
// promptTag == nil (NewSubContext does not inherit it), so FindPrompt only matches
// a prompt FRAME pushed inside the sub by a reified boundary. An abort whose tag is
// NOT on this chain — a full call/cc to DefaultPromptTag, or an exit targeting an
// OUTER boundary — is re-raised unchanged so the enclosing driver owns it; and
// ErrResumeContinuation (the call/cc trampoline signal) is never an ErrPromptAbort,
// so it always re-raises to the top RunResumable. No DefaultPromptTag install, no
// timer or panic handling: those belong to the one top-level RunResumable.
func (p *MachineContext) RunWithinBoundary() error {
	for {
		err := p.Run()
		if err == nil {
			return nil
		}

		var abortErr *ErrPromptAbort
		if !errors.As(err, &abortErr) {
			// A timer whose with-timeout boundary is on THIS sub-context's own chain is
			// resolved here — the symmetric counterpart to resolving an abort to a local
			// prompt — so a with-timeout that sits inside a surviving sub-context still has
			// a local driver. A timer for an OUTER with-timeout (boundary not on this
			// chain) re-raises to the enclosing driver, exactly like a not-found abort.
			var timerErr *ErrTimerInterrupt
			if errors.As(err, &timerErr) {
				boundary, found := p.FindPrompt(timerErr.Tag)
				if found {
					applyErr := p.resolveTimerInterrupt(timerErr, boundary)
					if applyErr != nil {
						return applyErr
					}
					continue
				}
			}
			// ErrResumeContinuation, an outer timer, an exception escape, or a real error:
			// the enclosing driver handles it.
			return err
		}

		prompt, found := p.FindPrompt(abortErr.Tag)
		if !found {
			// The abort targets a boundary OUTSIDE this sub-context; re-raise so the
			// enclosing RunWithinBoundary / RunResumable routes it.
			return err
		}

		// Reconcile winding, restore past the prompt, and run its handler or deliver
		// the abort values — the shared abort arm (see resolveAbort). For a NewSubContext
		// (promptTag nil) the found prompt is always a chain FRAME; the prompt == nil
		// arms inside resolveAbort are kept for parity with RunResumable. SourceWinding
		// (if carried) is the winding at the escape point, possibly deeper than this sub.
		done, abErr := p.resolveAbort(abortErr, prompt)
		if abErr != nil {
			return abErr
		}
		if done {
			return nil
		}
		continue
	}
}

// resolveAbort reconciles dynamic-wind from the escape-point winding to the FOUND
// prompt's, restores past the prompt frame, and runs the prompt's handler or delivers
// the abort values. It is the shared abort arm of RunResumable and RunWithinBoundary —
// the two drivers differ only in how they treat a NOT-FOUND prompt (the top-level
// RunResumable errors; a RunWithinBoundary sub-context re-raises to its enclosing
// driver), so that decision stays at each call site and resolveAbort is reached only
// with a prompt FindPrompt already matched. prompt may still be nil: the context-level
// boundary, which has no frame and no handler.
//
// Returns done=true when the driver should return nil (a context-level deliver: the
// resumed computation already ran to completion, so the abort value is the final
// result), done=false to keep looping, and a non-nil error to propagate. SourceWinding,
// when the abort carried it, is the winding at the escape point — possibly a deeper
// sub-context than this driver's own (the C3 deeper-sub after-thunk reconcile);
// otherwise the driver's own winding is used (value-delivery aborts).
func (p *MachineContext) resolveAbort(abortErr *ErrPromptAbort, prompt *MachineContinuation) (done bool, err error) {
	var targetStack WindingStack
	if prompt != nil {
		targetStack = prompt.windingStack
	}
	sourceStack := p.windingStack
	if abortErr.SourceWinding != nil {
		sourceStack = abortErr.SourceWinding
	}
	restoreErr := p.RestoreWithWindingFrom(nil, sourceStack, targetStack)
	if restoreErr != nil {
		return false, restoreErr
	}

	// Restore past the prompt frame (skip it). A nil prompt is the context-level
	// boundary — no frame to restore.
	if prompt != nil {
		p.Restore(prompt)
	}

	// Run the handler, or deliver ALL abort values (R7RS §6.10, including zero — a
	// continuation invoked with no values resumes with no values, not a fabricated
	// Void). Context-level prompts (nil) have no handler.
	if prompt != nil && prompt.PromptHandler() != nil {
		_, applyErr := p.ApplyCallable(prompt.PromptHandler(), abortErr.Values...)
		if applyErr != nil {
			return false, applyErr
		}
		return false, nil
	}
	p.SetValues(abortErr.Values...)
	return prompt == nil, nil
}

// resolveTimerInterrupt is the shared timer arm of RunResumable and RunWithinBoundary
// (mirroring resolveAbort). The two drivers differ only in how they treat a NOT-FOUND
// boundary — the top-level RunResumable errors (a fired timer must have a frame on the
// chain), a sub-context re-raises so the enclosing driver owns it — so each call site
// does its own FindPrompt and reaches this helper only with boundary (the with-timeout
// finalizer frame) already matched.
//
// It (1) captures the suspended thunk DELIMITED at boundary as a composable continuation
// the handler may resume; (2) HARD-SUSPENDS — restores past the abandoned thunk to the
// boundary WITHOUT unwinding, so dynamic-wind after-thunks inside the timed-out thunk do
// NOT run (Wile's deliberate timeout semantics — see prim_timer_test TestWithTimeoutDynamicWind
// case 2); (3) tears the fired timer down to the OUTER timer/ctx (the same absolute
// restore the finalizer frame re-does idempotently on the handler's return), so the
// handler runs under the surrounding context, not the Done timerCtx; (4) runs the handler
// with the resumable. Because p.cont is the finalizer frame, the handler returns INTO it,
// and the finalizer forwards the handler's value past the boundary.
func (p *MachineContext) resolveTimerInterrupt(timerErr *ErrTimerInterrupt, boundary *MachineContinuation) error {
	segment := p.CaptureInterruptContinuationAt(boundary)
	windingCopy := p.windingStack.Copy()
	resumable := NewComposableContinuation(segment, windingCopy, p.threadID, p.barrierValid)

	// Hard-suspension: discard the timed-out thunk's frames un-run (no after-thunks) and
	// land on the finalizer frame. boundary.windingStack is the winding captured at
	// with-timeout entry (RunBodyUnderFrame), so the thunk's winding frames are dropped.
	// The thunk's mid-expression operand stack is already deep-copied into `segment`, so
	// the dirty eval stack is released and a fresh one acquired for the handler.
	p.cont = boundary
	p.windingStack = boundary.windingStack
	oldEvals := p.evals
	p.evals = p.acquireStack()
	p.releaseStack(oldEvals)

	// Absolute teardown of the fired timer: cancel it and restore the outer timer + ctx.
	// p.timer is the innermost (fired) timer — the emit site read mc.timer, and nothing
	// changed it between emit and here. The finalizer frame re-does this identically.
	fired := p.timer
	if fired != nil {
		fired.cancel()
		p.timer = fired.parent
		p.ctx = fired.savedCtx
	}

	_, applyErr := p.ApplyCallable(timerErr.Handler, resumable)
	return applyErr
}
