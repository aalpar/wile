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
)

// errHalt is the internal VM sentinel returned by OperationRestoreContinuation
// when mc.cont == nil (i.e., no more frames to pop — execution is complete).
// Run() catches this and returns nil, so callers never see it.
var errHalt = values.NewStaticError("machine halt: no more operations to run")

// contextCheckMask gates how often the VM loop checks ctx.Done().
// Amortized batch checking: a non-blocking select is cheap (~15ns) but
// not free; checking every 1024 ops eliminates ~99.9% of them while
// keeping worst-case cancellation latency under 1ms at typical throughput.
// Power of 2 so the check is a single AND instruction.
// See BIBLIOGRAPHY.md "Amortized Batch Checking".
const contextCheckMask = 1023

var ErrMachineDoNotAdvancePC = values.NewStaticError("machine do not advance PC: operation did not advance program counter")

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

// ErrContinuationEscape is used to signal that a continuation was invoked from within
// a sub-context. This allows the escape to propagate up through nested foreign function calls.
type ErrContinuationEscape struct {
	Continuation *MachineContinuation // Where to resume (inside sub-context for call/cc in dynamic-wind)
	Value        values.Value
	Handled      bool                 // Set to true after the escape has been handled and mc has been restored
	WindingStack WindingStack         // Target winding stack for proper dynamic-wind handling
	EscapeCont   *MachineContinuation // Outer continuation to restore after Continuation completes (for sub-context escapes)
}

func (p *ErrContinuationEscape) Error() string {
	return "continuation escape"
}

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
	pendingEscape    *MachineContinuation // continuation to restore after current execution completes (for sub-context escapes)
	escapeCont       *MachineContinuation // escape continuation for sub-contexts: where to continue after sub-context completes
	counters         VMCounters           // performance counters (plain uint64, single-goroutine)
	thread           *values.Thread       // SRFI-18 thread object: nil = primordial thread
	syntaxCase       *syntaxCaseState     // per-context syntax-case expansion state; nil when not in syntax-case
	maxCallDepth     uint64               // 0 = unlimited (default), otherwise max continuation depth
}

// NewMachineContext creates a new machine context with the given context and continuation.
// The context enables cancellation/timeout support in the VM loop.
// For callers that don't need cancellation, pass context.Background().
func NewMachineContext(ctx context.Context, cont *MachineContinuation) *MachineContext {
	q := &MachineContext{
		ctx: ctx,
		vmState: vmState{
			env:         cont.env,         // cannot copy environment here, it will be copied when pushed onto the stack
			template:    cont.template,    // not needed to copy, templates are immutable
			singleValue: cont.singleValue, // must not copy the values, they are passed between contexts
			multiValues: cont.multiValues,
			evals:       cont.evals, // no copy needed: continuation is consumed once at context creation
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

func (p *MachineContext) Restore(cont *MachineContinuation) {
	p.counters.ContinuationsRestored++
	p.counters.StackPoolReleases++
	old := p.evals
	p.env = cont.env
	p.template = cont.template
	// Must copy evals to avoid corrupting the continuation's saved stack.
	// Without copying, modifications to p.evals after restoration would mutate
	// cont.evals, breaking re-invocation of the continuation.
	p.evals = cont.evals.Copy()
	p.cont = cont.parent
	p.pc = cont.pc
	// Restore callDepth from the continuation's cached value.
	// Each continuation stores its ancestor count at creation time, which
	// equals the chain length of cont.parent (which is now p.cont).
	// This replaces an O(d) chain walk with an O(1) field read.
	p.callDepth = cont.callDepth
	// Return the old evals stack to the pool. It was allocated by a prior
	// SaveContinuation and is now dead (replaced by the continuation's copy).
	releaseStack(old)
}

// RestoreAndRelease is the fast path for normal function return. It transfers
// the continuation's state into the MachineContext (like Restore) but avoids
// copying evals — instead it transfers ownership directly and pools the
// consumed frame. This is safe because normal return consumes the frame
// exactly once; call/cc and escape paths must use Restore (which copies).
//
// The sequence is:
//  1. Release mc's current evals to the stack pool (it's dead after restore)
//  2. Transfer cont's evals directly to mc (no copy)
//  3. Nil cont.evals so releaseContinuation won't double-release it
//  4. Pool the consumed continuation frame
func (p *MachineContext) RestoreAndRelease(cont *MachineContinuation) {
	p.counters.ContinuationsRestored++
	p.counters.StackPoolReleases++
	p.counters.ContinuationPoolReleases++

	old := p.evals
	p.env = cont.env
	p.template = cont.template
	p.evals = cont.evals // transfer, not copy
	p.cont = cont.parent
	p.pc = cont.pc
	p.callDepth = cont.callDepth

	releaseStack(old)

	// Break the evals reference before pooling so the transferred stack
	// (now p.evals) is not released again inside releaseContinuation.
	cont.evals = nil
	releaseContinuation(cont)
}

// PopContinuation pops the current continuation from the machine context and returns it.
// It restores the machine context to the state saved in the popped continuation.
//
// Note: Unlike Restore(), we do NOT copy evals here because PopContinuation is used
// for normal function return where the continuation is consumed once. Restore() is
// used for continuation re-entry (call/cc) where the same continuation may be invoked
// multiple times, requiring the copy to prevent stack corruption.
func (p *MachineContext) PopContinuation() *MachineContinuation {
	if p.callDepth > 0 {
		p.callDepth--
	}
	q := p.cont
	p.template = q.template
	p.env = q.env
	p.evals = q.evals
	p.cont = q.parent
	p.pc = q.pc
	p.singleValue = q.singleValue
	p.multiValues = q.multiValues
	return q
}

// SaveContinuation pushes a new continuation onto the machine context with the given offset to the current program counter.
// Returns ErrCallDepthExceeded if the call depth limit has been reached.
//
// Note: callDepth is incremented BEFORE calling NewMachineContinuationFromMachineContext.
// The continuation's own callDepth is derived from mc.cont (the parent pointer), not from
// mc.callDepth, so this pre-increment does not affect the continuation's cached depth.
// See the comment on NewMachineContinuationFromMachineContext for why this matters.
func (p *MachineContext) SaveContinuation(off int) error {
	p.callDepth++
	if p.maxCallDepth > 0 && p.callDepth > p.maxCallDepth {
		p.callDepth--
		return values.WrapForeignErrorf(values.ErrCallDepthExceeded,
			"call depth %d exceeds limit %d", p.callDepth+1, p.maxCallDepth)
	}
	p.counters.ContinuationsSaved++
	p.cont = NewMachineContinuationFromMachineContext(p, off)
	p.evals = acquireStack()
	return nil
}

func (p *MachineContext) CurrentContinuation() *MachineContinuation {
	q := p.cont.DeepCopy()
	return q
}

// CallDepth returns the depth of the current continuation stack.
func (p *MachineContext) CallDepth() int {
	return int(p.callDepth)
}

func (p *MachineContext) Apply(mcls *MachineClosure, vs ...values.Value) (*MachineContext, error) {
	tpl := mcls.Template()
	l := tpl.ParameterCount()

	// Check arity before copying environment (fast-fail path).
	// Wrong-arity calls are common enough (dynamic typing, variadic dispatch)
	// that avoiding the copy overhead is worthwhile.
	if !tpl.IsVariadic() {
		if len(vs) != l {
			return nil, values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "expected %d arguments, got %d", l, len(vs))
		}
	} else {
		if len(vs) < l-1 {
			return nil, values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "expected at least %d arguments, got %d", l-1, len(vs))
		}
	}

	// Create a fresh copy of the local environment for this call.
	// This is critical for recursive functions: without copying, all invocations
	// share the same bindings, causing parameter corruption when evaluating
	// arguments like (+ (f (- n 1)) (f (- n 2))).
	localEnv := mcls.env.LocalEnvironment().CopyForApply()
	env := environment.NewEnvironmentFrameWithParent(localEnv, mcls.env.Parent())
	bnds := localEnv.Bindings()
	p.counters.ClosuresApplied++
	p.counters.EnvsCopied++
	p.counters.BindingsCopied += uint64(len(bnds))
	p.counters.KeysShared++

	if !tpl.IsVariadic() {
		for i := range bnds[:l] {
			bnds[i].SetValue(vs[i])
		}
	} else {
		for i := range bnds[:l-1] {
			bnds[i].SetValue(vs[i])
		}
		bnds[l-1].SetValue(values.List(vs[l-1:]...))
	}

	p.template = tpl
	p.env = env
	p.pc = 0
	return p, nil
}

// ApplyCaseLambda applies a case-lambda closure by finding the matching clause.
func (p *MachineContext) ApplyCaseLambda(clcls *CaseLambdaClosure, vs ...values.Value) (*MachineContext, error) {
	mcls, ok := clcls.FindMatchingClause(len(vs))
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "no matching clause in case-lambda for %d arguments", len(vs))
	}
	return p.Apply(mcls, vs...)
}

// ApplyCallable dispatches a procedure call to the appropriate handler based
// on the callee's concrete type. This is the unified entry point for all
// Scheme procedure application, symmetric with OperationApply.
//
// Supported callable types:
//   - *MachineClosure: standard Scheme lambda
//   - *CaseLambdaClosure: R7RS case-lambda (§4.2.9)
//   - *Parameter: R7RS parameter object (§4.2.6)
//   - *ComposableContinuation: delimited continuation
//
// Precondition: p.ctx must be set (always true for contexts created via
// NewMachineContext or NewSubContext).
func (p *MachineContext) ApplyCallable(callable values.Value, args ...values.Value) (*MachineContext, error) {
	if callable == nil {
		return p, values.WrapForeignErrorf(values.ErrNotAProcedure,
			"application: cannot apply nil value")
	}
	switch cls := callable.(type) {
	case *MachineClosure:
		return p.Apply(cls, args...)
	case *CaseLambdaClosure:
		return p.ApplyCaseLambda(cls, args...)
	case *Parameter:
		return p.applyParameter(cls, args)
	case *ComposableContinuation:
		return p.applyComposableContinuation(cls, args)
	default:
		return p, values.WrapForeignErrorf(values.ErrNotAProcedure,
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
			_, err := sub.Apply(converter, newVal)
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
// its captured frames onto the current continuation chain. The continuation
// is deep-copied for safe re-invocation.
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
		return p, values.WrapForeignErrorf(values.ErrCrossThreadContinuation,
			"composable continuation: captured in thread %d, invoked from thread %d",
			cc.threadID, p.threadID)
	}

	// Deep-copy the segment for safe re-invocation
	segment := cc.Cont().DeepCopy()

	// Graft the segment's bottom frame onto the current continuation chain
	GraftContinuation(segment, p.cont)

	// Handle dynamic-wind: unwind current extents not in captured stack,
	// rewind captured extents not in current stack.
	err := p.RestoreWithWindingFrom(nil, p.windingStack, cc.WindingStack())
	if err != nil {
		return p, err
	}

	// Restore from the top of the segment (resume captured computation)
	p.Restore(segment)
	p.SetValue(args[0])
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
// Dispatch mode: If the current template has integer-dispatch bytecode
// (template.code is populated), Run uses the switch-dispatch loop. Otherwise
// it falls back to the interface-dispatch loop (Operation.Apply calls).
// During migration, a template has one or the other, never both.
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
				return values.WrapForeignErrorf(values.ErrNilParentEnvironment,
					"PopEnv: cannot pop top-level environment")
			}
			mc.env = parent
			mc.pc++

		case OpApply:
			vs := mc.evals.PopAll()
			mc.counters.StackPopAlls++
			mc.counters.StackElementsCopied += uint64(len(vs))
			result, err := mc.ApplyCallable(mc.GetValue(), vs...)
			if err != nil {
				return mc.WrapError(err, "")
			}
			mc = result

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
			o := mc.template.literals[instr.Arg]
			if o == nil {
				return mc.Error(fmt.Sprintf("literal index %v does not exist", instr.Arg))
			}
			gi, ok := o.(*environment.GlobalIndex)
			if !ok {
				return mc.Error(fmt.Sprintf("literal %v is not a global index", o))
			}
			var bd *environment.Binding
			if gi.Env != nil {
				bd = gi.Env.GetOwnGlobalBinding(gi)
			} else {
				bd = mc.env.GetGlobalBinding(gi)
			}
			if bd == nil {
				return mc.Error(fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
			}
			mc.SetValue(bd.Value())
			mc.pc++

		case OpStoreGlobal:
			o := mc.template.literals[instr.Arg]
			if o == nil {
				return mc.Error(fmt.Sprintf("literal index %v does not exist", instr.Arg))
			}
			gi, ok := o.(*environment.GlobalIndex)
			if !ok {
				return mc.Error(fmt.Sprintf("literal %v is not a global index", o))
			}
			val := mc.evals.Pop()
			var err error
			if gi.Env != nil {
				err = gi.Env.SetOwnGlobalValue(gi, val)
			} else {
				err = mc.env.GlobalEnvironment().SetOwnGlobalValue(gi, val)
			}
			if err != nil {
				return mc.WrapError(err, fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
			}
			mc.pc++

		case OpPeekK:
			mc.SetValue(mc.evals.PeekK(int(instr.Arg)))
			mc.pc++

		// --- Wave 3: two-operand operations (bit-packed slot|depth) ---

		case OpLoadLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			li := environment.NewLocalIndex(slot, depth)
			bd := mc.env.GetLocalBinding(li)
			if bd == nil {
				return mc.Error(fmt.Sprintf("no such local binding %s", li))
			}
			mc.SetValue(bd.Value())
			mc.pc++

		case OpStoreLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			li := environment.NewLocalIndex(slot, depth)
			err := mc.env.SetLocalValue(li, mc.evals.Pop())
			if err != nil {
				return mc.WrapError(err, "")
			}
			mc.pc++

		// --- Fallback: complex operations via side table ---

		case OpComplex:
			var err error
			mc, err = mc.template.sideTable[instr.Arg].Apply(mc.ctx, mc)
			if err != nil {
				if errors.Is(err, errHalt) {
					return nil
				}
				return err
			}

		default:
			return values.WrapForeignErrorf(values.ErrUnknownOpCode,
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
	mc.env = p.env.TopLevel()
	mc.evals = acquireStack()
	mc.threadID = p.threadID
	mc.parentMC = p
	mc.escapeCont = p.escapeCont
	mc.thread = p.thread
	mc.exceptionHandler = p.exceptionHandler
	mc.maxCallDepth = p.maxCallDepth
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
func (p *MachineContext) PushExceptionHandler(handler values.Value) {
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

// WindingStack returns the current winding stack.
func (p *MachineContext) WindingStack() WindingStack {
	return p.windingStack
}

// SetWindingStack sets the winding stack (used by sub-contexts).
func (p *MachineContext) SetWindingStack(stack WindingStack) {
	p.windingStack = stack
}

// PushWindingFrame adds a frame to the winding stack.
func (p *MachineContext) PushWindingFrame(frame *DynamicWindFrame) {
	p.windingStack.Push(frame)
}

// PopWindingFrame removes the innermost frame from the winding stack.
func (p *MachineContext) PopWindingFrame() *DynamicWindFrame {
	return p.windingStack.Pop()
}

// UnwindTo runs after thunks from innermost to the common ancestor.
// Returns error if any after thunk fails.
func (p *MachineContext) UnwindTo(commonDepth int) error {
	return p.unwindStackTo(p.windingStack, commonDepth)
}

// unwindStackTo runs after thunks from innermost to commonDepth on the given stack,
// then sets p.windingStack to the common ancestor prefix.
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
	// Run after thunks from innermost to outermost (reverse order)
	for i := len(stack) - 1; i >= commonDepth; i-- {
		frame := stack[i]
		if frame.After != nil {
			sub := p.NewSubContext()
			sub.windingStack = stack[:i:i] // Set stack to this level (cap to prevent aliasing)
			_, err := sub.Apply(frame.After)
			if err != nil {
				ReleaseSubContext(sub)
				return err
			}
			err = sub.Run()
			ReleaseSubContext(sub)
			if err != nil {
				// Propagate escapes and exceptions
				return err
			}
		}
	}
	// Update current winding stack to common ancestor
	p.windingStack = stack[:commonDepth:commonDepth]
	return nil
}

// RewindTo runs before thunks from common ancestor to target depth.
// Returns error if any before thunk fails.
func (p *MachineContext) RewindTo(target WindingStack, commonDepth int) error {
	// Run before thunks from outermost to innermost (forward order)
	for i := commonDepth; i < len(target); i++ {
		frame := target[i]
		if frame.Before != nil {
			sub := p.NewSubContext()
			sub.windingStack = p.windingStack // Current stack at this point
			_, err := sub.Apply(frame.Before)
			if err != nil {
				ReleaseSubContext(sub)
				return err
			}
			err = sub.Run()
			ReleaseSubContext(sub)
			if err != nil {
				return err
			}
		}
		// Add this frame to current winding stack
		p.windingStack = append(p.windingStack, frame)
	}
	return nil
}

// RestoreWithWinding restores a continuation with proper dynamic-wind handling.
// It unwinds from the current dynamic extent, rewinds to the target extent,
// then restores the machine state.
//
// If cont is nil (continuation captured in a sub-context), we still perform
// the winding operations but don't restore machine state - the caller should
// handle continued execution appropriately.
func (p *MachineContext) RestoreWithWinding(cont *MachineContinuation, targetStack WindingStack) error {
	return p.RestoreWithWindingFrom(cont, p.windingStack, targetStack)
}

// RestoreWithWindingFrom restores a continuation with proper dynamic-wind handling,
// using an explicit source winding stack instead of the current context's stack.
//
// This is needed when the escape originated from a sub-context that has a different
// winding stack than the context where RestoreWithWinding is called. For example,
// when call/cc captures inside a sub-context and the escape propagates up, the
// source winding stack (where the escape happened) may have frames that the
// top-level context doesn't know about.
//
// Parameters:
//   - cont: The continuation to restore to
//   - sourceStack: The winding stack where the escape originated (for unwinding)
//   - targetStack: The winding stack to restore to (for rewinding)
func (p *MachineContext) RestoreWithWindingFrom(cont *MachineContinuation, sourceStack, targetStack WindingStack) error {
	// Find common ancestor between source and target
	commonDepth := FindCommonWindingPrefix(sourceStack, targetStack)

	// Unwind: run after thunks for frames being exited (from source)
	err := p.unwindStackTo(sourceStack, commonDepth)
	if err != nil {
		return err
	}

	// Rewind: run before thunks for frames being entered (to target)
	err = p.RewindTo(targetStack, commonDepth)
	if err != nil {
		return err
	}

	// Restore the machine state (if we have a valid continuation).
	// DeepCopy is required for two reasons:
	//  1. Restore copies evals but shares the parent chain. Without deep-copying,
	//     subsequent re-invocations of the same continuation would share parent
	//     frames that RestoreAndRelease may have already pooled.
	//  2. Each invocation gets a fully independent disposable chain that can be
	//     safely consumed by the normal-return pooling path.
	if cont != nil {
		p.Restore(cont.DeepCopy())
	}
	return nil
}

// FindPrompt walks the continuation chain to find the nearest frame with
// a matching prompt tag. Also checks the context's own prompt tag (set by
// call-with-continuation-prompt on sub-contexts). Returns the matching
// frame and true, or nil and false.
//
// When the prompt is on the context itself (not a continuation frame), returns
// nil and true — the caller should treat this as "prompt at the boundary of
// this sub-context" and slice the entire continuation chain.
func (p *MachineContext) FindPrompt(tag *PromptTag) (*MachineContinuation, bool) {
	for frame := p.cont; frame != nil; frame = frame.parent {
		if frame.promptTag == tag {
			return frame, true
		}
	}
	if p.promptTag == tag {
		return nil, true
	}
	return nil, false
}

// SliceContinuationAt deep-copies the continuation chain segment from p.cont
// down to (but not including) the prompt frame. The returned chain's bottom
// frame has parent = nil, making it a standalone segment suitable for
// composable continuation capture.
func (p *MachineContext) SliceContinuationAt(prompt *MachineContinuation) *MachineContinuation {
	if p.cont == nil || p.cont == prompt {
		return nil
	}
	// Deep copy frames from p.cont to just before prompt
	top := p.cont.Copy()
	top.parent = nil
	current := top
	src := p.cont.parent
	for src != nil && src != prompt {
		frameCopy := src.Copy()
		frameCopy.parent = nil
		current.parent = frameCopy
		current = frameCopy
		src = src.parent
	}
	return top
}

// GraftContinuation walks the segment chain to its bottom frame and sets
// its parent to target, effectively splicing the segment onto the target chain.
func GraftContinuation(segment, target *MachineContinuation) {
	if segment == nil {
		return
	}
	current := segment
	for current.parent != nil {
		current = current.parent
	}
	current.parent = target
}

// SetPromptTag sets the prompt tag on this context. Used by
// call-with-continuation-prompt to mark sub-contexts as prompt boundaries.
func (p *MachineContext) SetPromptTag(tag *PromptTag) {
	p.promptTag = tag
}

// PromptTag returns the prompt tag for this context, or nil.
func (p *MachineContext) PromptTag() *PromptTag {
	return p.promptTag
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

			// If there's a pending escape continuation (from a sub-context escape),
			// restore to it and continue execution
			if p.pendingEscape != nil {
				escapeCont := p.pendingEscape
				p.pendingEscape = nil
				p.Restore(escapeCont.DeepCopy())
				continue
			}

			return nil
		}

		var escapeErr *ErrContinuationEscape
		if errors.As(err, &escapeErr) && !escapeErr.Handled {
			if escapeErr.Continuation == nil {
				// Continuation was nil (truly truncated) - can't recover
				return err
			}

			// Use the current winding stack as the source for unwinding.
			// Restore with proper winding handling
			restoreErr := p.RestoreWithWindingFrom(escapeErr.Continuation, p.windingStack, escapeErr.WindingStack)
			if restoreErr != nil {
				return restoreErr
			}
			p.SetValue(escapeErr.Value)

			// If there's an escape continuation (for sub-context escapes), save it
			// so we can restore to it after the inner execution completes
			if escapeErr.EscapeCont != nil {
				p.pendingEscape = escapeErr.EscapeCont
			}

			continue
		}

		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			prompt, found := p.FindPrompt(abortErr.Tag)
			if !found {
				return values.WrapForeignErrorf(values.ErrInvalidArgument, "abort-current-continuation: no prompt found for tag %s", abortErr.Tag.SchemeString())
			}

			// Unwind dynamic-wind from current to prompt's winding depth.
			// The prompt frame's winding stack captures the extent at prompt installation.
			targetStack := prompt.windingStack
			restoreErr := p.RestoreWithWindingFrom(nil, p.windingStack, targetStack)
			if restoreErr != nil {
				return restoreErr
			}

			// Restore to the prompt frame (skip past it)
			p.Restore(prompt)

			// Invoke the handler with the abort values
			handler := prompt.PromptHandler()
			if handler != nil {
				_, applyErr := p.Apply(handler, abortErr.Values...)
				if applyErr != nil {
					return applyErr
				}
				// Compensate: Apply sets pc=0, but Run() will start from pc.
				// No compensation needed since we just Apply'd fresh.
			} else {
				if len(abortErr.Values) > 0 {
					p.SetValue(abortErr.Values[0])
				} else {
					p.SetValue(values.Void)
				}
			}
			continue
		}

		return err
	}
}
