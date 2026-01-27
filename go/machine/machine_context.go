// Copyright 2025 Aaron Alpar
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

	"wile/environment"
	"wile/syntax"
	"wile/values"
)

var (
	ErrMachineHalt           = values.NewStaticError("machine halt: no more operations to run")
	ErrMachineDoNotAdvancePC = values.NewStaticError("machine do not advance PC: operation did not advance program counter")
)

// ErrContinuationEscape is used to signal that a continuation was invoked from within
// a sub-context. This allows the escape to propagate up through nested foreign function calls.
type ErrContinuationEscape struct {
	Continuation *MachineContinuation // Where to resume (inside sub-context for call/cc in dynamic-wind)
	Value        values.Value
	Handled      bool         // Set to true after the escape has been handled and mc has been restored
	WindingStack WindingStack // Target winding stack for proper dynamic-wind handling
	EscapeCont   *MachineContinuation // Outer continuation to restore after Continuation completes (for sub-context escapes)
}

func (e *ErrContinuationEscape) Error() string {
	return "continuation escape"
}

// MachineContext represents the execution context of a virtual machine.
// It holds the current environment, values, evaluation stack, continuation, and program counter.
// It is created from a MachineContinuation and can be modified during execution.
type MachineContext struct {
	ctx              context.Context
	env              *environment.EnvironmentFrame
	value            MultipleValues
	evals            *Stack               // evaluation stack, holds intermediate values during execution
	cont             *MachineContinuation // current continuation
	template         *NativeTemplate
	pc               int
	expanderCtx      *ExpanderContext  // set during macro transformer execution for syntax-local-* access
	exceptionHandler *ExceptionHandler // current exception handler chain for R7RS exceptions
	debugger         *Debugger         // optional debugger for breakpoints and stepping
	windingStack     WindingStack      // R7RS dynamic-wind extent tracking
	parentMC         *MachineContext   // parent context for sub-contexts, enables call/cc escape tracking
	pendingEscape    *MachineContinuation // continuation to restore after current execution completes (for sub-context escapes)
	escapeCont       *MachineContinuation // escape continuation for sub-contexts: where to continue after sub-context completes
}

// NewMachineContext creates a new machine context with the given context and continuation.
// The context enables cancellation/timeout support in the VM loop.
// For callers that don't need cancellation, pass context.Background().
func NewMachineContext(ctx context.Context, cont *MachineContinuation) *MachineContext {
	q := &MachineContext{
		ctx:      ctx,
		env:      cont.env,      // cannot copy environment here, it will be copied when pushed onto the stack
		template: cont.template, // not needed to copy, templates are immutable
		cont:     cont.parent,
		value:    cont.value, // must not copy the values, they are passed between contexts
		evals:    cont.evals, // must copy the eval stack
		pc:       cont.pc,
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

// FindEscapeContinuation walks up the parent context chain to find a continuation
// that can be used for escaping from a sub-context. This is used by call/cc to
// capture proper continuations when invoked inside sub-contexts (like dynamic-wind thunks).
//
// Returns the first non-nil continuation found by walking up the parentMC chain,
// or nil if no continuation is found (top-level reached).
func (p *MachineContext) FindEscapeContinuation() *MachineContinuation {
	// First check our own continuation
	if p.cont != nil {
		return p.cont
	}
	// Walk up the parent context chain
	for mc := p.parentMC; mc != nil; mc = mc.parentMC {
		if mc.cont != nil {
			return mc.cont
		}
	}
	return nil
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

func (p *MachineContext) SetValues(vs ...values.Value) {
	p.value = vs
}

func (p *MachineContext) SetValue(v values.Value) {
	p.value = NewMultipleValues(v)
}

func (p *MachineContext) GetValue() values.Value {
	if len(p.value) == 0 {
		return values.Void
	}
	return p.value[0]
}

func (p *MachineContext) GetValues() MultipleValues {
	return p.value
}

func (p *MachineContext) EnvironmentFrame() *environment.EnvironmentFrame {
	return p.env
}

func (p *MachineContext) Arg(index int) values.Value {
	return p.env.GetLocalBindingByIndex(index).Value()
}

func (p *MachineContext) Restore(cont *MachineContinuation) {
	p.env = cont.env
	p.template = cont.template
	// Must copy evals to avoid corrupting the continuation's saved stack.
	// Without copying, modifications to p.evals after restoration would mutate
	// cont.evals, breaking re-invocation of the continuation.
	p.evals = cont.evals.Copy()
	p.cont = cont.parent
	p.pc = cont.pc
}

// PopContinuation pops the current continuation from the machine context and returns it.
// It restores the machine context to the state saved in the popped continuation.
//
// Note: Unlike Restore(), we do NOT copy evals here because PopContinuation is used
// for normal function return where the continuation is consumed once. Restore() is
// used for continuation re-entry (call/cc) where the same continuation may be invoked
// multiple times, requiring the copy to prevent stack corruption.
func (p *MachineContext) PopContinuation() *MachineContinuation {
	q := p.cont
	p.template = q.template
	p.env = q.env
	p.evals = q.evals
	p.cont = q.parent
	p.pc = q.pc
	p.value = q.value
	return q
}

// SaveContinuation pushes a new continuation onto the machine context with the given offset to the current program counter.
func (p *MachineContext) SaveContinuation(off int) {
	p.cont = NewMachineContinuationFromMachineContext(p, off)
	p.evals = NewStack()
}

func (p *MachineContext) CurrentContinuation() *MachineContinuation {
	q := p.cont.Copy()
	return q
}

// CallDepth returns the depth of the current continuation stack.
func (p *MachineContext) CallDepth() int {
	if p.cont == nil {
		return 0
	}
	return p.cont.CallDepth() + 1
}

// FIXME: needs unit tests
// FIXME: not symmetric with Apply of MachineClosure
// FIXME: variadic parameters but no return values
func (p *MachineContext) Apply(mcls *MachineClosure, vs ...values.Value) (*MachineContext, error) {
	tpl := mcls.Template()
	// Create a fresh copy of the local environment for this call.
	// This is critical for recursive functions: without copying, all invocations
	// share the same bindings, causing parameter corruption when evaluating
	// arguments like (+ (f (- n 1)) (f (- n 2))).
	localEnv := mcls.env.LocalEnvironment().Copy().(*environment.LocalEnvironmentFrame)
	env := environment.NewEnvironmentFrameWithParent(localEnv, mcls.env.Parent())
	bnds := localEnv.Bindings()
	l := tpl.ParameterCount()
	if !tpl.IsVariadic() {
		if len(vs) != l {
			return nil, values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "expected %d arguments, got %d", l, len(vs))
		}
		for i := range bnds[:l] {
			bnds[i].SetValue(vs[i])
		}
	} else {
		if len(vs) < l-1 {
			return nil, values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "expected at least %d arguments, got %d", l-1, len(vs))
		}
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
// Context cancellation: The loop checks p.ctx.Done() on each iteration, allowing
// preemption via context.WithTimeout or context.WithCancel. This enables:
//   - Test timeouts that actually stop execution
//   - REPL interrupt support (Ctrl+C)
//   - Resource management for long-running computations
//
// Set the context via SetContext() before calling Run().
func (p *MachineContext) Run() error {
	var err error
	mc := p
	for mc.pc < len(mc.template.operations) {
		// Check for context cancellation (enables preemption via timeout/cancel)
		select {
		case <-mc.ctx.Done():
			return mc.ctx.Err()
		default:
		}

		// Check for debugger breaks
		if mc.debugger != nil {
			bp := mc.debugger.CheckBreakpoint(mc)
			if bp != nil {
				mc.debugger.TriggerBreak(mc, bp)
			} else if mc.debugger.ShouldStep(mc) {
				mc.debugger.TriggerBreak(mc, nil)
			}
		}

		mc, err = mc.template.operations[mc.pc].Apply(mc.ctx, mc)
		if err != nil {
			return err
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
	return &MachineContext{
		ctx:         p.ctx,
		template:    nil,
		pc:          0,
		env:         p.env.TopLevel(), // share global environment chain
		value:       nil,
		evals:       NewStack(),
		cont:        nil,         // fresh call stack - isolated from parent
		expanderCtx: nil,         // sub-contexts don't inherit expander context by default
		parentMC:    p,           // track parent for call/cc continuation capture
		escapeCont:  p.escapeCont, // inherit escape continuation for nested call/cc
	}
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
func (p *MachineContext) CurrentSource() *syntax.SourceContext {
	if p.template != nil {
		return p.template.SourceAt(p.pc)
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

	// Walk continuation chain
	cont := p.cont
	depth := 1
	for cont != nil && depth < maxDepth {
		frame := StackFrame{}
		if cont.template != nil {
			frame.FunctionName = cont.template.Name()
			frame.CurrentLoc = cont.template.SourceAt(cont.pc)
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
	// Run after thunks from innermost to outermost (reverse order)
	for i := len(p.windingStack) - 1; i >= commonDepth; i-- {
		frame := p.windingStack[i]
		if frame.After != nil {
			sub := p.NewSubContext()
			sub.windingStack = p.windingStack[:i] // Set stack to this level
			_, err := sub.Apply(frame.After)
			if err != nil {
				return err
			}
			err = sub.Run()
			if err != nil && !errors.Is(err, ErrMachineHalt) {
				// Propagate escapes and exceptions
				return err
			}
		}
	}
	// Update current winding stack to common ancestor
	p.windingStack = p.windingStack[:commonDepth]
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
				return err
			}
			err = sub.Run()
			if err != nil && !errors.Is(err, ErrMachineHalt) {
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
	// We need to unwind frames that are in sourceStack but not in the common prefix
	for i := len(sourceStack) - 1; i >= commonDepth; i-- {
		frame := sourceStack[i]
		if frame.After != nil {
			sub := p.NewSubContext()
			sub.windingStack = sourceStack[:i]
			_, err := sub.Apply(frame.After)
			if err != nil {
				return err
			}
			err = sub.Run()
			if err != nil && !errors.Is(err, ErrMachineHalt) {
				return err
			}
		}
	}

	// Update context's winding stack to common ancestor
	p.windingStack = sourceStack[:commonDepth]

	// Rewind: run before thunks for frames being entered (to target)
	if err := p.RewindTo(targetStack, commonDepth); err != nil {
		return err
	}

	// Restore the machine state (if we have a valid continuation)
	// We must copy the continuation before restoring because p.Restore assigns
	// p.evals = cont.evals directly. Without copying, subsequent invocations of
	// the same continuation (via call/cc re-entry) would see corrupted stacks
	// because the stack gets modified during execution.
	if cont != nil {
		p.Restore(cont.Copy())
	}
	return nil
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
// When execution completes (either normally or via ErrMachineHalt), any remaining
// frames on the winding stack are unwound (after thunks are called).
func (p *MachineContext) RunWithEscapeHandling() error {
	for {
		err := p.Run()

		// Check for successful completion (nil or ErrMachineHalt)
		if err == nil || errors.Is(err, ErrMachineHalt) {
			// Unwind any remaining frames (call after thunks)
			if len(p.windingStack) > 0 {
				if unwindErr := p.UnwindTo(0); unwindErr != nil {
					return unwindErr
				}
			}

			// If there's a pending escape continuation (from a sub-context escape),
			// restore to it and continue execution
			if p.pendingEscape != nil {
				escapeCont := p.pendingEscape
				p.pendingEscape = nil
				p.Restore(escapeCont)
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
			if restoreErr := p.RestoreWithWindingFrom(escapeErr.Continuation, p.windingStack, escapeErr.WindingStack); restoreErr != nil {
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

		return err
	}
}
