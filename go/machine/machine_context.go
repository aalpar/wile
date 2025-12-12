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
	Continuation *MachineContinuation
	Value        values.Value
	Handled      bool // Set to true after the escape has been handled and mc has been restored
}

func (e *ErrContinuationEscape) Error() string {
	return "continuation escape"
}

// MachineContext represents the execution context of a virtual machine.
// It holds the current environment, values, evaluation stack, continuation, and program counter.
// It is created from a MachineContinuation and can be modified during execution.
type MachineContext struct {
	env              *environment.EnvironmentFrame
	value            MultipleValues
	evals            *Stack               // evaluation stack, holds intermediate values during execution
	cont             *MachineContinuation // current continuation
	template         *NativeTemplate
	pc               int
	expanderCtx      *ExpanderContext  // set during macro transformer execution for syntax-local-* access
	exceptionHandler *ExceptionHandler // current exception handler chain for R7RS exceptions
	debugger         *Debugger         // optional debugger for breakpoints and stepping
}

// NewMachineContext creates a new machine context with the given continuation and initial values.
func NewMachineContext(cont *MachineContinuation) *MachineContext {
	q := &MachineContext{
		env:      cont.env,      // cannot copy environment here, it will be copied when pushed onto the stack
		template: cont.template, // not needed to copy, templates are immutable
		cont:     cont.parent,
		value:    cont.value, // must not copy the values, they are passed between contexts
		evals:    cont.evals, // must copy the eval stack
		pc:       cont.pc,
	}
	return q
}

func NewMachineContextFromMachineClosure(cls *MachineClosure) *MachineContext {
	return NewMachineContext(NewMachineContinuation(nil, cls.template, cls.env))
}

func (p *MachineContext) Parent() *MachineContinuation {
	return p.cont
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

func (p *MachineContext) Restore(cont *MachineContinuation) {
	p.env = cont.env
	p.template = cont.template
	p.evals = cont.evals
	p.cont = cont.parent
	p.pc = cont.pc
}

// PopContinuation pops the current continuation from the machine context and returns it.
// It restores the machine context to the state saved in the popped continuation.
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

func (p *MachineContext) Run(ctx context.Context) error {
	var err error
	mc := p
	mc.pc = 0
	for mc.pc < len(mc.template.operations) {
		// Check for debugger breaks
		if mc.debugger != nil {
			if bp := mc.debugger.CheckBreakpoint(mc); bp != nil {
				mc.debugger.TriggerBreak(mc, bp)
			} else if mc.debugger.ShouldStep(mc) {
				mc.debugger.TriggerBreak(mc, nil)
			}
		}

		mc, err = mc.template.operations[mc.pc].Apply(ctx, mc)
		if err != nil {
			return err
		}
	}
	return nil
}

// NewSubContext creates a new MachineContext for running sub-calls (e.g., apply, map, for-each).
// The sub-context shares the global environment but has a fresh call stack, eval stack, and value register.
// This allows foreign functions to call Scheme closures without corrupting the parent context's state.
func (p *MachineContext) NewSubContext() *MachineContext {
	return &MachineContext{
		template:    nil,
		pc:          0,
		env:         p.env.TopLevel(), // share global environment chain
		value:       nil,
		evals:       NewStack(),
		cont:        nil,         // fresh call stack
		expanderCtx: nil,         // sub-contexts don't inherit expander context by default
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

	if cont != nil {
		remaining := countFrames(cont)
		if remaining > 0 {
			trace = append(trace, StackFrame{
				FunctionName: fmt.Sprintf("... %d more frames ...", remaining),
			})
		}
	}

	return trace
}

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
