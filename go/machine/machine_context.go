package machine

import (
	"context"
	"wile/environment"
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
	env      *environment.EnvironmentFrame
	value    MultipleValues
	evals    *Stack               // evaluation stack, holds intermediate values during execution
	cont     *MachineContinuation // current continuation
	template *NativeTemplate
	pc       int
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
	env := mcls.env
	tpl := mcls.Template()
	bnds := env.LocalEnvironment().Bindings()
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
		template: nil,
		pc:       0,
		env:      p.env.TopLevel(), // share global environment chain
		value:    nil,
		evals:    NewStack(),
		cont:     nil, // fresh call stack
	}
}
