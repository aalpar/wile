package machine

import (
	"context"
	"fmt"
	"wile/values"
)

// ForeignFunctionV2 is the new foreign function signature that returns VMAction.
// This enables trampolining for proper call/cc support.
type ForeignFunctionV2 func(ctx context.Context, mc *MachineContext) VMAction

// OperationForeignFunctionCallV2 executes a ForeignFunctionV2 and processes the returned VMAction.
type OperationForeignFunctionCallV2 struct {
	Function ForeignFunctionV2
}

func NewOperationForeignFunctionCallV2(ffn ForeignFunctionV2) *OperationForeignFunctionCallV2 {
	return &OperationForeignFunctionCallV2{
		Function: ffn,
	}
}

func (p *OperationForeignFunctionCallV2) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	if p.Function == nil {
		return nil, fmt.Errorf("foreign function is nil")
	}

	action := p.Function(ctx, mc)
	return processVMAction(ctx, mc, action)
}

// processVMAction handles a VMAction, potentially recursively for chained actions.
func processVMAction(ctx context.Context, mc *MachineContext, action VMAction) (*MachineContext, error) {
	switch a := action.(type) {
	case VMActionReturn:
		mc.SetValue(a.Value)
		mc.pc++
		return mc, nil

	case VMActionReturnMultiple:
		mc.SetValues(a.Values...)
		mc.pc++
		return mc, nil

	case VMActionError:
		return nil, a.Err

	case VMActionApply:
		// Save our return point as a continuation
		// The continuation will call a.Then with the result
		result, err := executeClosureForResult(ctx, mc, a.Closure, a.Args)
		if err != nil {
			return nil, err
		}
		// Process the next action from Then callback
		nextAction := a.Then(result)
		return processVMAction(ctx, mc, nextAction)

	case VMActionApplyMultiple:
		results, err := executeClosureForResults(ctx, mc, a.Closure, a.Args)
		if err != nil {
			return nil, err
		}
		nextAction := a.Then(results)
		return processVMAction(ctx, mc, nextAction)

	case VMActionCallCC:
		// Capture current continuation
		cont := captureCurrentContinuation(mc)
		// Create a closure that invokes the continuation when called
		contClosure := newContinuationClosure(mc, cont)
		// Call the procedure with the continuation closure
		result, err := executeClosureForResult(ctx, mc, a.Procedure, []values.Value{contClosure})
		if err != nil {
			return nil, err
		}
		nextAction := a.Then(result)
		return processVMAction(ctx, mc, nextAction)

	case VMActionInvokeContinuation:
		// Restore the captured continuation state
		mc.Restore(a.Continuation)
		mc.SetValue(a.Value)
		return mc, nil

	default:
		return nil, fmt.Errorf("unknown VMAction type: %T", action)
	}
}

// executeClosureForResult runs a closure in a sub-context and returns its result.
func executeClosureForResult(ctx context.Context, mc *MachineContext, cls *MachineClosure, args []values.Value) (values.Value, error) {
	sub := mc.NewSubContext()
	if _, err := sub.Apply(cls, args...); err != nil {
		return nil, err
	}
	if err := sub.Run(ctx); err != nil && err != ErrMachineHalt {
		return nil, err
	}
	return sub.GetValue(), nil
}

// executeClosureForResults runs a closure and returns multiple values.
func executeClosureForResults(ctx context.Context, mc *MachineContext, cls *MachineClosure, args []values.Value) ([]values.Value, error) {
	sub := mc.NewSubContext()
	if _, err := sub.Apply(cls, args...); err != nil {
		return nil, err
	}
	if err := sub.Run(ctx); err != nil && err != ErrMachineHalt {
		return nil, err
	}
	return sub.GetValues(), nil
}

// captureCurrentContinuation captures the full machine state as a continuation.
func captureCurrentContinuation(mc *MachineContext) *MachineContinuation {
	return NewMachineContinuationFromMachineContext(mc, 1) // +1 to skip past current instruction
}

// newContinuationClosure creates a closure that, when called, invokes a captured continuation.
func newContinuationClosure(mc *MachineContext, cont *MachineContinuation) *MachineClosure {
	// Create a foreign function that invokes the continuation
	fn := func(ctx context.Context, innerMC *MachineContext) error {
		// Get the value passed to the continuation
		val := innerMC.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
		// Restore the continuation
		innerMC.Restore(cont)
		innerMC.SetValue(val)
		return nil
	}

	// Wrap in a closure - this is a 1-arg non-variadic function
	return NewForeignClosure(mc.EnvironmentFrame().TopLevel(), 1, false, fn)
}

func (p *OperationForeignFunctionCallV2) SchemeString() string {
	return "#<machine-operation-foreign-function-call-v2>"
}

func (p *OperationForeignFunctionCallV2) IsVoid() bool {
	return p == nil
}

func (p *OperationForeignFunctionCallV2) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationForeignFunctionCallV2)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
