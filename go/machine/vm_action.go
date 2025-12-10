package machine

import (
	"skeme/values"
)

// VMAction represents an action the VM should take after a foreign function returns.
// This trampolining model allows foreign functions to request closure calls, continuation
// invocations, and other control flow operations without directly manipulating VM state.
type VMAction interface {
	vmAction() // marker method to ensure type safety
}

// VMActionReturn signals that the foreign function completed normally.
// The VM should continue with the next operation.
type VMActionReturn struct {
	Value values.Value
}

func (VMActionReturn) vmAction() {}

// VMActionReturnMultiple signals completion with multiple return values.
type VMActionReturnMultiple struct {
	Values []values.Value
}

func (VMActionReturnMultiple) vmAction() {}

// VMActionApply signals that the VM should call a closure with the given arguments.
// After the closure returns, the VM calls the Then callback with the result,
// which returns the next action to take.
type VMActionApply struct {
	Closure *MachineClosure
	Args    []values.Value
	Then    func(result values.Value) VMAction
}

func (VMActionApply) vmAction() {}

// VMActionApplyMultiple is like VMActionApply but for closures that return multiple values.
type VMActionApplyMultiple struct {
	Closure *MachineClosure
	Args    []values.Value
	Then    func(results []values.Value) VMAction
}

func (VMActionApplyMultiple) vmAction() {}

// VMActionCallCC captures the current continuation and passes it to a procedure.
// The continuation is wrapped as a Scheme procedure that, when called, restores
// the captured machine state and returns the given value.
type VMActionCallCC struct {
	Procedure *MachineClosure
	Then      func(result values.Value) VMAction
}

func (VMActionCallCC) vmAction() {}

// VMActionInvokeContinuation signals that the VM should restore a captured continuation
// and return the given value to it.
type VMActionInvokeContinuation struct {
	Continuation *MachineContinuation
	Value        values.Value
}

func (VMActionInvokeContinuation) vmAction() {}

// VMActionError signals that an error occurred.
type VMActionError struct {
	Err error
}

func (VMActionError) vmAction() {}

// VMActionSequence runs multiple actions in sequence, threading results through.
type VMActionSequence struct {
	Actions []VMAction
}

func (VMActionSequence) vmAction() {}

// Helper constructors for common actions

// Return creates a VMActionReturn with the given value.
func Return(v values.Value) VMAction {
	return VMActionReturn{Value: v}
}

// ReturnVoid creates a VMActionReturn with void.
func ReturnVoid() VMAction {
	return VMActionReturn{Value: values.Void}
}

// ReturnMultiple creates a VMActionReturnMultiple with the given values.
func ReturnMultiple(vs ...values.Value) VMAction {
	return VMActionReturnMultiple{Values: vs}
}

// Apply creates a VMActionApply to call a closure and then continue.
func Apply(cls *MachineClosure, args []values.Value, then func(values.Value) VMAction) VMAction {
	return VMActionApply{
		Closure: cls,
		Args:    args,
		Then:    then,
	}
}

// ApplyAndReturn creates a VMActionApply that returns the closure's result.
func ApplyAndReturn(cls *MachineClosure, args []values.Value) VMAction {
	return VMActionApply{
		Closure: cls,
		Args:    args,
		Then:    func(result values.Value) VMAction { return Return(result) },
	}
}

// Error creates a VMActionError with the given error.
func Error(err error) VMAction {
	return VMActionError{Err: err}
}
