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
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// --- Apply ---

// OperationApply is the bytecode operation that dispatches procedure calls.
// The compiler emits it after pushing arguments onto the eval stack and placing
// the callee in the value register. Apply pops all arguments and delegates to
// MachineContext.ApplyCallable, which handles the five callable types
// (MachineClosure, ForeignClosure, CaseLambdaClosure, Parameter, ComposableContinuation).
type OperationApply struct {
	OperationBase
}

// NewOperationApply returns a new apply operation.
func NewOperationApply() *OperationApply {
	return &OperationApply{
		OperationBase: NewOperationBase("operation-apply"),
	}
}

// EqualTo returns true if o is also an OperationApply (identity by type).
func (p *OperationApply) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationApply)
	return SameType(p, v, ok)
}

// --- SelfTailCall ---

// OperationSelfTailCall is the in-place self-recursive tail call: it drains the
// ArgCount already-evaluated argument values off the eval stack, writes them into
// the current frame's parameter slots 0..ArgCount-1 (parallel assignment — the
// args are on the stack, so old slot values stay intact during evaluation), and
// resets pc=0. No frame acquire, no SaveContinuation, no continuation growth.
//
// Emitted only behind validate.bodyIsSelfTailReusable + a depth-0 self-tail call
// site; that proof (no capture, no escaping closure, non-variadic) is what makes
// reusing the live frame sound (escape-gated plan Phase 4).
type OperationSelfTailCall struct {
	OperationBase
	ArgCount int
}

// NewOperationSelfTailCall returns a self-tail-call op rebinding argCount slots.
func NewOperationSelfTailCall(argCount int) *OperationSelfTailCall {
	return &OperationSelfTailCall{
		OperationBase: NewOperationBaseWithGoName("operation:self-tail-call", "SelfTailCall"),
		ArgCount:      argCount,
	}
}

// EqualTo returns true if o is also an OperationSelfTailCall with the same arity.
func (p *OperationSelfTailCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSelfTailCall)
	return FieldMatches(p, v, ok, func(op *OperationSelfTailCall) int {
		return op.ArgCount
	})
}

// --- ForeignFunctionCall ---

// OperationForeignFunctionCall executes a Go function within the VM loop.
// Used for foreign closures that do nested VM execution (sub-context + Run),
// where the iterative VM loop prevents Go stack growth. Leaf primitives use
// ForeignClosure + applyForeign instead.
type OperationForeignFunctionCall struct {
	OperationBase
	Function ForeignFunction
}

func NewOperationForeignFunctionCall(ffn ForeignFunction) *OperationForeignFunctionCall {
	return &OperationForeignFunctionCall{
		OperationBase: NewOperationBase("machine-operation-foreign-function-call"),
		Function:      ffn,
	}
}

func (p *OperationForeignFunctionCall) Apply(mc *MachineContext) (rmc *MachineContext, rerr error) {
	if p.Function == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "foreign function is nil")
	}
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		var err error
		switch v := r.(type) {
		case error:
			err = v
		default:
			err = werr.WrapForeignErrorf(werr.ErrPanicRecovery, "foreign function call: %v", v)
		}
		rmc = nil
		// applyCallableError passes VM signal types (prompt abort, exception
		// escape, timer interrupt) through unchanged and converts everything
		// else to a Scheme exception. Mirrors the error-return path below.
		rerr = applyCallableError(mc, err)
	}()
	mc.counters.ForeignCalls++
	err := p.Function(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}
	mc.pc++
	return mc, nil
}

func (p *OperationForeignFunctionCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationForeignFunctionCall)
	return SameType(p, v, ok)
}

// --- UnpackListToStack ---

// OperationUnpackListToStack reads a proper list from the value register
// and pushes each element to the eval stack in order. Used by compiled
// (apply proc arg1 ... args) to flatten the final arg list onto the stack
// before Pull + OpApply.
//
// Errors if the value is not a proper list (improper list or non-list).
type OperationUnpackListToStack struct {
	OperationBase
}

// NewOperationUnpackListToStack returns a new unpack-list-to-stack operation.
func NewOperationUnpackListToStack() *OperationUnpackListToStack {
	return &OperationUnpackListToStack{
		OperationBase: NewOperationBase("operation-unpack-list-to-stack"),
	}
}

// EqualTo returns true if o is also an OperationUnpackListToStack (identity by type).
func (p *OperationUnpackListToStack) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnpackListToStack)
	return SameType(p, v, ok)
}
