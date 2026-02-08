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

	"github.com/aalpar/wile/values"
)

// OperationApply is the bytecode operation that dispatches procedure calls.
// It is the single point where all Scheme procedure application converges at
// runtime. The compiler emits OperationApply after pushing arguments onto the
// eval stack and placing the callee in the value register.
//
// The type switch inside Apply is exhaustive over the four callable value types
// in the VM. Each type reaches this point through different Scheme-level syntax
// but all share the same calling convention: callee in value[0], arguments
// popped from the eval stack.
type OperationApply struct{}

// NewOperationApply returns a new apply operation.
func NewOperationApply() *OperationApply {
	return &OperationApply{}
}

// SchemeString returns the external representation for debugging.
func (p *OperationApply) SchemeString() string {
	return "#<operation-apply>"
}

// IsVoid returns true if the receiver is nil (satisfies values.Value).
func (p *OperationApply) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if o is also an OperationApply (identity by type).
func (p *OperationApply) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationApply)
	return sameType(p, v, ok)
}

// Apply dispatches a procedure call to the appropriate handler based on the
// callee's concrete type. The eval stack holds the arguments (pushed by the
// compiler-generated Push/Pull sequence); PopAll retrieves them.
//
// The type switch covers every callable type in the VM:
//
//   - MachineClosure: standard Scheme lambda. Created by OperationMakeClosure
//     from compiled NativeTemplates. This is the common case — nearly all
//     user-defined and built-in procedures are MachineClosure values.
//
//   - CaseLambdaClosure: R7RS case-lambda (§4.2.9). Wraps multiple
//     MachineClosure clauses, each with a different arity. ApplyCaseLambda
//     selects the clause whose parameter count matches the argument count.
//
//   - Parameter: R7RS make-parameter (§4.2.6). A callable object that acts as
//     a mutable cell: zero arguments returns the current value, one argument
//     sets it (after an optional converter). Parameters must manage their own
//     continuation restore because they don't use NativeTemplate bytecode —
//     they execute Go code directly and must explicitly return control.
//
//   - ComposableContinuation: delimited continuation captured by
//     call-with-composable-continuation. Invoking it splices the captured
//     continuation frames onto the current chain, rewinding/unwinding
//     dynamic-wind extents as needed. See Flatt et al. ICFP 2007.
//
// Any other value in the callee position is a type error — Scheme attempted
// to call a non-procedure.
func (p *OperationApply) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	vs := mc.evals.PopAll()
	mc.counters.StackPopAlls++
	mc.counters.StackElementsCopied += uint64(len(vs))
	switch cls := mc.value[0].(type) {
	case *MachineClosure:
		return mc.Apply(cls, vs...)
	case *CaseLambdaClosure:
		return mc.ApplyCaseLambda(cls, vs...)
	case *Parameter:
		return applyParameter(ctx, mc, cls, vs)
	case *ComposableContinuation:
		return applyComposableContinuation(mc, cls, vs)
	default:
		err := mc.Error(fmt.Sprintf("expected a closure, got %s", mc.value[0].SchemeString()))
		return mc, err
	}
}

// applyParameter handles calling a parameter object.
// With 0 args: returns the current value.
// With 1 arg: sets the value (after applying converter if present).
// After setting the return value, we restore the saved continuation to return
// to the caller, just like a closure's RestoreContinuation would do.
func applyParameter(ctx context.Context, mc *MachineContext, param *Parameter, args []values.Value) (*MachineContext, error) { //nolint:unparam
	switch len(args) {
	case 0:
		// Get: return current value
		mc.SetValue(param.Value())
		// Restore continuation to return to caller (like RestoreContinuation)
		if mc.cont != nil {
			mc.Restore(mc.cont)
		} else {
			// Top-level with no saved continuation - just halt
			return mc, ErrMachineHalt
		}
		return mc, nil

	case 1:
		// Set: apply converter if present, then set value
		newVal := args[0]

		if param.HasConverter() {
			// Apply the converter using a sub-context
			converter := param.Converter()
			sub := mc.NewSubContext()
			_, err := sub.Apply(converter, newVal)
			if err != nil {
				wrapErr := mc.WrapError(err, "parameter: failed to apply converter")
				return mc, wrapErr
			}
			err = sub.Run()
			if err != nil {
				if !errors.Is(err, ErrMachineHalt) {
					wrapErr := mc.WrapError(err, "parameter: converter error")
					return mc, wrapErr
				}
			}
			newVal = sub.GetValue()
		}

		param.SetValue(newVal)
		mc.SetValue(values.Void)
		// Restore continuation to return to caller (like RestoreContinuation)
		if mc.cont != nil {
			mc.Restore(mc.cont)
		} else {
			// Top-level with no saved continuation - just halt
			return mc, ErrMachineHalt
		}
		return mc, nil

	default:
		err := mc.Error(fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)))
		return mc, err
	}
}

// applyComposableContinuation applies a composable continuation by splicing
// its captured frames onto the current continuation chain. The continuation
// is deep-copied for safe re-invocation.
//
// See: Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
func applyComposableContinuation(mc *MachineContext, cc *ComposableContinuation, args []values.Value) (*MachineContext, error) {
	if len(args) != 1 {
		err := mc.Error(fmt.Sprintf("composable continuation: expected 1 argument, got %d", len(args)))
		return mc, err
	}

	// Reject cross-thread composable continuation invocation
	if mc.threadID != cc.threadID {
		return mc, values.WrapForeignErrorf(values.ErrCrossThreadContinuation,
			"composable continuation: captured in thread %d, invoked from thread %d",
			cc.threadID, mc.threadID)
	}

	// Deep-copy the segment for safe re-invocation
	segment := cc.Cont().DeepCopy()

	// Graft the segment's bottom frame onto the current continuation chain
	GraftContinuation(segment, mc.cont)

	// Handle dynamic-wind: unwind current extents not in captured stack,
	// rewind captured extents not in current stack.
	err := mc.RestoreWithWindingFrom(nil, mc.windingStack, cc.WindingStack())
	if err != nil {
		return mc, err
	}

	// Restore from the top of the segment (resume captured computation)
	mc.Restore(segment)
	mc.SetValue(args[0])
	return mc, nil
}
