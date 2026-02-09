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

	"github.com/aalpar/wile/values"
)

// OperationApply is the bytecode operation that dispatches procedure calls.
// The compiler emits it after pushing arguments onto the eval stack and placing
// the callee in the value register. Apply pops all arguments and delegates to
// MachineContext.ApplyCallable, which handles the four callable types
// (MachineClosure, CaseLambdaClosure, Parameter, ComposableContinuation).
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

// Apply pops all arguments from the eval stack and delegates to
// MachineContext.ApplyCallable for type dispatch. See ApplyCallable for
// the supported callable types and their calling conventions.
//
// Errors from ApplyCallable are wrapped with the current source location
// so that bytecode-path failures include file/line context for debugging.
func (p *OperationApply) Apply(_ context.Context, mc *MachineContext) (*MachineContext, error) {
	vs := mc.evals.PopAll()
	mc.counters.StackPopAlls++
	mc.counters.StackElementsCopied += uint64(len(vs))
	result, err := mc.ApplyCallable(mc.value[0], vs...)
	if err != nil {
		return result, mc.WrapError(err, "")
	}
	return result, nil
}
