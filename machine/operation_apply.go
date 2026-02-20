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
)

// OperationApply is the bytecode operation that dispatches procedure calls.
// The compiler emits it after pushing arguments onto the eval stack and placing
// the callee in the value register. Apply pops all arguments and delegates to
// MachineContext.ApplyCallable, which handles the four callable types
// (MachineClosure, CaseLambdaClosure, Parameter, ComposableContinuation).
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
	return sameType(p, v, ok)
}
