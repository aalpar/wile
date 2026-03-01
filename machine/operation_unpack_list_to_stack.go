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
	return sameType(p, v, ok)
}
