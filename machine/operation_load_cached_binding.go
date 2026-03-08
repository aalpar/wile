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
	"fmt"

	"github.com/aalpar/wile/values"
)

// OperationLoadCachedBinding loads a global variable from a compile-time
// resolved *Binding pointer, bypassing the runtime environment lookup
// path used by OpLoadGlobal.
type OperationLoadCachedBinding struct {
	OperationBase
	BindingIndex int32
}

// NewOperationLoadCachedBinding creates a new cached binding load operation.
func NewOperationLoadCachedBinding(idx int32) *OperationLoadCachedBinding {
	return &OperationLoadCachedBinding{
		OperationBase: NewOperationBase("machine-operation-load-cached-binding"),
		BindingIndex:  idx,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadCachedBinding) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-cached-binding %d>", p.BindingIndex)
}

// EqualTo returns true if both operations have the same binding index.
func (p *OperationLoadCachedBinding) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadCachedBinding)
	return fieldMatches(p, v, ok, func(op *OperationLoadCachedBinding) int32 {
		return op.BindingIndex
	})
}
