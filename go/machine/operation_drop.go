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

	"wile/values"
)

// OperationDrop removes the top value from the eval stack without
// affecting the value register. This is used when we need to clean
// up the stack but preserve the current result.
type OperationDrop struct{}

func NewOperationDrop() *OperationDrop {
	return &OperationDrop{}
}

func (*OperationDrop) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.evals.Pop() // Discard the value
	mc.pc++
	return mc, nil
}

func (p *OperationDrop) SchemeString() string {
	return "#<machine-operation-drop>"
}

func (p *OperationDrop) IsVoid() bool {
	return p == nil
}

func (p *OperationDrop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationDrop)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
