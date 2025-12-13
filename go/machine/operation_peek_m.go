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
	"fmt"
	"wile/values"
)

type OperationPeekK struct {
	Depth int
}

func NewOperationPeekK(depth int) *OperationPeekK {
	return &OperationPeekK{Depth: depth}
}

func (p *OperationPeekK) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = []values.Value{mc.evals.PeekK(p.Depth)}
	mc.pc++
	return mc, nil
}

func (p *OperationPeekK) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-peek-k %d>", p.Depth)
}

func (p *OperationPeekK) IsVoid() bool {
	return p == nil
}

func (p *OperationPeekK) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPeekK)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Depth == v.Depth
}
