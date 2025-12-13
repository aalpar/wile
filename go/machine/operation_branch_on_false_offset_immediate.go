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

type OperationBranchOnFalseOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOnFalseOffsetImmediate(offset int) *OperationBranchOnFalseOffsetImmediate {
	return &OperationBranchOnFalseOffsetImmediate{
		Offset: offset,
	}
}

func (p *OperationBranchOnFalseOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-false-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnFalseOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOnFalseOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnFalseOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}

func (p *OperationBranchOnFalseOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	v := mc.evals.Pop()
	if values.EqualTo(v, values.FalseValue) {
		mc.pc += p.Offset
	} else {
		mc.pc++
	}
	return mc, nil
}
