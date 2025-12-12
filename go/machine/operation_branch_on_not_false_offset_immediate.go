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

type OperationBranchOnNotFalseOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOnNotFalseOffsetImmediate(offset int) *OperationBranchOnNotFalseOffsetImmediate {
	return &OperationBranchOnNotFalseOffsetImmediate{
		Offset: offset,
	}
}

func (p *OperationBranchOnNotFalseOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-not-false-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnNotFalseOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOnNotFalseOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnNotFalseOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}

func (p *OperationBranchOnNotFalseOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	v := mc.evals.Pop()
	if !values.EqualTo(v, values.FalseValue) {
		mc.pc += p.Offset
	} else {
		mc.pc++
	}
	return mc, nil
}
