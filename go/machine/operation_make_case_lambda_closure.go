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

// OperationMakeCaseLambdaClosure creates a case-lambda closure from multiple closures.
// Stack layout (top to bottom): closure_n, closure_n-1, ..., closure_1
// The closureCount immediate specifies how many closures to pop.
type OperationMakeCaseLambdaClosure struct {
	closureCount int
}

func NewOperationMakeCaseLambdaClosure(closureCount int) *OperationMakeCaseLambdaClosure {
	return &OperationMakeCaseLambdaClosure{
		closureCount: closureCount,
	}
}

func (p *OperationMakeCaseLambdaClosure) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	closures := make([]*MachineClosure, p.closureCount)
	for i := p.closureCount - 1; i >= 0; i-- {
		v := mc.evals.Pop()
		cls, ok := v.(*MachineClosure)
		if !ok {
			return mc, mc.Error(fmt.Sprintf("expected closure in case-lambda, got %T", v))
		}
		closures[i] = cls
	}

	caseLambda := NewCaseLambdaClosure(closures)
	mc.value = MultipleValues{caseLambda}
	mc.pc++
	return mc, nil
}

func (p *OperationMakeCaseLambdaClosure) SchemeString() string {
	return "#<machine-operation-make-case-lambda-closure>"
}

func (p *OperationMakeCaseLambdaClosure) IsVoid() bool {
	return p == nil
}

func (p *OperationMakeCaseLambdaClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeCaseLambdaClosure)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.closureCount == v.closureCount
}
