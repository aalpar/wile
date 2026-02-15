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
	"context"
	"fmt"

	"github.com/aalpar/wile/values"
)

// OperationMakeCaseLambdaClosure creates a case-lambda closure from multiple closures.
// Stack layout (top to bottom): closure_n, closure_n-1, ..., closure_1
// The closureCount immediate specifies how many closures to pop.
type OperationMakeCaseLambdaClosure struct {
	OperationBase
	closureCount int
}

func NewOperationMakeCaseLambdaClosure(closureCount int) *OperationMakeCaseLambdaClosure {
	return &OperationMakeCaseLambdaClosure{
		OperationBase: NewOperationBase("machine-operation-make-case-lambda-closure"),
		closureCount:  closureCount,
	}
}

func (p *OperationMakeCaseLambdaClosure) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	closures := make([]*MachineClosure, p.closureCount)
	for i := p.closureCount - 1; i >= 0; i-- {
		v := mc.evals.Pop()
		cls, ok := v.(*MachineClosure)
		if !ok {
			err := mc.Error(fmt.Sprintf("expected closure in case-lambda, got %T", v))
			return mc, err
		}
		closures[i] = cls
	}

	caseLambda := NewCaseLambdaClosure(closures)
	mc.SetValue(caseLambda)
	mc.pc++
	return mc, nil
}

func (p *OperationMakeCaseLambdaClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeCaseLambdaClosure)
	return fieldMatches(p, v, ok, func(op *OperationMakeCaseLambdaClosure) int { return op.closureCount })
}
