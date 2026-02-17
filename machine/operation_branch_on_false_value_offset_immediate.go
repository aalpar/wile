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

// OperationBranchOnFalseValueOffsetImmediate branches if the value register
// is #f. Unlike OperationBranchOnFalseOffsetImmediate, this reads directly
// from the value register instead of popping from the eval stack, eliminating
// the preceding Push instruction that would otherwise be needed.
//
// This is a peephole optimization for if-forms and syntax-case match/fender
// branches where the test result is in the value register and doesn't need
// to survive on the stack.
type OperationBranchOnFalseValueOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationBranchOnFalseValueOffsetImmediate(offset int) *OperationBranchOnFalseValueOffsetImmediate {
	return &OperationBranchOnFalseValueOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-branch-on-false-value-offset-immediate"),
		Offset:        offset,
	}
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationBranchOnFalseValueOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-false-value-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnFalseValueOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnFalseValueOffsetImmediate)
	return fieldMatches(p, v, ok, func(op *OperationBranchOnFalseValueOffsetImmediate) int {
		return op.Offset
	})
}

func (p *OperationBranchOnFalseValueOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	if !values.ValueToBool(mc.GetValue()) {
		mc.pc += p.Offset
	} else {
		mc.pc++
	}
	return mc, nil
}
