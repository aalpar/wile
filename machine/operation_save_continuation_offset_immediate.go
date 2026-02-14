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

type OperationSaveContinuationOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationSaveContinuationOffsetImmediate(off int) *OperationSaveContinuationOffsetImmediate {
	return &OperationSaveContinuationOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-save-continuation-offset-immediate"),
		Offset:        off,
	}
}

func (p *OperationSaveContinuationOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	// copy the current continuation and push it onto the eval stack
	err := mc.SaveContinuation(p.Offset)
	if err != nil {
		return nil, err
	}
	mc.pc++
	return mc, nil
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationSaveContinuationOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-save-continuation-offset-immediate %d>", p.Offset)
}

func (p *OperationSaveContinuationOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSaveContinuationOffsetImmediate)
	return fieldMatches(p, v, ok, func(op *OperationSaveContinuationOffsetImmediate) int { return op.Offset })
}
