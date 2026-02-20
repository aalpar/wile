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

type OperationBranchOffsetImmediate struct {
	OperationBase
	Offset int
}

func NewOperationBranchOffsetImmediate(offset int) *OperationBranchOffsetImmediate {
	return &OperationBranchOffsetImmediate{
		OperationBase: NewOperationBase("machine-operation-branch-offset-immediate"),
		Offset:        offset,
	}
}

// SchemeString overrides OperationBase to include offset value.
func (p *OperationBranchOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOffsetImmediate)
	return fieldMatches(p, v, ok, func(op *OperationBranchOffsetImmediate) int { return op.Offset })
}
