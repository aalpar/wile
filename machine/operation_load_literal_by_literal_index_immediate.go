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

// OperationLoadLiteralByLiteralIndexImmediate loads a literal value from the literals pool.
type OperationLoadLiteralByLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationLoadLiteralByLiteralIndexImmediate creates a new literal load operation.
func NewOperationLoadLiteralByLiteralIndexImmediate(li LiteralIndex) *OperationLoadLiteralByLiteralIndexImmediate {
	return &OperationLoadLiteralByLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-load-literal-by-literal-index-immediate"),
		LiteralIndex:  li,
	}
}

// Apply executes the operation, loading the literal value.
func (p *OperationLoadLiteralByLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	mc.SetValue(o)
	mc.pc++
	return mc, nil
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadLiteralByLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-literal-by-literal-index-immediate %d>", p.LiteralIndex)
}

// EqualTo returns true if both operations have the same literal index.
func (p *OperationLoadLiteralByLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLiteralByLiteralIndexImmediate)
	return fieldMatches(p, v, ok, func(op *OperationLoadLiteralByLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}
