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

// OperationLoadGlobalByGlobalIndexLiteralIndexImmediate loads a global variable using an index from the literals pool.
type OperationLoadGlobalByGlobalIndexLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate creates a new global load operation.
func NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(li LiteralIndex) *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationLoadGlobalByGlobalIndexLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-load-global-by-global-index-literal-index-immediate"),
		LiteralIndex:  li,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-global-by-global-index-literal-index-immediate %d>", p.LiteralIndex)
}

// EqualTo returns true if both operations have the same literal index.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate)
	return fieldMatches(p, v, ok, func(op *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}
