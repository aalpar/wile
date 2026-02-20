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

// OperationStoreGlobalByGlobalIndexLiteralIndexImmediate stores a value to a global variable using an index from the literals pool.
type OperationStoreGlobalByGlobalIndexLiteralIndexImmediate struct {
	OperationBase
	LiteralIndex LiteralIndex
}

// NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate creates a new global store operation.
func NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti LiteralIndex) *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationStoreGlobalByGlobalIndexLiteralIndexImmediate{
		OperationBase: NewOperationBase("machine-operation-store-global-by-global-index-literal-immediate"),
		LiteralIndex:  liti,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-global-by-global-index-literal-immediate %d>", p.LiteralIndex)
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreGlobalByGlobalIndexLiteralIndexImmediate)
	return fieldMatches(p, v, ok, func(op *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}
