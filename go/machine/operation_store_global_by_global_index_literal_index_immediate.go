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
	"wile/environment"
	"wile/values"
)

type OperationStoreGlobalByGlobalIndexLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti LiteralIndex) *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationStoreGlobalByGlobalIndexLiteralIndexImmediate{LiteralIndex: liti}
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-global-by-global-index-literal-immediate %d>", p.LiteralIndex)
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreGlobalByGlobalIndexLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	if o == nil {
		return nil, mc.Error(fmt.Sprintf("literal index %v does not exist", p.LiteralIndex))
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, mc.Error(fmt.Sprintf("literal %v is not a global index", o))
	}
	val := mc.evals.Pop()
	err := mc.env.GlobalEnvironment().SetOwnGlobalValue(gi, val)
	if err != nil {
		return nil, mc.WrapError(err, fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
	}
	mc.pc++
	return mc, nil
}
