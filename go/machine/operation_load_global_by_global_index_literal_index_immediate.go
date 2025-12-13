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

type OperationLoadGlobalByGlobalIndexLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(li LiteralIndex) *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationLoadGlobalByGlobalIndexLiteralIndexImmediate{LiteralIndex: li}
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-global-by-global-index-literal-index-immediate %d>", p.LiteralIndex)
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	if o == nil {
		return nil, mc.Error(fmt.Sprintf("literal index %v does not exist", p.LiteralIndex))
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, mc.Error(fmt.Sprintf("literal %v is not a global index", o))
	}
	// Use GetGlobalBinding which traverses the parent chain.
	// This is necessary for cross-phase lookups (e.g., expand-time primitives
	// accessed from syntax-case fenders running in a child environment).
	bd := mc.env.GetGlobalBinding(gi)
	if bd == nil {
		return nil, mc.Error(fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
	}
	mc.value = NewMultipleValues(bd.Value())
	mc.pc++
	return mc, nil
}
