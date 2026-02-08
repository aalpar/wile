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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

type OperationLoadLocalByLocalIndexImmediate struct {
	LocalIndex *environment.LocalIndex
}

func NewOperationLoadLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationLoadLocalByLocalIndexImmediate {
	return &OperationLoadLocalByLocalIndexImmediate{LocalIndex: li}
}

func (p *OperationLoadLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationLoadLocalByLocalIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLocalByLocalIndexImmediate)
	return fieldMethodMatches(p, v, ok,
		func(op *OperationLoadLocalByLocalIndexImmediate) *environment.LocalIndex {
			return op.LocalIndex
		},
		func(a, b *environment.LocalIndex) bool {
			return a.EqualTo(b)
		})
}

func (p *OperationLoadLocalByLocalIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	bd := mc.env.GetLocalBinding(p.LocalIndex)
	if bd == nil {
		return nil, mc.Error(fmt.Sprintf("no such local binding %s", p.LocalIndex))
	}
	mc.value = MultipleValues{bd.Value()}
	mc.pc++
	return mc, nil
}
