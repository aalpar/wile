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

type OperationStoreLocalByLocalIndexImmediate struct {
	LocalIndex *environment.LocalIndex
}

func NewOperationStoreLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationStoreLocalByLocalIndexImmediate {
	return &OperationStoreLocalByLocalIndexImmediate{LocalIndex: li}
}

func (p *OperationStoreLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationStoreLocalByLocalIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationStoreLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreLocalByLocalIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LocalIndex.EqualTo(v.LocalIndex)
}

func (p *OperationStoreLocalByLocalIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	err := mc.env.SetLocalValue(p.LocalIndex, mc.evals.Pop())
	if err != nil {
		return mc, err
	}
	mc.pc++
	return mc, nil
}
