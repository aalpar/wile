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
	"wile/values"
)

type OperationLoadLiteralByLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationLoadLiteralByLiteralIndexImmediate(li LiteralIndex) *OperationLoadLiteralByLiteralIndexImmediate {
	return &OperationLoadLiteralByLiteralIndexImmediate{LiteralIndex: li}
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	mc.value = []values.Value{o}
	mc.pc++
	return mc, nil
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-literal-by-literal-index-immediate %d>", p.LiteralIndex)
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLiteralByLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}
