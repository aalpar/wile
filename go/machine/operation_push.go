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
	"wile/values"
)

type OperationPush struct {
}

func NewOperationPush() *OperationPush {
	return &OperationPush{}
}

func (p *OperationPush) SchemeString() string {
	return "#<machine-operation-push>"
}

func (p *OperationPush) IsVoid() bool {
	return p == nil
}

func (p *OperationPush) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPush)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}

func (*OperationPush) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.evals.PushAll(mc.value)
	mc.pc++
	return mc, nil
}
