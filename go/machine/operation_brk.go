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

type OperationBrk struct {
	Fn func(context.Context, *MachineContext) error
}

func NewOperationBrk(fn func(context.Context, *MachineContext) error) *OperationBrk {
	return &OperationBrk{Fn: fn}
}

func (p *OperationBrk) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	err := p.Fn(ctx, mc)
	mc.pc++
	return mc, err
}

func (p *OperationBrk) SchemeString() string {
	return "#<machine-operation-brk>"
}

func (p *OperationBrk) IsVoid() bool {
	return p == nil
}

func (p *OperationBrk) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBrk)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
