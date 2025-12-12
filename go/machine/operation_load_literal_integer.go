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

var _ Operation = (*OperationLoadLiteralInteger)(nil)

type OperationLoadLiteralInteger struct {
	Value int64
}

func NewOperationLoadLiteralInteger(v int64) *OperationLoadLiteralInteger {
	return &OperationLoadLiteralInteger{Value: v}
}

func (p *OperationLoadLiteralInteger) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = nil
	mc.pc++
	return mc, nil
}

func (p *OperationLoadLiteralInteger) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-literal-integer>")
}

func (p *OperationLoadLiteralInteger) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadLiteralInteger) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLiteralInteger)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
