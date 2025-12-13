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
	"errors"
	"fmt"
	"wile/values"
)

type ForeignFunction func(ctx context.Context, mc *MachineContext) error

type OperationForeignFunctionCall struct {
	Function ForeignFunction
}

func NewOperationForeignFunctionCall(ffn ForeignFunction) *OperationForeignFunctionCall {
	return &OperationForeignFunctionCall{
		Function: ffn,
	}
}

func (p *OperationForeignFunctionCall) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	if p.Function == nil {
		return nil, fmt.Errorf("foreign function is nil")
	}
	err := p.Function(ctx, mc)
	if err != nil {
		// Check if this is a handled continuation escape.
		// When an escape is handled, the foreign function sets the Handled flag
		// and restores mc to the target continuation state.
		var escapeErr *ErrContinuationEscape
		if errors.As(err, &escapeErr) && escapeErr.Handled {
			return mc, nil
		}
		return nil, err
	}
	mc.pc++
	return mc, nil
}

func (p *OperationForeignFunctionCall) SchemeString() string {
	return "#<machine-operation-foreign-function-call>"
}

func (p *OperationForeignFunctionCall) IsVoid() bool {
	return p == nil
}

func (p *OperationForeignFunctionCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationForeignFunctionCall)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
