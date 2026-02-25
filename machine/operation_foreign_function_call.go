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
	"errors"

	"github.com/aalpar/wile/values"
)

// OperationForeignFunctionCall executes a Go function within the VM loop.
// Used for foreign closures that do nested VM execution (sub-context + Run),
// where the iterative VM loop prevents Go stack growth. Leaf primitives use
// ForeignClosure + applyForeign instead.
type OperationForeignFunctionCall struct {
	OperationBase
	Function ForeignFunction
}

func NewOperationForeignFunctionCall(ffn ForeignFunction) *OperationForeignFunctionCall {
	return &OperationForeignFunctionCall{
		OperationBase: NewOperationBase("machine-operation-foreign-function-call"),
		Function:      ffn,
	}
}

func (p *OperationForeignFunctionCall) Apply(mc *MachineContext) (rmc *MachineContext, rerr error) {
	if p.Function == nil {
		return nil, values.WrapForeignErrorf(values.ErrUnexpectedNil, "foreign function is nil")
	}
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		var err error
		switch v := r.(type) {
		case error:
			err = v
		default:
			err = values.WrapForeignErrorf(values.ErrPanicRecovery, "foreign function call: %v", v)
		}
		rmc = nil
		rerr = goErrorToSchemeException(mc, err)
	}()
	mc.counters.ForeignCalls++
	err := p.Function(mc)
	if err != nil {
		var abortErr *ErrPromptAbort
		if errors.As(err, &abortErr) {
			return nil, err
		}
		var exitErr *ErrExitEscape
		if errors.As(err, &exitErr) {
			return nil, err
		}
		var excErr *ErrExceptionEscape
		if errors.As(err, &excErr) {
			return nil, err
		}
		return nil, goErrorToSchemeException(mc, err)
	}
	mc.pc++
	return mc, nil
}

func (p *OperationForeignFunctionCall) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationForeignFunctionCall)
	return sameType(p, v, ok)
}
