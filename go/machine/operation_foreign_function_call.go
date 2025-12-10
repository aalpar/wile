package machine

import (
	"context"
	"errors"
	"fmt"
	"skeme/values"
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
