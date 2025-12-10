package machine

import (
	"context"
	"fmt"
	"skeme/values"
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
