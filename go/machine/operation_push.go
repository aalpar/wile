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
