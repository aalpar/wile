package machine

import (
	"context"
	"wile/values"
)

type OperationPop struct {
}

func NewOperationPop() *OperationPop {
	return &OperationPop{}
}

func (*OperationPop) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = []values.Value{mc.evals.Pop()}
	mc.pc++
	return mc, nil
}

func (p *OperationPop) SchemeString() string {
	return "#<machine-operation-pop>"
}

func (p *OperationPop) IsVoid() bool {
	return p == nil
}

func (p *OperationPop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPop)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
