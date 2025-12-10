package machine

import (
	"context"
	"skeme/values"
)

type OperationPull struct {
}

func NewOperationPull() *OperationPull {
	return &OperationPull{}
}

func (p *OperationPull) SchemeString() string {
	return "#<machine-operation-pull>"
}

func (p *OperationPull) IsVoid() bool {
	return p == nil
}

func (p *OperationPull) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPull)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}

func (*OperationPull) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = MultipleValues{mc.evals.Pull()}
	mc.pc++
	return mc, nil
}
