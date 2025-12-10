package machine

import (
	"context"
	"wile/values"
)

type OperationPopAll struct {
}

func NewOperationPopAll() *OperationPopAll {
	return &OperationPopAll{}
}

func (*OperationPopAll) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = mc.evals.PopAll()
	mc.pc++
	return mc, nil
}

func (p *OperationPopAll) SchemeString() string {
	return "#<machine-operation-pop-all>"
}

func (p *OperationPopAll) IsVoid() bool {
	return p == nil
}

func (p *OperationPopAll) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPopAll)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
