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
