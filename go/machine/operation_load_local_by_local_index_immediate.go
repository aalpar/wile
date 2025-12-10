package machine

import (
	"context"
	"fmt"
	"wile/environment"
	"wile/values"
)

type OperationLoadLocalByLocalIndexImmediate struct {
	LocalIndex *environment.LocalIndex
}

func NewOperationLoadLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationLoadLocalByLocalIndexImmediate {
	return &OperationLoadLocalByLocalIndexImmediate{LocalIndex: li}
}

func (p *OperationLoadLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationLoadLocalByLocalIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLocalByLocalIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LocalIndex.EqualTo(v.LocalIndex)
}

func (p *OperationLoadLocalByLocalIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	bd := mc.env.GetLocalBinding(p.LocalIndex)
	if bd == nil {
		return nil, values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such local binding %s", p.LocalIndex)
	}
	mc.value = MultipleValues{bd.Value()}
	mc.pc++
	return mc, nil
}
