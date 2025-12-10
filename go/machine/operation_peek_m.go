package machine

import (
	"context"
	"fmt"
	"skeme/values"
)

type OperationPeekK struct {
	Depth int
}

func NewOperationPeekK(depth int) *OperationPeekK {
	return &OperationPeekK{Depth: depth}
}

func (p *OperationPeekK) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = []values.Value{mc.evals.PeekK(p.Depth)}
	mc.pc++
	return mc, nil
}

func (p *OperationPeekK) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-peek-k %d>", p.Depth)
}

func (p *OperationPeekK) IsVoid() bool {
	return p == nil
}

func (p *OperationPeekK) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationPeekK)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Depth == v.Depth
}
