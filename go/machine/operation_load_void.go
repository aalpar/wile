package machine

import (
	"context"
	"fmt"
	"wile/values"
)

var _ Operation = (*OperationLoadVoid)(nil)

type OperationLoadVoid struct {
}

func NewOperationLoadVoid() *OperationLoadVoid {
	return &OperationLoadVoid{}
}

func (p *OperationLoadVoid) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.value = MultipleValues{values.Void}
	mc.pc++
	return mc, nil
}

func (p *OperationLoadVoid) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-void>")
}

func (p *OperationLoadVoid) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadVoid) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadVoid)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
