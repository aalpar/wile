package machine

import (
	"context"
	"fmt"
	"wile/values"
)

type OperationBranchOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOffsetImmediate(offset int) *OperationBranchOffsetImmediate {
	return &OperationBranchOffsetImmediate{
		Offset: offset,
	}
}

func (p *OperationBranchOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}

func (p *OperationBranchOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	mc.pc += p.Offset
	return mc, nil
}
