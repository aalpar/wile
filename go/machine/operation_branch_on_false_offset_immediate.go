package machine

import (
	"context"
	"fmt"
	"skeme/values"
)

type OperationBranchOnFalseOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOnFalseOffsetImmediate(offset int) *OperationBranchOnFalseOffsetImmediate {
	return &OperationBranchOnFalseOffsetImmediate{
		Offset: offset,
	}
}

func (p *OperationBranchOnFalseOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-false-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnFalseOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOnFalseOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnFalseOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}

func (p *OperationBranchOnFalseOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	v := mc.evals.Pop()
	if values.EqualTo(v, values.FalseValue) {
		mc.pc += p.Offset
	} else {
		mc.pc++
	}
	return mc, nil
}
