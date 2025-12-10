package machine

import (
	"context"
	"fmt"
	"skeme/values"
)

type OperationBranchOnNotFalseOffsetImmediate struct {
	Offset int
}

func NewOperationBranchOnNotFalseOffsetImmediate(offset int) *OperationBranchOnNotFalseOffsetImmediate {
	return &OperationBranchOnNotFalseOffsetImmediate{
		Offset: offset,
	}
}

func (p *OperationBranchOnNotFalseOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-branch-on-not-false-offset-immediate %d>", p.Offset)
}

func (p *OperationBranchOnNotFalseOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationBranchOnNotFalseOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBranchOnNotFalseOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}

func (p *OperationBranchOnNotFalseOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	v := mc.evals.Pop()
	if !values.EqualTo(v, values.FalseValue) {
		mc.pc += p.Offset
	} else {
		mc.pc++
	}
	return mc, nil
}
