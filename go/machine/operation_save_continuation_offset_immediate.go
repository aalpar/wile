package machine

import (
	"context"
	"fmt"
	"wile/values"
)

type OperationSaveContinuationOffsetImmediate struct {
	Offset int
}

func NewOperationSaveContinuationOffsetImmediate(off int) *OperationSaveContinuationOffsetImmediate {
	return &OperationSaveContinuationOffsetImmediate{
		Offset: off,
	}
}

func (p *OperationSaveContinuationOffsetImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	// copy the current continuation and push it onto the eval stack
	mc.SaveContinuation(p.Offset)
	mc.pc++
	return mc, nil
}

func (p *OperationSaveContinuationOffsetImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-save-continuation-offset-immediate %d>", p.Offset)
}

func (p *OperationSaveContinuationOffsetImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationSaveContinuationOffsetImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSaveContinuationOffsetImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.Offset == v.Offset
}
