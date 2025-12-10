package machine

import (
	"context"
	"fmt"
	"wile/values"
)

type OperationRestoreContinuation struct {
}

func NewOperationRestoreContinuation() *OperationRestoreContinuation {
	return &OperationRestoreContinuation{}
}

func (p *OperationRestoreContinuation) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	if mc.cont == nil {
		return nil, ErrMachineHalt
	}
	mc.Restore(mc.cont)
	return mc, nil
}

func (p *OperationRestoreContinuation) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-restore-continuation>")
}

func (p *OperationRestoreContinuation) IsVoid() bool {
	return p == nil
}

func (p *OperationRestoreContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationRestoreContinuation)
	if !ok {
		return false
	}
	return v == p
}
