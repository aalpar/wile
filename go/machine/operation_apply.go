package machine

import (
	"context"
	"wile/values"
)

type OperationApply struct {
}

func NewOperationApply() *OperationApply {
	return &OperationApply{}
}

func (p *OperationApply) SchemeString() string {
	return "#<operation-apply>"
}

func (p *OperationApply) IsVoid() bool {
	return p == nil
}

func (p *OperationApply) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationApply)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}

// FIXME: needs unit tests
func (p *OperationApply) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	vs := mc.evals.PopAll()
	switch cls := mc.value[0].(type) {
	case *MachineClosure:
		return mc.Apply(cls, vs...)
	case *CaseLambdaClosure:
		return mc.ApplyCaseLambda(cls, vs...)
	default:
		return mc, values.WrapForeignErrorf(values.ErrNotAClosure, "expected a closure, got %s", mc.value[0].SchemeString())
	}
}
