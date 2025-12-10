package machine

import (
	"context"
	"skeme/environment"
	"skeme/values"
)

type OperationMakeClosure struct {
}

func NewOperationMakeClosure() *OperationMakeClosure {
	return &OperationMakeClosure{}
}

func (p *OperationMakeClosure) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	env, ok := mc.evals.Pop().(*environment.EnvironmentFrame)
	if !ok {
		return mc, values.ErrNotALocalEnvironmentFrame
	}
	tpl, ok := mc.evals.Pop().(*NativeTemplate)
	if !ok {
		return mc, values.ErrNotAMachineTemplate
	}
	cls := NewClosureWithTemplate(tpl, env)
	mc.value = MultipleValues{cls}
	mc.pc++
	return mc, nil
}

func (p *OperationMakeClosure) SchemeString() string {
	return "#<machine-operation-make-closure>"
}

func (p *OperationMakeClosure) IsVoid() bool {
	return p == nil
}

func (p *OperationMakeClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeClosure)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return true
}
