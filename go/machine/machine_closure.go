package machine

import (
	"skeme/environment"
	"skeme/values"
)

type MachineClosure struct {
	env      *environment.EnvironmentFrame
	template *NativeTemplate
}

func NewClosureWithTemplate(tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineClosure {
	q := &MachineClosure{
		env:      env,
		template: tpl,
	}
	return q
}

func (p *MachineClosure) Template() *NativeTemplate {
	return p.template
}

func (p *MachineClosure) Copy() *MachineClosure {
	q := &MachineClosure{
		env:      p.env.Copy(),
		template: p.template,
	}
	return q
}

func (p *MachineClosure) IsVoid() bool {
	return p == nil
}

func (p *MachineClosure) SchemeString() string {
	return "#<machine-closure>"
}

func (p *MachineClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*MachineClosure)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	if p.env != v.env {
		return false
	}
	if p.template != v.template {
		return false
	}
	return true
}
