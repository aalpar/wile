// Copyright 2025 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.


package machine

import (
	"wile/environment"
	"wile/values"
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
