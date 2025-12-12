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
	"fmt"
	"wile/environment"
	"wile/values"
	"slices"
)

type MachineContinuation struct {
	parent   *MachineContinuation
	env      *environment.EnvironmentFrame
	template *NativeTemplate
	value MultipleValues
	evals *Stack
	pc    int
}

// NewMachineContinuation creates a new machine continuation with the given parent, template, environment frame, and initial values.
func NewMachineContinuation(parent *MachineContinuation, tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineContinuation {
	q := &MachineContinuation{
		parent:   parent,
		env:      env,
		template: tpl,
		value:    NewMultipleValues(),
		evals:    NewStack(),
		pc:       0,
	}
	return q
}

// NewMachineContinuationFromMachineContext creates a new machine continuation from the given machine context and an offset to the program counter.
// The new continuation inherits the environment, template, and evaluation stack from the machine context.
func NewMachineContinuationFromMachineContext(mc *MachineContext, off int) *MachineContinuation {
	q := &MachineContinuation{
		parent:   mc.cont,
		env:      mc.env,
		template: mc.template,
		value:    mc.value,
		evals:    mc.evals,
		pc:       mc.pc + off,
	}
	return q
}

func (p *MachineContinuation) Parent() *MachineContinuation {
	return p.parent
}

func (p *MachineContinuation) EnvironmentFrame() *environment.EnvironmentFrame {
	return p.env
}

func (p *MachineContinuation) Template() *NativeTemplate {
	return p.template
}

func (p *MachineContinuation) PC() int {
	return p.pc
}

func (p *MachineContinuation) SetPC(v int) {
	p.pc = v
}

func (p *MachineContinuation) PushValues(v ...values.Value) {
	p.value = append(p.value, v...)
}

// CallDepth returns the depth of the continuation stack.
func (p *MachineContinuation) CallDepth() int {
	r := p
	if r == nil {
		return 0
	}
	q := 0
	// count the number of parents
	for r.parent != nil {
		q++
		r = r.parent
	}
	return q
}

func (p *MachineContinuation) Copy() *MachineContinuation {
	q := &MachineContinuation{
		parent:   p.parent,
		env:      p.env,
		template: p.template,
		value:    slices.Clone(p.value),
		evals:    p.evals.Copy(),
		pc:       p.pc,
	}
	return q
}

func (p *MachineContinuation) SchemeString() string {
	return fmt.Sprintf("<machine-continuation %%%d>", p.pc)
}

func (p *MachineContinuation) IsVoid() bool {
	return p == nil
}

func (p *MachineContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*MachineContinuation)
	if !ok {
		return false
	}
	if p == v {
		return true
	}
	if p == nil || v == nil {
		return p == v
	}
	if p.parent != v.parent {
		return false
	}
	if p.env == nil || v.env == nil {
		return p.env == v.env
	}
	if p.evals != v.evals {
		return false
	}
	if p.template != v.template {
		return false
	}
	if p.pc != v.pc {
		return false
	}
	return true
}
