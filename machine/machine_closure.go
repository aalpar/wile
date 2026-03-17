// Copyright 2026 Aaron Alpar
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
	"slices"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

var _ values.Callable = (*MachineClosure)(nil)

// MachineClosure is a callable pairing compiled code with its lexical
// environment at definition time.
//
//	closure = ⟨λ, E, FV⟩, where:
//	  λ  = template  — compiled bytecode (NativeTemplate)
//	  E  = env       — pointer to enclosing EnvironmentFrame (parameter shape)
//	  FV = freeVars  — captured free variables (nil when no captures)
//
// Two representations coexist:
//   - Zero-capture closures: freeVars is nil, env provides parameter
//     bindings only. Created by OpMakeClosure.
//   - Flat closures: freeVars holds captured values, env provides
//     parameter shape. Created by OpMakeFlatClosure.
//
// Both representations use the same Apply path. The env frame is always
// allocated fresh (via InitApplyFrame) for non-noCopyApply closures;
// binding values are NOT copied because bindArgs and body opcodes
// initialize all slots.
//
// See BIBLIOGRAPHY.md "Linked Closure Representation".
type MachineClosure struct {
	env      *environment.EnvironmentFrame
	freeVars []values.Value // nil for zero-capture closures; populated for flat closures
	template *NativeTemplate
}

func (p *MachineClosure) closureMarker() {
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

// Env returns the closure's captured environment.
func (p *MachineClosure) Env() *environment.EnvironmentFrame {
	return p.env
}

// FreeVars returns the flat closure's captured free variable array,
// or nil for linked closures.
func (p *MachineClosure) FreeVars() []values.Value {
	return p.freeVars
}

// NewClosureWithFreeVars creates a flat closure with a captured free variable
// array and no linked environment. The freeVars slice is owned by the closure.
func NewClosureWithFreeVars(tpl *NativeTemplate, freeVars []values.Value) *MachineClosure {
	q := &MachineClosure{
		freeVars: freeVars,
		template: tpl,
	}
	return q
}

func (p *MachineClosure) Copy() *MachineClosure {
	q := &MachineClosure{
		template: p.template,
		freeVars: slices.Clone(p.freeVars),
	}
	if p.env != nil {
		q.env = p.env.Copy()
	}
	return q
}

func (p *MachineClosure) IsVoid() bool {
	return p == nil
}

func (p *MachineClosure) SchemeString() string {
	return "#<machine-closure>"
}

// AcceptsArity reports whether this closure can be called with n arguments.
// Fixed-arity closures require exactly paramCount args; variadic closures
// require at least paramCount-1 (the rest parameter collects the remainder).
func (p *MachineClosure) AcceptsArity(n int) bool {
	tpl := p.template
	if tpl.IsVariadic() {
		return n >= tpl.ParameterCount()-1
	}
	return n == tpl.ParameterCount()
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
