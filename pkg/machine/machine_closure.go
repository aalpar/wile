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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

var (
	_ values.Callable = (*MachineClosure)(nil)
	_ NamedCallable   = (*MachineClosure)(nil)
)

// MachineClosure is a linked closure (Church 1936, Landin 1964, Cardelli 1983):
// a pair of compiled code and the lexical environment at definition time.
//
//	closure = ⟨λ, E⟩, where:
//	  λ = template  — compiled bytecode (NativeTemplate)
//	  E = env       — pointer to enclosing EnvironmentFrame
//
//	Access cost: O(1) creation (capture pointer), O(depth) free variable
//	  lookup (traverse parent chain). Flat closures invert this trade-off.
//
//	Invariant: E is a live pointer into the frame chain, not a copy.
//	  Mutations via set! are visible through the closure because the
//	  closure shares the frame, not a snapshot.
//	Constrains: OperationMakeClosure (must link E to runtime parent),
//	  Apply (copies E on every call with a parented env, to prevent aliasing
//	  and thread races; a parentless E, which by invariant has zero
//	  parameters, is reused in place).
//	Constrained by: de Bruijn addressing (free vars addressed by
//	  slot,depth in E's chain), CESK model (E is the environment component).
//
// See BIBLIOGRAPHY.md "Linked Closure Representation".
type MachineClosure struct {
	env      *environment.EnvironmentFrame
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

// Name returns the closure's name from its compiled template.
func (p *MachineClosure) Name() string {
	return p.template.Name()
}

// Doc returns the closure's documentation string from its compiled template.
func (p *MachineClosure) Doc() string {
	return p.template.Doc()
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
