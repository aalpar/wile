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
// The captured environment is stored as a (frame, parent) PAIR rather than as
// one materialized frame, and which of the two is authoritative is decided by
// parent:
//
//	parent != nil  frame is the lambda's COMPILE-TIME frame, a template
//	               constant shared by every evaluation of this lambda, and it
//	               contributes only the local parameter shape. Apply builds the
//	               runtime frame from (frame.local, parent).
//	parent == nil  frame is a fully materialized runtime environment and Apply
//	               reads frame.Parent() itself, LATE. Produced by (compile ...)
//	               and the top-level thunk wrapper.
//
// The late read in the second case is load-bearing, not stylistic. (compile ...)
// captures mc.EnvironmentFrame(), and that frame's parent is observably non-nil
// at capture and nil by the time the thunk runs, so a parent snapshotted at
// construction sends the thunk down the copying branch and it faults on a frame
// with no locals. Anything that caches frame.Parent() early reintroduces that.
//
// Splitting the pair is what makes closure creation a single allocation.
// Materializing (frame.local, parent) into an EnvironmentFrame at creation time
// cost an extra 80-byte object per evaluated lambda, and every consumer then
// took it apart again: InitApplyFrame reads exactly p.local and p.parent, and
// derives global/phase/namespace from the parent. The frame was a carrier, not
// an environment. Note the aliasing is unchanged by this: the old
// NewEnvironmentFrameWithParent did `q.local = *local`, already sharing the
// compile-time frame's keys map and bindings array.
//
// Carrying both fields costs a word: the struct is 24 bytes where it was 16,
// which measured as +1.3% on call-bound code that never builds a closure. That
// is accepted, and the candidate for recovering it (move the parent == nil case
// to its own Closure implementation) is filed under Tier 4 in TODO.md, together
// with the late-Parent constraint any such split has to preserve.
type MachineClosure struct {
	frame    *environment.EnvironmentFrame
	parent   *environment.EnvironmentFrame
	template *NativeTemplate
}

func (p *MachineClosure) closureMarker() {
}

// NewClosureWithTemplate builds a closure over an already-materialized
// environment, leaving parent nil so Apply reads env.Parent() late. See the
// type comment: reading it here instead would change behavior.
func NewClosureWithTemplate(tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineClosure {
	q := &MachineClosure{
		frame:    env,
		template: tpl,
	}
	return q
}

// ApplyParent returns the runtime parent the closure's apply frame hangs from,
// or nil when Apply must reuse the captured frame in place.
func (p *MachineClosure) ApplyParent() *environment.EnvironmentFrame {
	if p.parent != nil {
		return p.parent
	}
	return p.frame.Parent()
}

// NewClosureCapturing builds a closure from a lambda's compile-time frame and
// the runtime environment captured at creation, without materializing the
// intermediate frame the two would combine into. This is the OpMakeClosure
// path; parent must be non-nil, which mc.env always is.
func NewClosureCapturing(tpl *NativeTemplate, shape, parent *environment.EnvironmentFrame) *MachineClosure {
	q := &MachineClosure{
		frame:    shape,
		parent:   parent,
		template: tpl,
	}
	return q
}

func (p *MachineClosure) Template() *NativeTemplate {
	return p.template
}

// Env returns the closure's captured environment, materializing it when the
// closure holds the (compile-time shape, runtime parent) pair. Callers get a
// fresh frame each time rather than a shared one, so this is a reflection and
// debugging accessor, not an apply-path call: Apply goes straight to
// InitApplyFrameWithParent and never builds this.
func (p *MachineClosure) Env() *environment.EnvironmentFrame {
	if p.parent == nil {
		return p.frame
	}
	return environment.NewEnvironmentFrameWithParent(p.frame.LocalEnvironment(), p.parent)
}

// Frame returns the frame the closure captured: the compile-time shape when it
// holds a pair, the materialized environment otherwise. Unlike Env it never
// allocates, and unlike Env it is not by itself the closure's environment.
func (p *MachineClosure) Frame() *environment.EnvironmentFrame {
	return p.frame
}

func (p *MachineClosure) Copy() *MachineClosure {
	return NewClosureWithTemplate(p.template, p.Env().Copy())
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
	if p.frame != v.frame || p.parent != v.parent {
		return false
	}
	if p.template != v.template {
		return false
	}
	return true
}
