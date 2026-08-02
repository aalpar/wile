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
//	               runtime frame from (frame.local, parent). This is what
//	               OpMakeClosure produces, i.e. every closure in a running
//	               program.
//	parent == nil  frame is a materialized runtime environment and ApplyParent
//	               falls back to frame.Parent(). NO PRODUCTION PRODUCER puts a
//	               closure in this state: all three callers of
//	               NewClosureWithTemplate (extensions/eval PrimCompile,
//	               machine/util.go, compilation/compile_syntax_rules.go) pass a
//	               frame built by NewEnvironmentFrameWithParent, which panics on
//	               a nil parent. Verified by panicking in that arm and running
//	               the whole suite green.
//
// So a nil ApplyParent means the frame was RELEASED while this closure still
// held it — ResetForPool zeroes the parent. Apply treats that as a fault rather
// than a representation; see machine_context_apply.go.
//
// An earlier revision of this comment justified the late frame.Parent() read as
// load-bearing, because (compile ...) captured a pooled frame whose parent went
// nil between capture and call. That was the use-after-release bug fixed in this
// same arc (PrimCompile now captures a stable child of MutableRuntime), so the
// justification is gone even though the late read is retained as the cheap way
// to detect the fault.
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

// EqualTo is OBJECT IDENTITY, and must stay that way.
//
// R7RS §6.1 requires equal? to agree with eqv? on procedures, and equal?
// reaches this method as its leaf comparison (values.Equal -> EqualTo), so any
// field-wise comparison here is an equal?/eqv? divergence at the Scheme level.
// It also decides member and assoc, which the stdlib defines over equal?.
//
// Field-wise comparison is not merely risky, it cannot work: frame is the
// lambda's compile-time frame, a template constant shared by every evaluation,
// and parent is the activation. Two closures built by one lambda form in one
// activation therefore agree on every field while being distinct procedures.
// That is not hypothetical — it is reachable from a tail loop or a call/cc
// re-entry, and it made (equal? a b) answer #t where (eqv? a b) answered #f.
//
// This read as safe before the shape+parent split only because each closure
// then carried a freshly allocated runtime frame, which made a field compare
// accidentally equivalent to identity. The accident is gone; the guarantee is
// now explicit.
func (p *MachineClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*MachineClosure)
	if !ok {
		return false
	}
	return p == v
}
