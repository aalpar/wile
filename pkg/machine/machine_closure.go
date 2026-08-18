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
	"github.com/aalpar/wile/pkg/werr"
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
//	  E = (frame, parent) — the enclosing environment, kept as a pair
//
//	Access cost: O(1) creation (capture pointer), O(depth) free variable
//	  lookup (traverse parent chain). Flat closures invert this trade-off.
//
//	Invariant: E is a live pointer into the frame chain, not a copy.
//	  Mutations via set! are visible through the closure because the
//	  closure shares the frame, not a snapshot.
//	Constrains: OperationMakeClosure (must link E to runtime parent),
//	  Apply (builds a fresh frame from the pair on every call, to prevent
//	  aliasing across recursive calls and SRFI-18 thread races).
//	Constrained by: de Bruijn addressing (free vars addressed by
//	  slot,depth in E's chain), CESK model (E is the environment component).
//
// See BIBLIOGRAPHY.md "Linked Closure Representation".
//
// E is never materialized into one frame. Materializing it cost an extra
// 80-byte object per evaluated lambda that every consumer then took apart
// again: InitApplyFrame reads exactly p.local and p.parent and derives
// global/phase/namespace from the parent. The frame was a carrier, not an
// environment.
//
//	parent    the runtime environment captured at closure creation — the ONLY
//	          per-activation word, and the whole of what makes this a closure.
//	template  the compiled body, which also owns the local parameter shape
//	          (NativeTemplate.shape). One template, one shape: the shape is a
//	          property of the compiled body, so every closure over a lambda
//	          agrees on it and none of them need to carry it.
//
// The shape was briefly a third field here, which is why an intermediate design
// paid 24 bytes to avoid the 80-byte frame. It is now on the template and this
// type is back to two words.
//
// BOTH constructors capture parent eagerly, so there is one representation and
// no nil branch discriminating a second: OpMakeClosure passes mc.env, and
// NewClosureWithTemplate reads it off the frame it is handed. A nil parent is
// unreachable from production — NewClosureCapturing panics on one, and both
// NewClosureWithTemplate callers (extensions/eval PrimCompile,
// compilation/compile_syntax_rules.go createTransformerClosure) pass a frame
// from NewEnvironmentFrameWithParent, which panics on a nil parent. Apply
// faults on it anyway rather than running with no global, namespace or phases.
//
// Reading it eagerly gives up one check an earlier late frame.Parent() read
// bought: a frame RELEASED after the closure was built zeroes its own parent,
// and this closure no longer notices. That check only ever covered the two
// NewClosureWithTemplate sites — OpMakeClosure has always captured eagerly, and
// builds every closure a Scheme program makes — and both of them now build a
// fresh frame instead of borrowing a pooled one, which is what actually closed
// the (compile ...) use-after-release the check was standing in for. The live
// protection is mc.envPooled = false at OpMakeClosure, not a nil read.
type MachineClosure struct {
	parent   *environment.EnvironmentFrame
	template *NativeTemplate
}

func (p *MachineClosure) closureMarker() {
}

// NewClosureWithTemplate builds a closure over an already-materialized
// environment, for the two callers that build a template and its environment
// together rather than going through compileClosureBody. It splits env the same
// way OpMakeClosure does: the local half becomes the template's shape, the
// parent becomes the closure's captured environment. env must therefore have a
// parent, which NewEnvironmentFrameWithParent guarantees for both.
//
// Recording the shape on the template is a write to a shared object, so it is
// sound only because both callers pass a template they just built and have not
// yet published. A template already carrying a different shape is a caller
// error, not a case to merge.
func NewClosureWithTemplate(tpl *NativeTemplate, env *environment.EnvironmentFrame) *MachineClosure {
	tpl.SetShape(env)
	q := &MachineClosure{
		parent:   env.Parent(),
		template: tpl,
	}
	return q
}

// ApplyParent returns the runtime parent the closure's apply frame hangs from.
// A nil result is not a shape, it is a closure that recorded no environment;
// see the type comment.
func (p *MachineClosure) ApplyParent() *environment.EnvironmentFrame {
	return p.parent
}

// NewClosureCapturing builds a closure over a template whose shape is already
// recorded and the runtime environment captured at creation, without
// materializing the frame the two would combine into. This is the OpMakeClosure
// path. parent must be non-nil: a nil one would leave the closure with no
// record of what it closed over, and the compile-time frame reachable through
// the template holds placeholders, so any fallback would be a wrong answer
// rather than a crash.
func NewClosureCapturing(tpl *NativeTemplate, parent *environment.EnvironmentFrame) *MachineClosure {
	if parent == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNilParentEnvironment,
			"NewClosureCapturing: nil runtime parent; a captured closure must record the environment it closed over"))
	}
	// Checked here rather than in Apply: the shape is a property of the
	// template, so it is settled once per closure and does not need re-asking
	// per call. A template with no shape was never compiled as a closure body,
	// which makes this a producer error — the frame it would apply with does not
	// exist, so there is nothing to degrade to. Structural, not measured: moving
	// the guard here did not move BenchmarkParallelScalingCompute.
	if tpl.Shape() == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNotAMachineTemplate,
			"NewClosureCapturing: template records no local shape; it was not compiled as a closure body"))
	}
	q := &MachineClosure{
		parent:   parent,
		template: tpl,
	}
	return q
}

func (p *MachineClosure) Template() *NativeTemplate {
	return p.template
}

// Env materializes the environment from the template's shape and the captured
// parent. Callers get a fresh frame each time rather than a shared one, so this
// is a reflection and debugging accessor, not an apply-path call: Apply goes
// straight to InitApplyFrameWithParent and never builds this. The local half is
// copied by value (see EnvironmentFrame.local), so the result reads the same
// bindings, not a snapshot of them.
//
// nil for a closure that recorded no environment, or over a template with no
// shape — the same degenerate states Apply faults on, reported here as "no
// environment" because callers already treat a nil frame that way.
func (p *MachineClosure) Env() *environment.EnvironmentFrame {
	shape := p.template.Shape()
	if p.parent == nil || shape == nil {
		return nil
	}
	return environment.NewEnvironmentFrameWithParent(shape.LocalEnvironment(), p.parent)
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
