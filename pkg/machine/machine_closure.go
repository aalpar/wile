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

// MachineClosure is a flat closure: compiled code, the values of its free
// variables, and a static link for global resolution.
//
//	closure = ⟨λ, free, link⟩, where:
//	  λ    = template — compiled bytecode (NativeTemplate)
//	  free = the free variables' values, in the template's FreeNames order
//	  link = the environment the apply frame hangs from
//
//	Access cost: O(free) creation (copy the values off the eval stack), O(1)
//	  free-variable read (index the vector). Linked closures invert this trade-off.
//
//	Invariant: a slot the template's FreeBoxed marks holds a *values.Box shared
//	  with every other holder, so an assignment stays visible through the copy.
//	Constrains: OperationMakeClosure (pushes free values in slot order, picks
//	  link via closureLink), Apply (builds a fresh frame from the template's shape
//	  and link on every call, to prevent aliasing across recursive calls and
//	  SRFI-18 thread races, and installs free as the running vector).
//	Constrained by: the boxing pass (compilation/boxing.go), which decides which
//	  slots are cells; CESK model (link supplies the E component's globals).
//
// See BIBLIOGRAPHY.md "Linked Closure Representation" for the model this
// replaced.
//
// The local parameter shape is not a field: it is a property of the compiled
// body, so it rides on the template (NativeTemplate.shape) and every closure
// over a lambda agrees on it. The apply frame is never materialized here either;
// Apply combines shape and link straight into a pooled frame.
//
// BOTH constructors record a non-nil link, so there is one representation and
// no nil branch discriminating a second: OpMakeClosure passes
// closureLink(mc.env, tpl), and NewClosureWithTemplate reads env.Parent() off
// the frame it is handed. A nil link is unreachable from production —
// NewClosureCapturing panics on one, and both NewClosureWithTemplate callers
// (extensions/eval PrimCompile, compilation/compile_syntax_rules.go
// createTransformerClosure) pass a frame from NewEnvironmentFrameWithParent,
// which panics on a nil parent. Apply faults on it anyway rather than running
// with no global, namespace or phases.
//
// Reading the link eagerly gives up one check an earlier late frame.Parent()
// read bought: a frame RELEASED after the closure was built zeroes its own
// parent, and this closure no longer notices. That check only ever covered the
// two NewClosureWithTemplate sites, and both of them now build a fresh frame
// instead of borrowing a pooled one, which is what actually closed the
// (compile ...) use-after-release the check was standing in for.
type MachineClosure struct {
	// link is the STATIC LINK: the environment the apply frame hangs from, for
	// GLOBAL resolution and for the namespace/phase/store the frame derives from
	// it. It is one pointer to a view, not a chain to be walked: free-variable
	// lookup no longer travels it.
	//
	// closureLink picks it: the lexical root at the creating frame's own phase,
	// or the creating frame itself for a template that RetainsLexicalEnv. Never
	// Runtime(), because phaseLevel is a RELATIVE index on the owner's macro
	// tower and the apply frame inherits the creating frame's level through this
	// pointer. A link pinned to Runtime() would collapse the tower for every
	// closure built above phase 0.
	link     *environment.EnvironmentFrame
	template *NativeTemplate
	// free holds the VALUES of the closure's free variables, in the slot order
	// compileClosureBody fixed (NativeTemplate.FreeNames names them, FreeBoxed
	// says which are cells). A boxed entry holds the *values.Box, shared with
	// every other holder.
	//
	// nil for a closure that captures nothing, and for the two closures built
	// over an already-materialized environment (createTransformerClosure,
	// PrimCompile) — neither has a free VARIABLE, only an environment for global
	// resolution, which is what link is.
	//
	// This is what makes "no closure built by OpMakeClosure points at a frame"
	// true by construction. It does NOT extend to call/cc: a captured
	// continuation still holds mc.env (NewMachineContinuation), and nothing in
	// this design changes that.
	free []values.Value
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
		link:     env.Parent(),
		template: tpl,
	}
	return q
}

// Link returns the static link: the environment the closure's apply frame hangs
// from. A nil result is not a shape, it is a closure that recorded no
// environment; see the type comment.
func (p *MachineClosure) Link() *environment.EnvironmentFrame {
	return p.link
}

// Free returns the closure's free-variable vector, in the slot order the
// template's FreeNames name. nil for a closure that captures nothing. The slice
// is the closure's own — treat it as read-only.
func (p *MachineClosure) Free() []values.Value {
	return p.free
}

// NewClosureCapturing builds a closure over a template whose shape is already
// recorded, its static link, and its free-variable vector, without
// materializing the frame shape and link would combine into. This is the
// OpMakeClosure path. link must be non-nil: a nil one would leave the closure
// with no environment to resolve globals through, and the compile-time frame reachable through
// the template holds placeholders, so any fallback would be a wrong answer
// rather than a crash.
func NewClosureCapturing(tpl *NativeTemplate, link *environment.EnvironmentFrame, free []values.Value) *MachineClosure {
	if link == nil {
		panic(werr.WrapForeignErrorf(werr.ErrNilParentEnvironment,
			"NewClosureCapturing: nil static link; a captured closure must record the environment it resolves globals through"))
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
		link:     link,
		template: tpl,
		free:     free,
	}
	return q
}

func (p *MachineClosure) Template() *NativeTemplate {
	return p.template
}

// Env materializes the environment from the template's shape and the static
// link. Callers get a fresh frame each time rather than a shared one, so this
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
	if p.link == nil || shape == nil {
		return nil
	}
	return environment.NewEnvironmentFrameWithParent(shape.LocalEnvironment(), p.link)
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
// Field-wise comparison is not merely risky, it cannot work: template is a
// constant shared by every evaluation of the lambda, and link and the free
// values can coincide across evaluations. Two closures built by one lambda form
// can therefore agree on every field while being distinct procedures.
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
