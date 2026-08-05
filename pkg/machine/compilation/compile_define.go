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

package compilation

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileValidatedDefine compiles a validated define form.
func CompileValidatedDefine(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedDefine)
	if v.IsFunction {
		// (define (name params...) body...) - compile as lambda then define
		return p.CompileValidatedDefineFn(ctctx, v)
	}
	// (define name expr) - compile value then store
	return p.compileValidatedDefineVar(ctctx, v)
}

// declareDefineBinding creates the binding for a define form before compiling its value.
// This early declaration enables self-recursive definitions like (define (fact n) ... (fact (- n 1)) ...).
// Returns the symbol for use by the caller when storing the compiled value.
func (p *CompileTimeContinuation) declareDefineBinding(v *validate.ValidatedDefine) (*values.Symbol, error) {
	// Get the symbol for the name (validator guarantees it's a SyntaxSymbol)
	sym := v.Name().Sym
	symbolScopes := v.Name().Scopes()
	symbolSource := v.Name().SourceContext()
	// Create binding early for recursion support
	if p.env.LocalEnvironment() != nil {
		li, _ := p.env.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, symbolScopes, symbolSource)
		// An internal define allocates its slot here, after compileClosureBody's
		// parameter-count check has already run, so this is the only place a body
		// define's slot can be rejected before a Load/StoreLocal is emitted for it.
		if li != nil {
			err := checkLocalSlotCapacity(li.Over()+1, "define")
			if err != nil {
				return nil, err
			}
		}
		return sym, nil
	}
	// The binder's own scope set is its hygiene identity in the global frame: a
	// user-written top-level binder carries an empty set, a macro-introduced one
	// carries the expansion's intro scope, so they land in different slots and
	// neither can capture the other. No renaming required.
	_, created := p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, symbolScopes)

	// Top-level immutability (WithImmutableTopLevel, the engine default): when enabled, a
	// defined-once, never-set!-in-unit top-level define is rebind-stable, and a
	// later redefine of an already-stable binding is rejected. When disabled,
	// behavior is identical to before (the fast path below still fires).
	//
	// Q3 (layered-environment): enforce in the namespace's own user runtime global
	// (the per-Engine user top-level) AND in its sealed base (immutable primitives +
	// bootstrap procedures — the optimizer's Stable anchors; capture-safe procedures like
	// zero?/not must stay Stable here or the frame-reclaim classifier stops trusting them).
	// EXEMPT user-loaded libraries: a library body compiles against a NewChildRuntime
	// frame that shares its parent's namespace but owns its own seal (so it is neither
	// ns.Runtime() nor ns.SealedBase()), keeping a library's cross-form
	// (define x)/(set! x) mutable (R2).
	ns := p.env.Namespace()
	immTop := ns != nil && ns.ImmutableTopLevel() &&
		(ns.Runtime() == p.env || ns.SealedBase() == p.env)

	if created && len(symbolScopes) == 0 && symbolSource == nil && !immTop {
		return sym, nil
	}
	// Address the binding just created, not a parent-chain walk. GetGlobalBinding
	// resolves a creation-returned (deferred, scope-less) index wildcard, i.e. to
	// the name's FIRST slot in this frame — for a macro-introduced binder that is
	// the user's variable, whose meta this would then overwrite. Mirror of
	// emitDefineStore's re-resolve.
	own := p.env.GlobalEnvironment()
	ownIndex := own.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(symbolScopes))
	if ownIndex == nil {
		return sym, nil
	}
	binding := own.GetOwnGlobalBinding(ownIndex)
	if binding == nil {
		return sym, nil
	}
	if immTop {
		// Redefine guard: a second top-level define of an already-stable binding
		// would rebind a procedure the frame optimizer may have proven stable in
		// an earlier unit. Reject it (defined-once across units). Keyed on the
		// Stable field specifically, NOT IsStable(): an imported binding (Stable
		// field false) must still be supersedable by a define (R7RS §5.3.1).
		m := binding.Meta()
		if !created && m != nil && m.Stable {
			return nil, werr.WrapForeignErrorf(
				werr.ErrImmutableBinding,
				"define: cannot redefine immutable top-level binding %q",
				sym.Key,
			)
		}
	}
	// This binding is a shared global under concurrent SRFI-18 compiles, so the
	// metadata write goes through UpdateMeta (copy-on-write CAS) rather than a
	// write-through pointer that a lock-free reader could tear.
	binding.UpdateMeta(func(m *environment.BindingMeta) bool {
		// Top-level define supersedes an imported binding (R7RS §5.3.1): the
		// define overwrites the value in the same slot, so drop the import
		// *provenance* and a subsequent set! on this binding is permitted.
		m.Imported = false
		m.Scopes = symbolScopes
		if symbolSource != nil {
			m.Source = symbolSource
		}
		if immTop {
			// Discharge the rebind-stability conclusion from in-unit evidence:
			// the language now forbids the cross-unit set!/redefine that
			// StableInUnit alone could not rule out (the set! gate + the
			// redefine guard above), so StableInUnit becomes a sound basis for
			// Stable. Off by default.
			m.Stable = v.StableInUnit
		}
		return true
	})
	return sym, nil
}

// compileValidatedDefineVar compiles the simple variable form of define.
//
// Usage: (define name expr)
//
// Examples:
//
//	(define x 42)                    ; bind x to 42
//	(define pi 3.14159)              ; bind pi to a float
//	(define greet (lambda (n) ...))  ; bind greet to a lambda (not function shorthand)
//
// Unlike the function shorthand (define (name ...) ...), this form does NOT
// create the binding before evaluating expr. This means self-reference in expr
// will fail: (define x (+ x 1)) is an error if x is not already defined.
//
// For the function shorthand form, see CompileValidatedDefineFn.
func (p *CompileTimeContinuation) compileValidatedDefineVar(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
	// Step 1: Declare the binding in the environment.
	// For variable defines, this still happens early, but since the value expression
	// cannot reference itself (unlike function defines), the order is less critical.
	// The binding is created but not yet populated with a value.
	sym, err := p.declareDefineBinding(v)
	if err != nil {
		return err
	}

	// Step 2: Compile the value expression.
	// The expression is NOT in tail position because define is a definition, not
	// an expression that returns a meaningful value. The result goes into the
	// value register, ready to be stored.
	err = p.compileValidated(ctctx.NotInTail(), v.SubExp())
	if err != nil {
		return err
	}

	// Step 3: Store the compiled value into the binding and load void.
	// After this, the binding holds the value and the value register contains void
	// (since define returns an unspecified value per R7RS).
	return p.emitDefineStore(sym, v.Name().Scopes())
}

// emitDefineStore emits bytecode to store the compiled value into the defined binding.
// Assumes the value to store is in the value register. Emits push, store (local or global),
// and loads void since define returns an unspecified value per R7RS.
//
// This is the final phase of define compilation, shared by both compileValidatedDefineVar
// and CompileValidatedDefineFn. The caller has already:
//  1. Declared the binding via declareDefineBinding
//  2. Compiled the value expression (leaving result in value register)
func (p *CompileTimeContinuation) emitDefineStore(sym *values.Symbol, scopes []*syntax.Scope) error {
	// Push the value from the value register to the eval stack.
	// Store operations consume from the stack, not the value register.
	p.AppendOperations(machine.NewOperationPush())

	if p.env.LocalEnvironment() != nil {
		// Local context (inside a lambda body): store to local variable slot.
		// The binding was already declared by declareDefineBinding (scope-aware),
		// so retrieve its slot scope-aware too. A bare-name lookup would send two
		// hygienically-distinct same-named internal defines to slot 0, leaving the
		// second slot #!void (the internal-define analogue of the let-store bug).
		li := p.env.GetLocalIndex(sym, syntax.ScopesOf(scopes))
		if li == nil {
			return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
				"compile define: binding %q not found in local environment", sym.Key)
		}
		p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
	} else {
		// Global context (top-level): store to global environment.
		// Global indices are stored in the literals pool since they're runtime values.
		// The operation loads the index from literals and stores the value there.
		// Mirror of the local branch above: the binding was declared scope-aware,
		// so address it scope-aware too. CreateGlobalBinding hands back a DEFERRED
		// index (no frame, no scopes), which OpStoreGlobal would resolve wildcard
		// to the name's first slot — the user's binding, not the macro's. Re-resolve
		// to get the pinned (frame, slot) pair for the binding just declared.
		p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, scopes)
		gi := p.env.GetGlobalIndexWithScopes(sym, syntax.ScopesOf(scopes))
		if gi == nil {
			return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
				"compile define: binding %q not found in global environment", sym.Key)
		}
		liti := p.template.MaybeAppendLiteral(gi)
		p.AppendOperations(machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti))
	}

	// Load void into the value register as define's return value.
	// Per R7RS 5.3.1: "The result of a definition is unspecified."
	// We use void to represent this unspecified value.
	p.AppendOperations(machine.NewOperationLoadVoid())
	return nil
}

// CompileValidatedDefineFn compiles the function shorthand form of define.
//
// Usage: (define (name param ...) body ...)
//
//	(define (name param ... . rest) body ...)
//
// This is syntactic sugar equivalent to:
//
//	(define name (lambda (param ...) body ...))
//
// Examples:
//
//	(define (square x) (* x x))           ; fixed arity
//	(define (sum . args) (apply + args))  ; variadic (all args)
//	(define (sum x . rest) (apply + x rest))  ; variadic (1+ args)
//
// The function name is bound before compiling the body to enable self-recursion:
//
//	(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))
func (p *CompileTimeContinuation) CompileValidatedDefineFn(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) error {
	// Step 1: Declare the binding early for self-recursion support.
	// This must happen before compiling the body so that references to the
	// function name within the body resolve correctly.
	sym, err := p.declareDefineBinding(v)
	if err != nil {
		return err
	}

	// Step 1.5: Stamp CaptureSafe when CALLING this procedure can never capture the
	// caller's continuation (its body runs no capture operator and all its callees are
	// capture-safe). This lets the frame-reclaim classifier trust a proven capture-safe
	// Scheme procedure — stdlib (zero?, not) or a user helper — as a callee, exactly
	// like a capture-safe primitive, recovering the coverage the retired name whitelist
	// hand-listed. CaptureSafe is a static capability stamped unconditionally; the
	// classifier still pairs it with IsStable(), so a mutable/rebindable define is not
	// trusted. Sound and conservative: a false verdict (e.g. a forward reference to a
	// not-yet-stamped sibling) only forgoes the optimization.
	name := v.Name()
	binding := p.env.GetBinding(name.Sym, syntax.ScopesOf(name.Scopes()))
	if binding != nil && validate.ProcedureBodyIsCaptureSafe(v, name, p.env) {
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			m.CaptureSafe = true
			return true
		})
	}

	// Step 2: Set up the closure's environment and bytecode template.
	// The local environment holds parameter bindings; compileClosure creates
	// the child environment frame after binding parameters.
	lenv := environment.NewLocalEnvironment(0)
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Record the function name for stack traces and debugging.
	// This makes error messages show "(fact)" instead of "(anonymous)".
	tpl.SetName(sym.Key)

	// Step 3: Compile the closure - binds parameters, compiles body, emits MakeClosure.
	// After this, the closure is in the value register ready to be stored.
	err = p.compileClosure(ctctx, tpl, lenv, v, p.frameReuseForDefine(ctctx, v))
	if err != nil {
		return err
	}

	// Step 4: Store the closure in the binding and load void as the result.
	// define returns an unspecified value per R7RS; we use void.
	return p.emitDefineStore(sym, v.Name().Scopes())
}

// frameReuseForDefine returns the frame-reuse disposition for a function-form
// define. At most one mode is armed; self-tail (in-place rebind) takes precedence
// over release when a define qualifies for both — the only cost is forgoing the
// once-per-invocation release of a base-case general tail call, negligible vs. the
// per-iteration in-place reuse.
//
//   - frameReuseSelfTail requires BOTH a self-tail-reusable body
//     (validate.BodyIsSelfTailReusable: no capture/escape, non-variadic, no in-body
//     set! of the name, a depth-0 tail self call) AND a Stable binding — the op
//     hardcodes a jump to pc=0, sound only if the name can never be rebound. A
//     top-level define is Stable only under WithImmutableTopLevel; an internal
//     define resolves to a non-Stable local here and is excluded (also sound — a
//     sibling could set! it, which the in-body predicate cannot see).
//   - frameReuseRelease needs NO IsStable() check: OpReleaseEnvFrame does a normal
//     apply (re-resolving the callee), so the define's own rebindability is
//     irrelevant; only the body's capture/escape/callee-safety matters, which
//     validate.BodyIsFrameReleasable checks (callee stability enforced there).
func (p *CompileTimeContinuation) frameReuseForDefine(ctctx CompileTimeCallContext, v *validate.ValidatedDefine) frameReuse {
	name := v.Name()
	binding := p.env.GetBinding(name.Sym, syntax.ScopesOf(name.Scopes()))
	if binding != nil && binding.IsStable() && validate.BodyIsSelfTailReusable(v, name, p.env) {
		return selfTailReuse(name, len(v.Params().Required))
	}
	// Release path: the interprocedural unit verdict (covers mutual recursion and
	// define->define tail calls) UNION the per-body predicate (self-recursion over
	// capture-safe primitives; also the only signal for internal defines and
	// non-unit compiles, where the verdict is withheld and reads false). Both are
	// SOUND "safe" predicates, so their union is sound and can only ADD reclaimable
	// frames over the shipped per-body gate — no continuation suite that passed
	// before can regress.
	//
	// The verdict is read through unitFrameReclaimVerdict, NOT off the map, keyed by
	// this define's ScopedBindingKey (validate.ScopedBindingKeyOf(name)) — the same
	// scope-discriminated identity the classifier keyed the verdict on, an exact match
	// for the very define being compiled, so a macro-introduced define and a colliding
	// user define read distinct verdicts. The method's gate stays live while a let body
	// compiles on this same compiler; it is now an explicit tightening, since an
	// internal define's name carries the enclosing scope and so misses the top-level map.
	if p.unitFrameReclaimVerdict(validate.ScopedBindingKeyOf(name)) || validate.BodyIsFrameReleasable(v, name, p.env) {
		return releaseReuse()
	}
	// Third disjunct, for an INTERNAL define only: prove it against its letrec*
	// sibling group. BodyIsFrameReleasable above seeds only this define's own
	// parameters, so a call to a sibling resolves through a flat env, finds
	// nothing (the sibling is local), and refuses — which is why mutually
	// recursive internal defines allocated a frame per iteration. The group
	// predicate discharges that with the same co-induction the letrec-binding
	// form uses. ctctx.enclosingDefines is nil at top level, where there is no
	// group and the two disjuncts above are the whole story.
	if ctctx.enclosingDefines != nil && validate.InternalDefineFrameReleasable(v, ctctx.enclosingDefines, p.env) {
		return releaseReuse()
	}
	return noFrameReuse()
}
