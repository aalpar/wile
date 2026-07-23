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

// compile_let.go compiles all four binding forms (let, let*, letrec, letrec*)
// through a single entry point. The ValidatedLet.Kind field determines which
// bytecode pattern is emitted.
//
// Two orthogonal dimensions:
//   - Init compilation env: before OpPushEnv (let) vs after (let*, letrec, letrec*)
//   - Store order: all-then-store (let, letrec) vs sequential (let*, letrec*)

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// compileLetrecBindingInit compiles the i-th binding's init for a letrec/letrec*
// (and the desugared named-let). When the init is a lambda eligible for in-place
// self-tail reuse — its body is self-tail-reusable on the binding name and the
// name is immutable across the let — it is compiled with a self-tail context so
// the lambda's depth-0 tail self calls emit OpSelfTailCall. Otherwise the init
// compiles normally (non-tail). p.env is the let frame here, so the lambda body's
// self reference resolves to the letrec binding.
func (p *CompileTimeContinuation) compileLetrecBindingInit(ctctx CompileTimeCallContext, v *validate.ValidatedLet, i int) error {
	lam, isLam := v.Bindings[i].Init.(*validate.ValidatedLambda)
	if isLam {
		arity, ok := validate.LetBindingSelfTailReusable(v, i, p.env)
		if ok {
			name := v.Bindings[i].Name.Sym.Key
			lenv := environment.NewLocalEnvironment(0)
			tpl := machine.NewNativeTemplate(0, 0, false)
			tpl.SetName(name)
			return p.compileClosure(ctctx.NotInTail(), tpl, lenv, lam, selfTailReuse(name, arity))
		}
	}
	return p.compileValidated(ctctx.NotInTail(), v.Bindings[i].Init)
}

// CompileValidatedLet compiles all binding forms based on Kind.
//
// let:     <inits> Push... | OpPushEnv | StoreLocal(reverse) | body | OpPopEnv
// let*:    OpPushEnv | (init Push StoreLocal)... | body | OpPopEnv
// letrec:  OpPushEnv | <inits> Push... | StoreLocal(reverse) | body | OpPopEnv
// letrec*: OpPushEnv | (init Push StoreLocal)... | body | OpPopEnv
func CompileValidatedLet(p *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error {
	v := expr.(*validate.ValidatedLet)
	n := len(v.Bindings)

	// For plain let: compile inits BEFORE creating the env frame
	// (inits don't see bindings, so they're compiled in the parent env).
	if n > 0 && v.Kind == validate.LetKindLet {
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			p.AppendOperations(machine.NewOperationPush())
		}
	}

	// Build compile-time env with bindings. For let*: bindings are
	// deferred — added incrementally so each init only sees preceding
	// bindings (R7RS 4.2.2: let* is equivalent to nested lets).
	childEnv := p.createLetCompileEnv(v)

	registeredBIDs := p.registerInlineCandidates(childEnv, v.Bindings)
	defer p.unregisterInlineCandidates(registeredBIDs)

	savedEnv := p.env
	p.env = childEnv
	defer func() {
		p.env = savedEnv
	}()

	// Predeclare body defines to count total slots needed.
	p.predeclareBodyDefines(v.Body())

	// For let*: binding names aren't in the env yet, so add their
	// count to get the true total for OpPushEnv.
	totalSlots := len(childEnv.LocalEnvironment().Bindings())
	if v.Kind == validate.LetKindLetStar {
		totalSlots += n
	}

	err := checkLocalSlotCapacity(totalSlots, "let")
	if err != nil {
		return err
	}

	p.AppendOperations(machine.NewOperationPushEnv(totalSlots))

	// Emit init compilation + stores based on Kind.
	switch v.Kind {
	case validate.LetKindLet:
		// Inits already on stack — store in reverse (LIFO). Resolve the slot
		// scope-aware: two hygienically-distinct same-named bindings occupy
		// separate slots, so a bare-name (nil-scope) lookup would send every
		// store to slot 0 and leave the others #!void.
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym, syntax.ScopesOf(v.Bindings[i].Name.Scopes()))
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile let: binding %q not found in local environment",
					v.Bindings[i].Name.Sym)
			}
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetStar:
		// Sequential with incremental visibility: compile init (sees
		// only preceding bindings), THEN register the name so subsequent
		// inits and the body can see it.
		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				return err
			}
			childEnv.MaybeCreateLocalBinding(
				b.Name.Sym,
				environment.BindingTypeVariable,
				b.Name.Scopes(),
				b.Name.SourceContext(),
			)
			li := childEnv.GetLocalIndex(b.Name.Sym, syntax.ScopesOf(b.Name.Scopes()))
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile let*: binding %q not found in local environment",
					b.Name.Sym)
			}
			p.AppendOperations(machine.NewOperationPush())
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetrecStar:
		// Sequential: all bindings visible from the start (letrec*
		// region includes all inits). Bindings already in env.
		for i := range v.Bindings {
			b := v.Bindings[i]
			err := p.compileLetrecBindingInit(ctctx, v, i)
			if err != nil {
				return err
			}
			li := childEnv.GetLocalIndex(b.Name.Sym, syntax.ScopesOf(b.Name.Scopes()))
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile letrec*: binding %q not found in local environment",
					b.Name.Sym)
			}
			p.AppendOperations(machine.NewOperationPush())
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}

	case validate.LetKindLetrec:
		// Delayed assignment: compile all inits, push all, then store all.
		for i := range v.Bindings {
			err := p.compileLetrecBindingInit(ctctx, v, i)
			if err != nil {
				return err
			}
			p.AppendOperations(machine.NewOperationPush())
		}
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym, syntax.ScopesOf(v.Bindings[i].Name.Scopes()))
			if li == nil {
				return werr.WrapForeignErrorf(machine.ErrBindingNotFound,
					"compile letrec: binding %q not found in local environment",
					v.Bindings[i].Name.Sym)
			}
			p.AppendOperations(machine.NewOperationStoreLocalByLocalIndexImmediate(li))
		}
	}

	// The let body runs in the pushed frame (OpPushEnv above), so it is no longer
	// at the enclosing closure's parameter-frame depth: clear any self-tail context
	// so a self call inside the body is NOT rewritten to the in-place OpSelfTailCall
	// (which rebinds the parameter frame). Tail position itself is preserved.
	err = p.compileValidatedSequence(ctctx.WithoutFrameReuse(), v.Body())
	if err != nil {
		return err
	}

	if !ctctx.inTail {
		p.AppendOperations(machine.NewOperationPopEnv())
	}

	return nil
}

// createLetCompileEnv creates a compile-time child environment with all
// let bindings as local variables.
func (p *CompileTimeContinuation) createLetCompileEnv(
	v *validate.ValidatedLet,
) *environment.EnvironmentFrame {
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

	// For let*: DON'T add bindings here. They are added incrementally
	// during compilation so each init only sees preceding bindings
	// (R7RS 4.2.2). For all other kinds, add all bindings upfront.
	if v.Kind == validate.LetKindLetStar {
		return childEnv
	}
	for _, b := range v.Bindings {
		childEnv.MaybeCreateLocalBinding(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
		if b.CaptureSafe {
			stampLetBindingCaptureSafe(childEnv, b)
		}
	}
	return childEnv
}

// stampLetBindingCaptureSafe stamps a freshly-created let binding CaptureSafe and
// Stable (callback specialization Strategy A, unify path). The inline-HOF dispatch
// sets ValidatedLetBinding.CaptureSafe only for a callback it has already proven
// capture-safe (CallbackIsCaptureSafe), so the inlined loop calling this binding
// passes bodyCalleesAllCaptureSafe and reclaims. Stable holds because the synthetic
// let never set!s the binding; the proof required the callback's own stability.
func stampLetBindingCaptureSafe(childEnv *environment.EnvironmentFrame, b validate.ValidatedLetBinding) {
	bound := childEnv.GetBinding(b.Name.Sym, syntax.ScopesOf(b.Name.Scopes()))
	if bound == nil {
		return
	}
	bound.UpdateMeta(func(m *environment.BindingMeta) bool {
		m.CaptureSafe = true
		m.Stable = true
		return true
	})
}

// predeclareBodyDefines scans the body for define forms and pre-creates
// their bindings in the current compile-time env. Unwraps begin blocks
// to find defines from macro-expanded forms like define-values.
func (p *CompileTimeContinuation) predeclareBodyDefines(
	body []validate.ValidatedExpr,
) {
	for _, expr := range body {
		p.predeclareDefineFromValidatedRecursive(expr)
	}
}

// predeclareDefineFromValidatedRecursive pre-creates bindings for defines,
// recursing into begin blocks to find defines from macro expansions
// (e.g., define-values expands to (begin (define ...) ...)).
func (p *CompileTimeContinuation) predeclareDefineFromValidatedRecursive(
	expr validate.ValidatedExpr,
) {
	switch v := expr.(type) {
	case *validate.ValidatedDefine:
		predeclareBinding(p.env, v.Name().Sym, v.Name().Scopes(), v.Name().SourceContext())
	case *validate.ValidatedBegin:
		for _, sub := range v.Body() {
			p.predeclareDefineFromValidatedRecursive(sub)
		}
	}
}

// registerInlineCandidates scans let bindings for lambdas eligible for
// call-site inlining. Returns the BindingIDs that were registered so
// the caller can unregister them when the let scope exits.
func (p *CompileTimeContinuation) registerInlineCandidates(
	childEnv *environment.EnvironmentFrame,
	bindings []validate.ValidatedLetBinding,
) []environment.BindingID {
	if p.inlineThreshold == 0 {
		return nil
	}

	var registered []environment.BindingID
	for i := range bindings {
		b := &bindings[i]
		if b.Mutable || b.Escapes {
			continue
		}

		lam, ok := b.Init.(*validate.ValidatedLambda)
		if !ok {
			continue
		}

		params := lam.Params()
		if params.Rest != nil {
			continue
		}

		if len(lam.Body()) > p.inlineThreshold {
			continue
		}

		bid, resolved := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if !resolved {
			continue
		}

		if p.inlineCandidates == nil {
			p.inlineCandidates = make(map[environment.BindingID]inlineCandidate)
		}
		p.inlineCandidates[bid] = inlineCandidate{
			lambda: lam,
			env:    childEnv,
		}
		registered = append(registered, bid)
	}
	return registered
}

// unregisterInlineCandidates removes previously registered inline candidates
// when the enclosing let scope exits.
func (p *CompileTimeContinuation) unregisterInlineCandidates(bids []environment.BindingID) {
	for _, bid := range bids {
		delete(p.inlineCandidates, bid)
	}
}
