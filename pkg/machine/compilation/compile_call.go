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
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// emitProcAndArgs compiles the procedure expression then each argument,
// pushing the procedure and every argument onto the eval stack in order.
// Shared by the validated-call and validated-apply emit paths; the apply path
// flattens its final list onto the stack after this returns.
func (p *CompileTimeContinuation) emitProcAndArgs(ctctx CompileTimeCallContext, proc validate.ValidatedExpr, args []validate.ValidatedExpr) error {
	err := p.compileValidated(ctctx.NotInTail(), proc)
	if err != nil {
		return err
	}
	p.AppendOperations(machine.NewOperationPush())

	for _, arg := range args {
		err = p.compileValidated(ctctx.NotInTail(), arg)
		if err != nil {
			return err
		}
		p.appendPushAt(arg.Source())
	}
	return nil
}

// tryEmitSelfTailCall emits OpSelfTailCall for a tail call to the enclosing
// self-tail-reusable closure, returning (true, nil) if it did so.
//
// The selfTail context is armed only on a closure body proven safe (and, for a
// top-level self, Stable) and is dropped by NotInTail(), so a set selfTail at an
// inTail call site whose operator name and arity match the enclosing closure is
// exactly a tail self call. The emit evaluates the arguments (non-tail, pushing
// each onto the eval stack in slot order — old parameter values stay intact,
// making the op's drain-and-bind a parallel assignment), then the op unwinds any
// intervening let frames, rebinds the parameter slots and loops to pc=0.
//
// frameReuse.letDepth is the unwind count, and evaluating the arguments BEFORE it
// is the ordering that makes depth>0 work at all: the arguments are what read the
// let bindings, so a pop that ran first would read frames it had already left.
// The count is carried by the context rather than recomputed here, and it is
// incremented at exactly one site — see UnderLetFrame.
//
// This runs FIRST in compileValidatedCall, ahead of the three inline
// specializations, so a site that becomes a self-tail call cannot also be inlined.
func (p *CompileTimeContinuation) tryEmitSelfTailCall(ctctx CompileTimeCallContext, v *validate.ValidatedCall) (bool, error) {
	if !ctctx.inTail || ctctx.frameReuse.kind != frameReuseSelfTail {
		return false, nil
	}
	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}
	// IDENTITY, NOT SPELLING. This gate is the AUTHORITY for the rewrite: a false
	// positive here does not merely forgo an optimization, it turns a call to some
	// OTHER procedure into a jump to pc=0 (machine_context.go's OpSelfTailCall). The
	// safety predicates run post-expansion, so a macro-introduced binder and a user
	// identifier of the same name are distinguished by scopes alone — a template that
	// opens (let loop …) and tail-calls a pattern variable instantiated with the
	// user's own top-level `loop` reaches here with matching spelling and arity.
	//
	// Both sides resolve inside this one call, which is what makes comparing
	// *environment.Binding pointers sound: a local binding is a pointer into a slice
	// EnsureLocalBinding can reallocate, so the pointer is an identity only within a
	// window that contains no binding creation.
	//
	// self == nil should not happen — the arming site resolved this very symbol — but
	// it refuses rather than guesses, and the ratchet test is what would catch a
	// systematic refusal. op != self covers both "same spelling, different binding"
	// and "does not resolve at all".
	self := p.env.GetBinding(ctctx.frameReuse.selfSym.Sym, syntax.ScopesOf(ctctx.frameReuse.selfSym.Scopes()))
	op := p.env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	args := v.Body()
	if self == nil || op != self || len(args) != ctctx.frameReuse.arity {
		return false, nil
	}
	for _, arg := range args {
		err := p.compileValidated(ctctx.NotInTail(), arg)
		if err != nil {
			return false, err
		}
		p.appendPushAt(arg.Source())
	}
	p.AppendOperations(machine.NewOperationSelfTailCall(ctctx.frameReuse.arity, ctctx.frameReuse.letDepth))
	return true, nil
}

// compileValidatedCall compiles a validated function call (proc args...).
//
// Direct-style compilation (Dybvig 1987, Ch. 3). The compiler emits stack
// operations for intermediate values instead of CPS or A-normal form.
//
//	(f x y) compiles to:
//	  [if !tail: SaveContinuation]  — push σ onto K
//	  <compile f>   Push            — S = [f]
//	  <compile x>   Push            — S = [f, x]
//	  <compile y>   Push            — S = [f, x, y]
//	  Pull                          — value = f, S = [x, y]
//	  Apply                         — call f with args from S
//
//	where tail = ctctx.inTail (tracked by value through recursive calls).
//
//	Invariant: tail calls emit no SaveContinuation. K does not grow.
//	  This is what makes proper tail recursion work (Clinger 1998).
//	Constrains: CESK transitions (non-tail pushes σ onto K, tail reuses K),
//	  peephole optimizer (may fuse Pull+Apply → PullApply).
//	Constrained by: CompileTimeCallContext.inTail (set by compileBody
//	  for the last expression in a sequence).
//
// See BIBLIOGRAPHY.md "Direct-Style Compilation".
func (p *CompileTimeContinuation) compileValidatedCall(ctctx CompileTimeCallContext, v *validate.ValidatedCall) error {
	emitted, err := p.tryEmitSelfTailCall(ctctx, v)
	if err != nil {
		return err
	}
	if emitted {
		return nil
	}

	inlined, err := p.tryInlineCall(ctctx, v)
	if err != nil {
		return err
	}
	if inlined {
		return nil
	}

	inlinedHOF, err := p.tryInlineHOFCall(ctctx, v)
	if err != nil {
		return err
	}
	if inlinedHOF {
		return nil
	}

	// After the three specializations, which reach callees this cannot see and
	// carry their own arity checks where they need one (tryInlineCall reads the
	// let-bound lambda's AST, not a binding).
	err = p.checkCallArity(v)
	if err != nil {
		return err
	}

	var operationSaveContinuationIndex int
	if !ctctx.inTail {
		// Non-tail call: save continuation so we can return here after the call
		operationSaveContinuationIndex = p.emitPatchableSaveContinuation()
	}
	// Tail call: skip SaveContinuation - the callee will return directly to our caller

	// Compile the procedure and arguments, pushing each onto the stack
	err = p.emitProcAndArgs(ctctx, v.Proc(), v.Body())
	if err != nil {
		return err
	}

	// Reclaimable general tail call: the parameter frame is dead now (proc + args
	// are on the eval stack), and the enclosing body was proven frame-releasable
	// (no capture/escape, only capture-safe callees), so release it to the pool
	// before applying — the callee's acquire reuses it. Emitted after the args
	// (which still read the frame) and before Pull+Apply. The frameReuseRelease
	// disposition is depth-0 (cleared on let descent), so p.env is the parameter
	// frame. A frameReuseSelfTail body takes the OpSelfTailCall path above instead.
	if ctctx.inTail && ctctx.frameReuse.kind == frameReuseRelease {
		p.AppendOperations(machine.NewOperationReleaseEnvFrame())
	}

	// Pull the procedure and apply
	p.AppendOperations(
		machine.NewOperationPull(),
		machine.NewOperationApply(),
	)

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(operationSaveContinuationIndex)
	}

	return nil
}

// tryInlineCall checks whether a call can be inlined by substituting a
// let-bound lambda's body at the call site. Returns (true, nil) if the
// call was inlined, (false, nil) if inlining does not apply, or
// (false, err) on compilation failure.
func (p *CompileTimeContinuation) tryInlineCall(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedCall,
) (bool, error) {
	if p.inlineCandidates == nil {
		return false, nil
	}

	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}

	bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	if !resolved {
		return false, nil
	}

	candidate, found := p.inlineCandidates[bid]
	if !found {
		return false, nil
	}

	// Only inline when the call site is in the same scope as the
	// registration site. Nested scopes (e.g., let* with duplicate names)
	// may shadow free variables that the lambda captured at definition.
	if p.env != candidate.env {
		return false, nil
	}

	// Guard against recursive inlining.
	if p.currentlyInlining != nil {
		inlining := p.currentlyInlining.Get(bid)
		if inlining {
			return false, nil
		}
	}

	// Arity check: argument count must match parameter count exactly.
	// The binding is !Mutable (no set!) so the lambda's parameter count is
	// known at compile time. An arity mismatch is a guaranteed runtime error;
	// report it now instead of deferring to the VM.
	params := candidate.lambda.Params()
	args := v.Body()
	if len(args) != len(params.Required) {
		return false, werr.WrapForeignErrorf(
			werr.ErrWrongNumberOfArguments,
			"inline call to %s: expected %d argument(s), got %d",
			sym.Symbol.Sym, len(params.Required), len(args),
		)
	}

	// Build synthetic let bindings: each parameter bound to the corresponding argument.
	// Mark Escapes=true to prevent registerInlineCandidates from treating these
	// synthetic bindings as inline candidates — their Mutable/Escapes flags have
	// not been computed by the validator.
	syntheticBindings := make([]validate.ValidatedLetBinding, len(params.Required))
	for i, param := range params.Required {
		syntheticBindings[i] = validate.ValidatedLetBinding{
			Name:    param,
			Init:    args[i],
			Escapes: true,
		}
	}

	syntheticLet := validate.NewValidatedLet(
		"let",
		v.Source(),
		validate.LetKindLet,
		syntheticBindings,
		candidate.lambda.Body(),
	)

	// Set recursion guard.
	if p.currentlyInlining == nil {
		p.currentlyInlining = values.NewMapSet[environment.BindingID](0)
	}
	p.currentlyInlining.Set(bid)
	defer func() {
		p.currentlyInlining.Unset(bid)
	}()

	return true, CompileValidatedLet(p, ctctx, syntheticLet)
}

// tryInlineHOFCall inlines a call to a curated tail higher-order procedure
// (for-each, …) by substituting its pre-validated single-list loop template at the
// call site, when the callback argument is provably capture-safe. The inlined loop
// then self-tail-reclaims its env frame instead of leaking ~1/element (callback
// specialization Strategy A). Returns (true, nil) if inlined, (false, nil) if it
// does not apply, (false, err) on compilation failure.
//
// Unify: the callback (symbol or lambda) is always synthetic-let-bound to the
// template's callback parameter, and that binding is stamped CaptureSafe+Stable
// (justified by CallbackIsCaptureSafe) so the inlined loop's call to it passes
// bodyCalleesAllCaptureSafe. There is no symbol-substitution path.
func (p *CompileTimeContinuation) tryInlineHOFCall(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedCall,
) (bool, error) {
	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}

	b := p.env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	if b == nil {
		return false, nil
	}
	cbIdx := b.InlineHOFParam()
	if cbIdx < 0 {
		// Only a curated HOF binding is stamped, and the two stamp seams stamp only
		// structurally non-rebindable bindings: StampInlineHOFs sweeps the immutable
		// sealed base, and stampImportedInlineHOF stamps imported bindings (Imported ⇒
		// IsStable ⇒ R7RS forbids set!). So a stamped binding reaching here is always
		// Stable — a user (define fold …) resolves to an unstamped runtime binding
		// (cbIdx -1, handled above) and (set! fold …) on an import is rejected. No
		// separate IsStable() gate is needed; the stamp already implies it.
		return false, nil
	}

	store := p.env.Namespace().InlineHOFTemplates()
	if store == nil {
		return false, nil
	}
	// Select the template by the binding's STAMPED identity, not the call-site
	// name: an import-renamed curated HOF (e.g. fold imported as fold-right) must
	// inline its OWN template, not the one its surface name happens to match.
	tmplAny, found := store.InlineHOFTemplate(b.InlineHOFName())
	if !found {
		return false, nil
	}
	template, ok := tmplAny.(*validate.ValidatedLambda)
	if !ok {
		return false, nil
	}

	// Arity must match the single-list clause exactly; a multi-list call (more
	// args) falls through to the real HOF, whose apply-based clause is not inlined.
	// The second clause constrains the TEMPLATE, not the call: cbIdx comes from the
	// binding's stamp, so it must index the template's own parameter list. The arity
	// equality is what carries that bound over to args below.
	params := template.Params()
	args := v.Body()
	l := len(params.Required)
	if len(args) != l || cbIdx >= l {
		return false, nil
	}

	// The callback must be provably capture-safe; otherwise the inlined reclaiming
	// loop could release a frame across a capturing call (use-after-release).
	if !validate.CallbackIsCaptureSafe(args[cbIdx], p.env) {
		return false, nil
	}

	// Build the synthetic let: each template parameter bound to the call's
	// corresponding argument. The callback binding is stamped CaptureSafe (unify),
	// justified by the proof above. Escapes prevents re-registration as an inline
	// candidate (the params' Mutable/Escapes are not validator-computed here).
	syntheticBindings := make([]validate.ValidatedLetBinding, len(params.Required))
	for i, param := range params.Required {
		syntheticBindings[i] = validate.ValidatedLetBinding{
			Name:        param,
			Init:        args[i],
			Escapes:     true,
			CaptureSafe: i == cbIdx,
		}
	}
	syntheticLet := validate.NewValidatedLet(
		"let", v.Source(), validate.LetKindLet,
		syntheticBindings, template.Body(),
	)
	return true, CompileValidatedLet(p, ctctx, syntheticLet)
}
