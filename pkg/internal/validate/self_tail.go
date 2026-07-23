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

package validate

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// BodyIsSelfTailReusable is the compiler entry point: it builds the production
// capture-operator identity test from env (hygiene-correct: a locally-shadowed
// call/cc is not the primitive) and runs the self-tail-reuse safety predicate.
// selfName is the closure's own bound name as a symbol Key.
//
// NOTE: this is the IN-BODY safety half. The caller must additionally ensure the
// self BINDING is immutable against cross-unit redefinition — for a top-level
// define that means IsStable() at the emit site; for a lexical named-let loop the
// in-body ¬set! this predicate checks is the whole story.
func BodyIsSelfTailReusable(
	proc ValidatedBodyAndParams,
	selfName string,
	env *environment.EnvironmentFrame,
) bool {
	return bodyIsSelfTailReusable(proc, selfName, makeIsCaptureOp(env)) &&
		bodyCalleesAllCaptureSafe(proc.Body(), selfName, env)
}

// bodyCalleesAllCaptureSafe reports whether every call operator in body is either
// the self name or a confirmed capture-safe, non-rebindable callee. This is the
// interprocedural half of capture-safety that the textual bodyReferencesCaptureOperator
// cannot see: a loop that calls an unknown function — a user callback (map/for-each's
// `proc`), a computed operator, a procedure-invoking or set!-able binding — could
// have that callee capture the continuation that pins the frame, so in-place reuse
// would corrupt a re-entered continuation. A callee that is both CaptureSafe
// (Binding.IsCaptureSafe()) and Stable (Imported ∨ frozen, so not rebindable to a
// capturing procedure) cannot. CaptureSafe covers both a primitive stamped from
// !PrimitiveSpec.InvokesProcedure AND a Scheme procedure proven capture-safe at
// compile time (ProcedureBodyIsCaptureSafe, e.g. zero?/not/a user helper). Self
// calls are safe by the co-inductive assumption that the function under analysis
// does not itself capture.
//
// This is conservative — it refuses a same-unit user define callee (mutual
// recursion), which the interprocedural classifier (ClassifyFrameReclaim) admits —
// but it is SOUND, which the kill criterion demands.
func bodyCalleesAllCaptureSafe(body []ValidatedExpr, selfName string, env *environment.EnvironmentFrame) bool {
	if env == nil {
		return false
	}
	safe := true
	walkBodySeq(body, nameSet(nil), func(proc ValidatedExpr, bound nameSet) {
		if !safe {
			return
		}
		sym, ok := proc.(*ValidatedSymbol)
		if !ok {
			safe = false // computed operator ⇒ unknown ⇒ could capture
			return
		}
		name := sym.Symbol.Sym.Key
		if name == selfName && !bound.has(name) {
			return // the genuine self call
		}
		if bound.has(name) {
			safe = false // shadowed local
			return
		}
		b := env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
		if b == nil || !b.IsCaptureSafe() || !b.IsStable() {
			// Not a capture-safe, non-rebindable primitive: a user-defined callee
			// (no CaptureSafe), an unresolved name, or a set!-able primitive. A
			// same-unit define callee is rejected here (interprocedural mutual
			// recursion is the classifier's job, not the per-body predicate's).
			safe = false
		}
	})
	return safe
}

// BodyIsFrameReleasable reports whether proc's activation frame may be released to
// the pool at its tail calls (the OpReleaseEnvFrame optimization — fib-shaped
// general tail recursion). Unlike OpSelfTailCall it does NOT rebind in place or
// hardcode a jump: it releases the dead frame and performs a normal apply (which
// re-resolves the callee). So it needs neither the define's stability nor a
// self-tail call — only that the body can never expose its frame to a continuation:
//
//   - no capture operator in the body (no call/cc &c.),
//   - no escaping closure, and
//   - every callee is the self name or a capture-safe, non-rebindable primitive
//     (bodyCalleesAllCaptureSafe) — a callee that could capture would pin the frame.
//
// This is a conservative, per-body approximation of the interprocedural classifier
// (ClassifyFrameReclaim): it covers self-recursion over capture-safe primitives
// (fib, tak) but refuses a tail call to another user-defined function (mutual
// recursion), which the full call graph would admit. Sound either way; the
// call-graph precision is a later refinement.
func BodyIsFrameReleasable(proc ValidatedBodyAndParams, selfName string, env *environment.EnvironmentFrame) bool {
	if env == nil {
		return false
	}
	body := proc.Body()
	// Frame-releasable = capture-safe-as-callee AND no escaping closure: releasing
	// proc's OWN frame additionally requires that no closure parents it (escape),
	// which calling proc does not.
	return bodyCannotCaptureCaller(body, selfName, env) &&
		!bodyCreatesEscapingClosure(body)
}

// bodyCannotCaptureCaller is the shared core of BodyIsFrameReleasable and
// ProcedureBodyIsCaptureSafe: the body runs no capture operator (call/cc &c.) and
// every callee is itself capture-safe (bodyCalleesAllCaptureSafe resolves each
// callee's IsCaptureSafe()/IsStable()). It deliberately omits the escaping-closure
// check — escape governs whether the body's OWN frame may be released, not whether
// CALLING the body captures the caller's continuation. env must be non-nil.
func bodyCannotCaptureCaller(body []ValidatedExpr, selfName string, env *environment.EnvironmentFrame) bool {
	return !bodyReferencesCaptureOperator(body, makeIsCaptureOp(env)) &&
		bodyCalleesAllCaptureSafe(body, selfName, env)
}

// ProcedureBodyIsCaptureSafe reports whether CALLING proc can never capture the
// caller's continuation — the property that lets the frame-reclaim classifier trust
// proc as a callee. It is exactly bodyCannotCaptureCaller (BodyIsFrameReleasable
// MINUS the escaping-closure check): a procedure that merely builds and returns a
// closure does not capture the caller's continuation when called, so it is safe as a
// callee even though its own frame is not releasable.
//
// The compiler stamps a define's binding CaptureSafe from this verdict
// (compile_define.go), so a proven-safe Scheme procedure — stdlib (zero?, not) or a
// user helper — is trusted exactly like a capture-safe primitive, without a
// hand-maintained whitelist. Conservative on forward references: a callee not yet
// stamped reads IsCaptureSafe()==false, so proc is left unstamped (sound — a missed
// stamp only forgoes the optimization).
func ProcedureBodyIsCaptureSafe(proc ValidatedBodyAndParams, selfName string, env *environment.EnvironmentFrame) bool {
	if env == nil {
		return false
	}
	return bodyCannotCaptureCaller(proc.Body(), selfName, env)
}

// LetBindingSelfTailReusable reports the arity and eligibility of the i-th
// binding of a recursively-scoped let (letrec / letrec* / named-let) for in-place
// self-tail reuse — the local-binding analogue of selfTailForDefine. ok is true iff
//
//   - the binding's Init is a lambda whose body is self-tail-reusable on the
//     binding name (bodyIsSelfTailReusable), AND
//   - the binding name is immutable across the WHOLE let: never set! in any
//     binding init or in the let body.
//
// The second clause is the local immutability story. bodyIsSelfTailReusable only
// inspects the lambda's own body, but a sibling init or the let body could set!
// the name, which would make a hardcoded jump-to-pc=0 unsound. A lexical letrec
// binding cannot be mutated from outside the let, so set!-freedom within the let
// is sufficient (no IsStable() is needed, unlike a top-level define).
//
// PRECONDITION: the caller must ensure v.Kind is recursive (letrec-family) so the
// lambda can see itself; in a plain let/let* the name is not in scope in the init.
func LetBindingSelfTailReusable(v *ValidatedLet, i int, env *environment.EnvironmentFrame) (int, bool) {
	lam, ok := v.Bindings[i].Init.(*ValidatedLambda)
	if !ok {
		return 0, false
	}
	name := v.Bindings[i].Name.Sym.Key
	if !BodyIsSelfTailReusable(lam, name, env) {
		return 0, false
	}
	if letMutatesName(v, name) {
		return 0, false
	}
	return len(lam.Params().Required), true
}

// letMutatesName reports whether name is set! anywhere in the let's binding inits
// or body (shadow-aware). Covers the sibling-init and let-body mutation paths that
// the per-lambda predicate does not see.
func letMutatesName(v *ValidatedLet, name string) bool {
	for _, b := range v.Bindings {
		if exprMutatesName(b.Init, name, nameSet(nil)) {
			return true
		}
	}
	return seqMutatesName(v.Body(), name, nameSet(nil))
}

// bodyIsSelfTailReusable reports whether a closure named selfName may have its
// activation (parameter) frame reused in place at its self-recursive tail calls
// — the safety gate for the OpSelfTailCall codegen (escape-gated plan Phase 4,
// sibling compile-time-self-tail design). It holds iff the body is safe for reuse
// AND has at least one rewritable self-tail call:
//
//  1. Non-variadic params — a rest slot cannot be re-bound by a flat parallel
//     store of the evaluated arguments.
//  2. The body references no capture operator (bodyReferencesCaptureOperator): a
//     captured continuation pins the frame, so reusing it would corrupt the
//     continuation (the failure mode that sank the runtime recycler).
//  3. The body creates no escaping closure (bodyCreatesEscapingClosure): an
//     escaping closure parents the frame and may outlive the call.
//  4. There is at least one call to selfName in TAIL position at depth 0 — mc.env
//     at that call IS the parameter frame, not nested inside a let/lambda that
//     pushed an intermediate frame (v1 restriction) — with arity == len(Required).
//
// SOUNDNESS of ignoring the rest. Reuse fires ONLY at the depth-0 tail self calls
// (per-site in codegen). With (2) and (3) excluded, every SaveContinuation in the
// body is balanced (LIFO, restored on return), so at a depth-0 tail self call the
// frame is reachable only through mc.env — its args are already evaluated onto the
// stack and nothing executes after. Therefore non-tail self calls, self used as a
// value, and depth>0 tail self calls are neither required nor disqualifying: they
// just don't satisfy (4), so a body containing only such forms returns false.
//
// selfName is the closure's own bound name (a define name, or a named-let loop
// variable) as a symbol Key. isCaptureOp is the resolved capture-operator identity
// test (see frame_reclaim_build.go); in isolation tests a name-only stub is used.
//
// THIS UNEXPORTED FORM IS FAIL-OPEN IN ISOLATION and is TEST-ONLY. It checks only
// the in-body capture/escape facts; it does NOT run bodyCalleesAllCaptureSafe, so
// a body that calls a capturing helper (the map/for-each hazard) would pass here.
// Soundness requires the interprocedural callee check too, which is why every
// PRODUCTION caller goes through the exported BodyIsSelfTailReusable (which ANDs
// bodyCalleesAllCaptureSafe). Do not call this directly from compiler code.
func bodyIsSelfTailReusable(
	proc ValidatedBodyAndParams,
	selfName string,
	isCaptureOp func(*syntax.SyntaxSymbol) bool,
) bool {
	p := proc.Params()
	if p == nil || p.Rest != nil {
		return false
	}
	body := proc.Body()
	if bodyReferencesCaptureOperator(body, isCaptureOp) {
		return false
	}
	if bodyCreatesEscapingClosure(body) {
		return false
	}
	// Seed the shadow set with the parameter names: a parameter named the same as
	// the function shadows the self reference, so a call to it is not a self-call
	// (e.g. (define (loop loop) (loop x)) invokes the parameter).
	seed := nameSet(nil).withParams(p)

	// The self BINDING must be immutable in the body: a set! on selfName means a
	// later self-call must dispatch to the new value, which OpSelfTailCall's
	// hardcoded jump-to-0 cannot do. (For a top-level self binding the producer
	// must ALSO be Stable against cross-unit redefinition — checked at the emit
	// site, where the resolved binding is available; this clause covers the
	// in-body half, which is the whole story for a lexical named-let loop.)
	if seqMutatesName(body, selfName, seed) {
		return false
	}
	return tailSeqHasSelfCall(body, 0, seed, selfName, len(p.Required))
}

// seqMutatesName walks a body sequence for a set! of name. Internal-define names
// are hoisted into the shadow set first (letrec* — a same-name internal define
// shadows the enclosing name across the whole sequence).
func seqMutatesName(body []ValidatedExpr, name string, bound nameSet) bool {
	inner := bound
	var defined []string
	for _, e := range body {
		d, ok := e.(*ValidatedDefine)
		if ok {
			defined = append(defined, d.Name().Sym.Key)
		}
	}
	if len(defined) > 0 {
		inner = bound.with(defined...)
	}
	for _, e := range body {
		if exprMutatesName(e, name, inner) {
			return true
		}
	}
	return false
}

// exprMutatesName reports whether expr contains a set! of name that is not
// lexically shadowed. Binding forms (lambda, case-lambda, let, internal define)
// extend the shadow set for their sub-scopes; every other form descends with the
// inherited set (none of them introduces a variable binding).
func exprMutatesName(expr ValidatedExpr, name string, bound nameSet) bool {
	if expr == nil {
		return false
	}
	switch v := expr.(type) {
	case *ValidatedSetBang:
		if v.Name.Sym.Key == name && !bound.has(name) {
			return true
		}
		return exprMutatesName(v.SubExp(), name, bound)
	case *ValidatedLambda:
		return seqMutatesName(v.Body(), name, bound.withParams(v.Params()))
	case *ValidatedCaseLambda:
		for _, clause := range v.Clauses() {
			if seqMutatesName(clause.Body(), name, bound.withParams(clause.Params())) {
				return true
			}
		}
		return false
	case *ValidatedLet:
		// Inits run in the outer scope; the body in the let-extended scope.
		for _, b := range v.Bindings {
			if exprMutatesName(b.Init, name, bound) {
				return true
			}
		}
		return seqMutatesName(v.Body(), name, bound.withLetBindings(v.Bindings))
	case *ValidatedBegin:
		return seqMutatesName(v.Body(), name, bound)
	case *ValidatedDefine:
		if v.IsFunction {
			return seqMutatesName(v.Body(), name, bound.withParams(v.Params()))
		}
		return exprMutatesName(v.SubExp(), name, bound)
	default:
		found := false
		WalkSubExprs(expr, func(child ValidatedExpr, _ ChildRole) {
			if found {
				return
			}
			if exprMutatesName(child, name, bound) {
				found = true
			}
		})
		return found
	}
}

// tailSeqHasSelfCall reports whether the tail position of a body sequence
// contains a depth-0 self call to selfName with the given arity. Only the last
// expression of the sequence is in tail position; internal-define names are
// hoisted into the shadow set (letrec* — a same-name internal define means the
// operator no longer resolves to the enclosing self).
func tailSeqHasSelfCall(body []ValidatedExpr, depth int, bound nameSet, selfName string, arity int) bool {
	if len(body) == 0 {
		return false
	}
	inner := bound
	var defined []string
	for _, e := range body {
		d, ok := e.(*ValidatedDefine)
		if ok {
			defined = append(defined, d.Name().Sym.Key)
		}
	}
	if len(defined) > 0 {
		inner = bound.with(defined...)
	}
	return tailExprHasSelfCall(body[len(body)-1], depth, inner, selfName, arity)
}

// tailExprHasSelfCall reports whether expr, evaluated in tail position at the
// given frame depth, is (or contains in a tail sub-position) a depth-0 self call
// to selfName with the given arity. It descends ONLY through tail-transparent
// forms (if branches, begin/let tails); a let body increments depth because it
// runs in a pushed frame. Non-tail sub-expressions (call arguments, if tests,
// let inits) are deliberately not walked — a self call there cannot reuse the
// frame, so it is irrelevant to this query.
func tailExprHasSelfCall(expr ValidatedExpr, depth int, bound nameSet, selfName string, arity int) bool {
	if expr == nil {
		return false
	}
	switch v := expr.(type) {
	case *ValidatedCall:
		sym, ok := v.Proc().(*ValidatedSymbol)
		if !ok {
			return false
		}
		if sym.Symbol.Sym.Key != selfName || bound.has(selfName) {
			return false
		}
		return depth == 0 && len(v.Body()) == arity
	case *ValidatedIf:
		// Both branches inherit the tail position and frame depth.
		return tailExprHasSelfCall(v.Conseq, depth, bound, selfName, arity) ||
			tailExprHasSelfCall(v.Alt, depth, bound, selfName, arity)
	case *ValidatedBegin:
		return tailSeqHasSelfCall(v.Body(), depth, bound, selfName, arity)
	case *ValidatedLet:
		// A let runs its body in a frame pushed above the parameter frame, so its
		// tail self calls are at depth+1 (v1 leaves them un-rewritten). Its bound
		// names shadow.
		return tailSeqHasSelfCall(v.Body(), depth+1, bound.withLetBindings(v.Bindings), selfName, arity)
	default:
		return false
	}
}
