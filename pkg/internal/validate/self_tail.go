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
	"slices"

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
		bodyCalleesAllCaptureSafe(proc, selfName, env)
}

// bodyCalleesAllCaptureSafe reports whether every call operator in proc's body is
// either the self name or a confirmed capture-safe, non-rebindable callee. This is the
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
// THE SHADOW SET IS SEEDED WITH proc'S OWN PARAMETERS, and that seed is load-bearing.
// The walk resolves each operator through env.GetBinding against a FLAT env with no
// local frames, so the nameSet is what stands in for local resolution. Unseeded, a
// parameter sharing a capture-safe global's name resolves to that global and the body
// is wrongly trusted: (define (f car x) (car x)) stamped CaptureSafe, though `car` is
// bound to whatever the caller passes — including call/cc. The sibling walkers already
// seed their params (bodyIsSelfTailReusable, resolveCallEdges); this one did not.
// Taking proc rather than its body is what keeps the seed from being forgotten again.
//
// A LEXICALLY BOUND operator is not refused outright. If the shadow set records
// what the name is bound to — a lambda init or a function-form internal define,
// never set! within its binding form — the call imposes no obligation this walk
// has not already discharged, because that initializer's body is walked in place
// (see localCaptureSafe for the premise). Every loop-shaped Scheme procedure
// binds its loop this way — a named let is a letrec whose single binding is the
// loop lambda — so the blanket refusal denied the stamp to most of the corpus:
// 19/68 top-level defines across the Gabriel corpus, against 42/68 with the
// local rule. A binder the walk cannot see through still refuses.
//
// This is conservative — it refuses a same-unit user define callee (mutual
// recursion), which the interprocedural classifier (ClassifyFrameReclaim) admits —
// but it is SOUND, which the kill criterion demands.
func bodyCalleesAllCaptureSafe(proc ValidatedBodyAndParams, selfName string, env *environment.EnvironmentFrame) bool {
	return bodyCalleesAllCaptureSafeUnder(proc, selfName, env, nil)
}

// bodyCalleesAllCaptureSafeUnder is bodyCalleesAllCaptureSafe with an explicit
// starting shadow set, on top of which proc's parameters are seeded as always.
// The base carries an ASSUMPTION the caller is responsible for discharging. The
// two callers that pass one are LetBindingFrameReleasable and
// InternalDefineFrameReleasable — the two spellings of a local recursive binding
// group. Each seeds its group's siblings so a call to a sibling can be cleared,
// then verifies every member of that group under the same seed.
//
// Passing a base is therefore not a way to widen the walk locally. A base whose
// entries are not independently proven turns localCaptureSafe's premise — the
// walk has already reached this initializer — into an unchecked claim.
func bodyCalleesAllCaptureSafeUnder(
	proc ValidatedBodyAndParams,
	selfName string,
	env *environment.EnvironmentFrame,
	base nameSet,
) bool {
	if env == nil {
		return false
	}
	safe := true
	seed := base.withParams(proc.Params())
	walkBodySeq(proc.Body(), seed, func(op ValidatedExpr, bound nameSet) {
		if !safe {
			return
		}
		safe = calleeCaptureSafe(op, bound, selfName, env)
	})
	return safe
}

// calleeCaptureSafe decides one call operator under the shadow set in force at
// its call site.
func calleeCaptureSafe(
	op ValidatedExpr,
	bound nameSet,
	selfName string,
	env *environment.EnvironmentFrame,
) bool {
	sym, ok := op.(*ValidatedSymbol)
	if !ok {
		return false // computed operator ⇒ unknown ⇒ could capture
	}
	name := sym.Symbol.Sym.Key
	local, shadowed := bound[name]
	if shadowed {
		return localCaptureSafe(local)
	}
	if name == selfName {
		return true // the genuine self call
	}
	// Not a capture-safe, non-rebindable primitive: a user-defined callee (no
	// CaptureSafe), an unresolved name, or a set!-able primitive. A same-unit
	// define callee is rejected here (interprocedural mutual recursion is the
	// classifier's job, not the per-body predicate's).
	b := env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	return b != nil && b.IsCaptureSafe() && b.IsStable()
}

// localCaptureSafe answers for an operator bound by an enclosing local scope.
// A binder the walk cannot see through, or a name set! within its own binding
// form, refuses — which was every local's answer before A-local.
//
// A VISIBLE, UNMUTATED LOCAL IMPOSES NO FURTHER OBLIGATION, and that is the
// whole of A-local. It needs no recursive proof of the initializer, because the
// enclosing walk has already proven it in place: withLet and withInternalDefines
// record init only for binders whose initializer walkCallSites descends into on
// the very next statement (frame_reclaim_build.go — the *ValidatedLet arm walks
// every binding's Init, walkBodySeq walks every body expression). So every call
// operator inside that initializer has already been decided by this same pass,
// under its own lexical scope, into this same accumulator. Calling the local
// reaches nothing the walk has not already reached.
//
// That coupling — record evidence only where the body is walked — is the
// predicate's soundness premise, and it is what the shape tests pin: a local
// whose body calls a procedure-invoking callee must refuse, for each binder
// kind (alocal_test.go). A binder form added to the shadow set without a
// matching descent would break the premise silently.
func localCaptureSafe(local localBinding) bool {
	return local.init != nil && !local.mutated
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
	// Frame-releasable = capture-safe-as-callee AND no escaping closure: releasing
	// proc's OWN frame additionally requires that no closure parents it (escape),
	// which calling proc does not.
	return bodyCannotCaptureCaller(proc, selfName, env) &&
		!bodyCreatesEscapingClosure(proc.Body())
}

// bodyCannotCaptureCaller is the shared core of BodyIsFrameReleasable and
// ProcedureBodyIsCaptureSafe: the body runs no capture operator (call/cc &c.) and
// every callee is itself capture-safe (bodyCalleesAllCaptureSafe resolves each
// callee's IsCaptureSafe()/IsStable() under a shadow set seeded with proc's
// parameters). It deliberately omits the escaping-closure check — escape governs
// whether the body's OWN frame may be released, not whether CALLING the body
// captures the caller's continuation. env must be non-nil.
func bodyCannotCaptureCaller(proc ValidatedBodyAndParams, selfName string, env *environment.EnvironmentFrame) bool {
	return !bodyReferencesCaptureOperator(proc.Body(), makeIsCaptureOp(env)) &&
		bodyCalleesAllCaptureSafe(proc, selfName, env)
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
	return bodyCannotCaptureCaller(proc, selfName, env)
}

// LetBindingSelfTailReusable reports the arity and eligibility of the i-th
// binding of a recursively-scoped let (letrec / letrec* / named-let) for in-place
// self-tail reuse — the local-binding analogue of selfTailForDefine. ok is true iff
//
//   - the binding's Init is a lambda whose body is self-tail-reusable on the
//     binding name (BodyIsSelfTailReusable, i.e. the in-body facts AND
//     bodyCalleesAllCaptureSafe), AND
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

// LetBindingFrameReleasable reports whether the i-th binding's lambda may have
// its OWN activation frame released at its tail calls (OpReleaseEnvFrame) — the
// local analogue of BodyIsFrameReleasable, and the release-path sibling of
// LetBindingSelfTailReusable.
//
// WHAT THIS COVERS THAT SELF-TAIL DOES NOT. A loop whose tail call is to a
// SIBLING binding rather than to itself: mutual local recursion, which has no
// depth-0 self call and so fails bodyIsSelfTailReusable's clause (4) outright.
// Measured at 2 env-frame allocations per iteration before this predicate
// existed. A loop whose self call sits inside a further let is NOT covered —
// that call is at depth > 0, where the reuse disposition is cleared on the let
// descent (compile_let.go), so no arming here can reach it.
//
// THE CO-INDUCTION, AND WHY IT IS A GROUP PREDICATE. Clearing a call to sibling
// `o` rests on `o` not capturing, which is exactly the property being proven for
// `o`. The shadow set therefore seeds the WHOLE letrec group (withLet), and this
// function discharges the resulting assumption by verifying every lambda-bound
// member under that same seed. Verifying only the binding asked about would
// leave the assumption standing on itself. The group answer is uniform — one
// unsafe member refuses the whole group — which loses precision when an unrelated
// sibling is unsafe, and is the conservative direction.
//
// Only a lambda init is ever assumed anything: withLet records evidence solely
// for lambda inits (letBindingLocal), so a sibling bound to a parameter or a
// computed value stays opaque and refuses at its call site. A sibling set! within
// the let is likewise already folded in as localBinding.mutated, so a name that
// can be rebound to a capturing procedure is never cleared.
//
// TWO CHECKS BELOW ARE REDUNDANT TODAY, and are kept as explicit tightenings
// (they can withhold a verdict, never grant one — the subsystem's kill-don't-guard
// rule). Both survive mutation testing, so neither is load-bearing at present and
// a reader must not infer that it is:
//
//   - The InitsInScope precondition. The group seed is withLet's INIT scope, which
//     for a plain let binds every name opaquely, so a sibling call already refuses
//     there. The guard becomes load-bearing the moment that seed changes to the
//     body scope.
//   - The group capture-operator scan. An INVOKED capture operator is also a call
//     operator, and call/cc carries no CaptureSafe stamp, so the callee walk
//     refuses it first. The scan is what would catch a capture reached other than
//     by direct invocation, and it keeps this predicate's shape identical to
//     bodyCannotCaptureCaller's.
//
// PRECONDITION: v.Kind must be recursive (letrec family), or the bindings are not
// in scope in each other's inits and the group seed would describe outer bindings.
func LetBindingFrameReleasable(v *ValidatedLet, i int, env *environment.EnvironmentFrame) bool {
	if env == nil {
		return false
	}
	if !v.Kind.InitsInScope() {
		return false
	}
	lam, ok := v.Bindings[i].Init.(*ValidatedLambda)
	if !ok {
		return false
	}
	// Releasing THIS binding's frame additionally requires that no closure it
	// creates parents that frame — the same extra clause BodyIsFrameReleasable
	// carries over ProcedureBodyIsCaptureSafe, and it applies to i alone.
	if bodyCreatesEscapingClosure(lam.Body()) {
		return false
	}
	group, _ := nameSet(nil).withLet(v)
	for j := range v.Bindings {
		sib, isLambda := v.Bindings[j].Init.(*ValidatedLambda)
		if !isLambda {
			// Not assumed safe by the seed, so nothing to discharge for it.
			continue
		}
		name := v.Bindings[j].Name.Sym.Key
		if bodyReferencesCaptureOperator(sib.Body(), makeIsCaptureOp(env)) {
			return false
		}
		if !bodyCalleesAllCaptureSafeUnder(sib, name, env, group) {
			return false
		}
	}
	return true
}

// InternalDefineFrameReleasable is LetBindingFrameReleasable's twin for the OTHER
// spelling of a local recursive binding group: internal defines. R7RS §5.3.2 gives
// them letrec* semantics, but they are never rewritten into a ValidatedLet — they
// stay ValidatedDefine nodes compiled in the enclosing lambda's own frame — so the
// let-binding predicate never sees them, while the shape and the hazard are
// identical. body is the enclosing body sequence whose internal defines form the
// group; d must be one of them.
//
// This is the spelling that occurs in practice: benchmarks/larceny/src alone
// carries 255 internal defines in compiler.scm and 32 in earley.scm, against zero
// explicit letrec forms in either corpus. Mutual recursion between two internal
// defines allocated a frame per iteration (2.0 measured) for exactly the reason
// the letrec form did — a call to a sibling resolves through env.GetBinding, finds
// nothing because the sibling is local, and refuses.
//
// The co-induction, the uniform group answer, and the per-binding escape clause
// are LetBindingFrameReleasable's, unchanged; see there for why each is shaped
// that way. Only the seed differs (withInternalDefines rather than withLet).
//
// MEMBERSHIP IS CHECKED, and that check is load-bearing rather than defensive
// hygiene. The group arrives from the compiler, which must supply the body that
// predeclared d — there are three such sites. If a caller ever passes a STALE
// enclosing body, an unrelated define sharing a sibling's name would be recorded
// as evidence for a call to a different procedure entirely, which is unsound. A
// stale body does not contain d, so refusing when d is absent converts that whole
// class of plumbing error into a forgone optimization.
func InternalDefineFrameReleasable(
	d *ValidatedDefine,
	body []ValidatedExpr,
	env *environment.EnvironmentFrame,
) bool {
	if env == nil || !d.IsFunction {
		return false
	}
	if !slices.Contains(body, ValidatedExpr(d)) {
		return false
	}
	if bodyCreatesEscapingClosure(d.Body()) {
		return false
	}
	group := nameSet(nil).withInternalDefines(body)
	for _, e := range body {
		sib, isDefine := e.(*ValidatedDefine)
		if !isDefine || !sib.IsFunction {
			// A non-function define binds a value, for which withInternalDefines
			// records no evidence, so nothing is assumed and nothing to discharge.
			continue
		}
		if bodyReferencesCaptureOperator(sib.Body(), makeIsCaptureOp(env)) {
			return false
		}
		if !bodyCalleesAllCaptureSafeUnder(sib, sib.Name().Sym.Key, env, group) {
			return false
		}
	}
	return true
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
		// The (let ((t E)) (if t t B)) shape `or` expands to compiles WITHOUT a
		// frame (compileOrShapedLet), so its alternative's tail self calls stay at
		// this depth. Keeping the two readings in step is why LetIsOrShaped is
		// exported instead of being a shape match local to codegen: if only the
		// compiler dropped the frame, this walk would still count the call at
		// depth+1 and refuse to rewrite the very site the lowering just freed.
		//
		// The bound set is deliberately NOT extended. LetIsOrShaped has already
		// established that the binder does not occur in B at a compatible scope, so
		// there is nothing there for it to shadow — including the case where the
		// binder happens to spell selfName, which that same scan refuses outright.
		_, orAlt, isOrShaped := LetIsOrShaped(v)
		if isOrShaped {
			return tailExprHasSelfCall(orAlt, depth, bound, selfName, arity)
		}
		// A let runs its body in a frame pushed above the parameter frame, so its
		// tail self calls are at depth+1 (v1 leaves them un-rewritten). Its bound
		// names shadow.
		return tailSeqHasSelfCall(v.Body(), depth+1, bound.withLetBindings(v.Bindings), selfName, arity)
	default:
		return false
	}
}
