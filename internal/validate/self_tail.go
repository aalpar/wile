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
	"github.com/aalpar/wile/internal/syntax"
)

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
	// The self BINDING must be immutable in the body: a set! on selfName means a
	// later self-call must dispatch to the new value, which OpSelfTailCall's
	// hardcoded jump-to-0 cannot do. (For a top-level self binding the producer
	// must ALSO be Stable against cross-unit redefinition — checked at the emit
	// site, where the resolved binding is available; this clause covers the
	// in-body half, which is the whole story for a lexical named-let loop.)
	if bodyMutatesName(body, selfName) {
		return false
	}
	return tailSeqHasSelfCall(body, 0, nameSet(nil), selfName, len(p.Required))
}

// bodyMutatesName reports whether any set! reachable from body assigns to name,
// accounting for lexical shadowing: a set! to a lambda/let/internal-define
// binding of the same name targets that inner binding, not the enclosing one.
func bodyMutatesName(body []ValidatedExpr, name string) bool {
	return seqMutatesName(body, name, nameSet(nil))
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
