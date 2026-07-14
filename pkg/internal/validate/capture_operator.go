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
	"github.com/aalpar/wile/pkg/syntax"
)

// bodyReferencesCaptureOperator reports whether any call in body has an
// operator that isCaptureOp classifies as a continuation-capturing primitive
// (call/cc, call-with-composable-continuation, abort-current-continuation).
//
// A capture *anywhere* in the lexical body disqualifies the function: the
// captured continuation pins the function's frame (machine_context_continuation.go
// SaveContinuation → machine_continuation.go Copy) regardless of where in the
// body the capture textually appears. The walk therefore descends through
// every sub-expression — arguments, closure bodies, and immediately-applied
// lambdas alike.
//
// isCaptureOp must be fail-safe: it returns true for the capture primitives by
// resolved binding identity (see frame_reclaim_build.go). A symbol it cannot
// confirm as a capture primitive returns false — the safety net for an
// unconfirmed operator is the edge-resolution path in mayCapture (an
// unresolved callee ⇒ unsafe), not this predicate.
func bodyReferencesCaptureOperator(
	body []ValidatedExpr,
	isCaptureOp func(sym *syntax.SyntaxSymbol) bool,
) bool {
	for _, e := range body {
		if exprReferencesCaptureOperator(e, isCaptureOp) {
			return true
		}
	}
	return false
}

func exprReferencesCaptureOperator(
	expr ValidatedExpr,
	isCaptureOp func(sym *syntax.SyntaxSymbol) bool,
) bool {
	if expr == nil {
		return false
	}

	// An OPAQUE SUBTREE is raw, unvalidated syntax that WalkSubExprs does not
	// descend into: a quasiquote's Template, and a *ValidatedLiteral that wraps a
	// passthrough FORM (cond-expand, include, let-syntax, …) rather than
	// self-evaluating data. See opaque_subtree.go for what reaches the analysis
	// this way and why.
	//
	// Anything can hide in there — `(,(call/cc …)), (cond-expand (else (lambda ()
	// x))), (include "sets-it.scm") — and it can capture, escape or mutate at
	// runtime, invisibly to this scan and to walkCallSites. Because the subtree is
	// un-analysable here, conservatively treat EVERY one as a capture risk: the
	// sound-by-default stance that an un-analysed subtree counts as unsafe.
	//
	// Flagging it here suffices for BOTH frame-reuse arming paths (the self-tail
	// OpSelfTailCall and the general-tail OpReleaseEnvFrame): a referencesCapture
	// node is unsafe regardless of its other facts. Before the *ValidatedLiteral
	// arm existed, a closure captured inside a cond-expand did not disqualify
	// reuse, codegen rebound the parameter frame in place under a live closure,
	// and the closure then read a slot holding another iteration's value — or a
	// value of another type entirely.
	//
	// Refining to "only subtrees that actually contain an unquote / a capture"
	// needs a nesting-aware raw-syntax walk and is deferred (precision, not
	// soundness).
	_, isOpaque := opaqueRawSyntax(expr)
	if isOpaque {
		return true
	}

	call, ok := expr.(*ValidatedCall)
	if ok {
		sym, ok := call.Proc().(*ValidatedSymbol)
		if ok && isCaptureOp(sym.Symbol) {
			return true
		}
	}

	app, ok := expr.(*ValidatedApply)
	if ok {
		sym, ok := app.Proc.(*ValidatedSymbol)
		if ok && isCaptureOp(sym.Symbol) {
			return true
		}
	}

	found := false
	WalkSubExprs(expr, func(child ValidatedExpr, _ ChildRole) {
		if found {
			return
		}
		if exprReferencesCaptureOperator(child, isCaptureOp) {
			found = true
		}
	})
	return found
}

// bodyCreatesEscapingClosure reports whether body creates a lambda or
// case-lambda that is not in immediately-applied operator position.
//
// v1 (conservative): any closure that is not the operator of its enclosing
// call (RoleCallProc) is assumed to escape — it may be stored, returned, or
// passed onward, parenting the enclosing frame via OpMakeClosure
// (operations_closure.go sets envPooled=false). An immediately-applied lambda
// does not itself escape, but its own body is still walked (a closure created
// inside it can escape). Precision relaxation via depth tracking is deferred.
func bodyCreatesEscapingClosure(body []ValidatedExpr) bool {
	for _, e := range body {
		if exprCreatesEscapingClosure(e, RoleNormal) {
			return true
		}
	}
	return false
}

func exprCreatesEscapingClosure(expr ValidatedExpr, role ChildRole) bool {
	if expr == nil {
		return false
	}

	switch expr.(type) {
	case *ValidatedLambda, *ValidatedCaseLambda:
		if role != RoleCallProc {
			return true
		}
	}

	found := false
	WalkSubExprs(expr, func(child ValidatedExpr, childRole ChildRole) {
		if found {
			return
		}
		if exprCreatesEscapingClosure(child, childRole) {
			found = true
		}
	})
	return found
}
