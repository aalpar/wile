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

// LetIsOrShaped recognizes
//
//	(let ((t E)) (if t t B))
//
// and returns E and B. This is what `or` expands to (bootstrap_macros.scm), one
// let per operand beyond the first, and it is the single most common
// macro-introduced environment frame in the tree: 340 occurrences across the
// stdlib and the larceny corpus, none of them written in any .scm file.
//
// THE FRAME IS UNOBSERVABLE, which is what makes removing it a rewrite rather
// than an optimization with a side condition. `t` is bound only to be tested and
// returned, and OpBranchOnFalseValue reads the value register without clobbering
// it, so the value E already left there IS the consequent's value. The form
// compiles to E, a branch, and B — no slot, no OpPushEnv, and no reload.
//
// Note what this does NOT require: nothing about E's type. An earlier design
// lowered `or` to (if E #t B), which is only valid when E yields a boolean and
// would have needed the primitive return-type annotation carried across the
// registry/validate layering. The register passthrough returns E's own value, so
// (or 5 1) is still 5 and the type question does not arise.
//
// WHY THE ALTERNATIVE IS SCANNED. Lowering evaluates B in the enclosing frame
// instead of the let's, so a reference to `t` there would resolve to a different
// binding or to none. `or`'s temp is hygienic and cannot appear in B, but the
// same shape written by hand can, and (cond (test => f)) produces a sibling
// shape whose consequent consumes the value. Both are refused.
//
// The scan matches by name AND binderScopes ⊆ refScopes — Flatt's rule, through
// the same syntax.ScopesCompatible the environment's resolveLocal uses, so it
// cannot drift from real resolution. Over-approximating in that direction is
// safe: a genuine reference necessarily carries the binder's name and satisfies
// the subset relation, so none is missed, while an unrelated same-name
// occurrence at a compatible scope merely costs the lowering. Refusing is always
// the free direction here — the frame stays, exactly as it does today.
//
// The head positions need no such scan. Test and Conseq must BE this binding's
// symbol, and no form intervenes between the let and them that could rebind the
// name, so a match there is exact rather than approximate.
func LetIsOrShaped(v *ValidatedLet) (init ValidatedExpr, alt ValidatedExpr, ok bool) {
	// A letrec-family init is evaluated with the binding already in scope and so
	// cannot be hoisted out of the frame; a tag binds a loop procedure over it.
	if v.Kind != LetKindLet || v.Tag != nil || len(v.Bindings) != 1 {
		return nil, nil, false
	}
	body := v.Body()
	if len(body) != 1 {
		return nil, nil, false
	}
	ifExpr, isIf := body[0].(*ValidatedIf)
	if !isIf {
		return nil, nil, false
	}
	// A one-armed if yields void on the false branch; the lowering emits no code
	// for the consequent and would have nowhere to put that void.
	if ifExpr.Alt == nil {
		return nil, nil, false
	}
	binder := v.Bindings[0].Name
	if !isRefTo(ifExpr.Test, binder) || !isRefTo(ifExpr.Conseq, binder) {
		return nil, nil, false
	}
	if bindingOccursIn(ifExpr.Alt, binder) {
		return nil, nil, false
	}
	return v.Bindings[0].Init, ifExpr.Alt, true
}

// isRefTo reports whether expr is exactly a reference to binder.
func isRefTo(expr ValidatedExpr, binder *syntax.SyntaxSymbol) bool {
	sym, isSym := expr.(*ValidatedSymbol)
	if !isSym {
		return false
	}
	return refMatchesBinder(sym.Symbol, binder)
}

// bindingOccursIn reports whether binder's name occurs anywhere in expr at a
// compatible scope, counting set! targets. See LetIsOrShaped for why an
// over-count is the safe direction.
func bindingOccursIn(expr ValidatedExpr, binder *syntax.SyntaxSymbol) bool {
	q := false
	WalkBindingRefs(expr, func(sym *syntax.SyntaxSymbol, _ RefRole, _ int) {
		if refMatchesBinder(sym, binder) {
			q = true
		}
	})
	if q {
		return true
	}
	return opaqueSubtreeMentions(expr, binder)
}

// opaqueSubtreeMentions is bindingOccursIn's second half, covering what
// WalkBindingRefs structurally cannot reach: the subtrees WalkSubExprs reports
// CHILDLESS. A quasiquote template and a passthrough form (cond-expand,
// include, let-syntax) hold raw syntax this package never validated, so the
// reference walk concludes "nothing in there" and the lowering drops a frame
// whose binder IS referenced. Measured:
// (let ((c (assq 'z '((a 1))))) (if c c `(missing ,c))) evaluated to
// (missing 42), reading an OUTER c, where (missing #f) is the meaning.
//
// It is not a blanket refusal. The depth walk is forEachRawSymbol's, reused
// rather than re-derived, so a name that appears only as template DATA costs
// nothing — refusing every opaque alternative would retire the lowering on
// exactly the quasiquote-heavy code it was written for. What IS blanket is the
// nil payload: opaqueRawSyntax's contract makes the boolean the opacity answer
// and the payload merely what to scan, and a nil one is when we know least.
func opaqueSubtreeMentions(expr ValidatedExpr, binder *syntax.SyntaxSymbol) bool {
	if expr == nil {
		return false
	}
	raw, opaque := opaqueRawSyntax(expr)
	if !opaque {
		q := false
		WalkSubExprs(expr, func(child ValidatedExpr, _ ChildRole) {
			if opaqueSubtreeMentions(child, binder) {
				q = true
			}
		})
		return q
	}
	if raw == nil {
		return true
	}
	// A *ValidatedQuasiquote's Template is the form's argument, so the caller
	// has already stepped one level in; every other opaque node is ordinary
	// code. Same split markOpaqueTemplate / markOpaqueCode make.
	depth := quasiDepthCode
	_, isQuasi := expr.(*ValidatedQuasiquote)
	if isQuasi {
		depth = quasiDepthTemplate
	}
	q := false
	forEachRawSymbol(raw, depth, func(sym *syntax.SyntaxSymbol) {
		if refMatchesBinder(sym, binder) {
			q = true
		}
	})
	return q
}

// refMatchesBinder is the name-plus-scope-subset test both halves share.
func refMatchesBinder(ref, binder *syntax.SyntaxSymbol) bool {
	if ref.Sym.Key != binder.Sym.Key {
		return false
	}
	return syntax.ScopesCompatible(binder.Scopes(), ref.Scopes())
}
