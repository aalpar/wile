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
	"github.com/aalpar/wile/pkg/values"
)

// Opaque subtrees: the forms whose bodies this package validates as raw syntax
// it never looks inside.
//
// Two shapes reach the analysis as un-analysed code:
//
//   - *ValidatedQuasiquote, whose Template is a raw syntax tree. An unquote can
//     hold any expression: `(,(set! g 1)), `(,(call/cc k)), `(,(lambda () x)).
//   - *ValidatedLiteral wrapping a FORM rather than self-evaluating data. The
//     passthrough registry (register.go) and the macro validators park
//     cond-expand, include, let-syntax, with-syntax and friends here; their
//     bodies are compiled later, in their own validation unit.
//
// WalkSubExprs treats both as childless, which is correct — there are no
// *validated* children to walk — but it means every analysis built on it
// (escape marking, mutable marking, capture detection, StableInUnit) silently
// concludes "nothing in there." That is not conservative, it is simply blind,
// and the review of 2026-07-13 found it miscompiling in two directions:
//
//   - a set! hidden in one is invisible, so a let-bound lambda is INLINED with
//     its stale pre-set! body ((let ((f (lambda () 7))) `(,(set! f (lambda ()
//     99)))  (f)) returned 7, not 99), and a legal same-unit top-level set! is
//     wrongly REJECTED under the default immutable top level;
//   - a closure or call/cc hidden in one does not disqualify in-place frame
//     reuse, so codegen arms OpSelfTailCall and a live continuation's frame is
//     rebound underneath it — the resumed continuation reads slots that now
//     hold another iteration's values, or values of another TYPE entirely.
//
// The stance taken here is the one the quasiquote capture check already took and
// stated: an un-analysed subtree counts as unsafe. Everything below is an
// over-approximation in the safe direction, never an assumption of absence.
//
// Why a raw scan for `set!` would NOT be enough. It is tempting to walk the raw
// syntax looking for set! in operator position and mark only those targets. That
// is unsound: the subtree is pre-expansion for its own unit, so a macro inside it
// can expand TO a set! whose target never appears as the literal symbol `set!`
// here. Missing one of those puts the silent inliner miscompile straight back.
// So every symbol the subtree mentions is treated as a potential set! target.
//
// Why macro TEMPLATES are not in the hazard set. Validation runs AFTER expansion
// (compilation.ExpandAndCompile: expand, then CompileExpression → ValidateExpression).
// A define-syntax template is therefore never runtime code in this scope — by the
// time we validate, any use of the macro has already been substituted, and the
// substituted set! is an ordinary validated form this unit sees. It is the forms
// that SURVIVE expansion — cond-expand, include, and a quasiquote template — that
// are genuinely opaque here.

// opaqueRawSyntax reports whether an expression is an opaque subtree, and returns
// the raw syntax it conceals. It is the single place that decides what "opaque"
// means; every consumer below and in capture_operator.go asks through it.
//
// The two return values answer two DIFFERENT questions, and conflating them is a
// fail-open bug. Opacity is a property of the NODE ("this analysis cannot see
// inside"); the raw syntax is merely the payload to scan, and it may be nil. A
// *ValidatedQuasiquote with a nil Template is still opaque — a nil payload is when
// we know least, not most, so reporting it as transparent would be exactly
// backwards. Callers that scan must nil-guard the payload; callers that ask
// "is this unsafe?" must use only the boolean.
func opaqueRawSyntax(expr ValidatedExpr) (syntax.SyntaxValue, bool) {
	switch e := expr.(type) {
	case *ValidatedQuasiquote:
		return e.Template, true

	case *ValidatedLiteral:
		// A *ValidatedLiteral is overloaded: genuine self-evaluating data
		// (numbers, strings, booleans, the empty list) AND passthrough forms.
		// Only the latter conceal code. A form is a non-empty syntax pair;
		// self-evaluating data never is.
		pair, ok := e.Value.(*syntax.SyntaxPair)
		if !ok || pair.IsEmptyList() {
			return nil, false
		}
		return pair, true
	}
	return nil, false
}

// forEachRawSymbol calls fn for every SyntaxSymbol anywhere in a raw syntax tree,
// including inside quoted data. Quoted symbols cannot be mutated, so visiting
// them costs precision but not soundness — and telling quoted from unquoted here
// needs a nesting-aware quasiquote walk, which is the deferred refinement, not the
// fix.
//
// The spine is walked iteratively so that a long list costs no Go stack; only
// genuine nesting (car, vector element) recurses.
func forEachRawSymbol(v values.Value, fn func(*syntax.SyntaxSymbol)) {
	switch e := v.(type) {
	case nil:
		return

	case *syntax.SyntaxSymbol:
		fn(e)

	case *syntax.SyntaxPair:
		for cur := e; cur != nil; {
			forEachRawSymbol(cur.Car(), fn)
			next, ok := cur.Cdr().(*syntax.SyntaxPair)
			if !ok {
				forEachRawSymbol(cur.Cdr(), fn)
				return
			}
			cur = next
		}

	case *syntax.SyntaxVector:
		for _, elem := range e.Values {
			forEachRawSymbol(elem, fn)
		}
	}
}

// markOpaqueSubtree records every binding an opaque subtree mentions as a
// possible set! target, in both the local and the global namespace — the same two
// arms validateSetBang uses for a set! it CAN see, applied to every name a subtree
// it CANNOT see could be setting.
//
// This is what makes the analysis conservative rather than blind. Marking a name
// mutated is the permissive direction for every consumer: a mutable let binding is
// heap-allocated rather than inlined, and a non-StableInUnit define is left
// assignable rather than frozen. An under-mark costs correctness, silently.
//
// What an over-mark costs, precisely. Not merely "an optimization" — say what is
// withdrawn. A non-StableInUnit define never gets BindingMeta.Stable, and top-level
// immutability is ENFORCED off that same stamp: the set! rejection
// (compilation.compileSetBang, keyed on binding.IsStable()) and the redefine guard
// (compilation.compileDefine, keyed on Meta().Stable) both read it. So over-marking
// a name silently turns top-level immutability OFF for it: given
// (begin (define x 1) `(x)), a later unit's (set! x 2) COMPILES, where without the
// quasiquote it is rejected.
//
// That is a real cost to a user-facing guarantee, and it is still safe, for a reason
// worth stating rather than assuming: enforcement and optimization key on the SAME
// flag, so an over-mark withdraws both together. No consumer treats a binding as
// immutable without reading Stable, so there is no reader left holding an assumption
// the admitted set! could falsify. The two do not want opposite error directions from
// this data; the set! rejection exists to protect the anchor, so it wants the same
// direction the optimizer does.
//
// The imprecision, not the direction, is the defect: see forEachRawSymbol, which
// marks template DATA that no unquote can reach.
func markOpaqueSubtree(env *environment.EnvironmentFrame, raw values.Value, result *ValidationResult) {
	if result == nil || raw == nil {
		return
	}
	forEachRawSymbol(raw, func(sym *syntax.SyntaxSymbol) {
		if env != nil {
			ref := env.ResolveBindingRef(sym.Sym, sym.Scopes())
			if ref.IsLocal() {
				result.markMutated(ref)
			}
		}
		result.markMutated(environment.GlobalRef(sym.Key()))
	})
}
