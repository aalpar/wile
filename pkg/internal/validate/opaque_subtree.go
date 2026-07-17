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
// So every symbol in an EVALUATED position is treated as a potential set! target.
//
// The walk narrows by POSITION, never by what the code appears to do. Whether the
// compiler would evaluate a position is a syntactic fact this package can decide
// (forEachRawSymbol / quasiHeadDepth); what an unevaluated macro would expand to is
// not. Skipping a symbol because it is quasiquoted data is therefore safe in a way
// that skipping one because "it does not look like a set!" never is.
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

// quasiDepthCode is the entry depth for a subtree that is ordinary CODE: a
// passthrough body, an include, a cond-expand. Every symbol in it is a live
// reference until a quote or quasiquote says otherwise.
//
// quasiDepthTemplate is the entry depth for a quasiquote TEMPLATE, which the
// caller has already stepped into. Its symbols are DATA until an unquote brings
// them back to quasiDepthCode. This mirrors the compiler's own convention
// (compilation.compileQuasiquoteDatum: "depth=1 means we're inside one level of
// quasiquote"; depth 0 is the trigger to evaluate).
const (
	quasiDepthCode     = 0
	quasiDepthTemplate = 1
)

// The quasiquote keywords this walk recognizes, matched by name.
const (
	quoteKey           = "quote"
	quasiquoteKey      = "quasiquote"
	unquoteKey         = "unquote"
	unquoteSplicingKey = "unquote-splicing"
)

// quasiHeadDepth reports how to walk a form whose head symbol is key, sitting at
// quasiquote depth quasi. argDepth is the depth for the form's ARGUMENTS (its cdr);
// barrier reports that nothing inside the form is live, so the walk skips it whole.
//
// This predicate is the soundness boundary of the whole file. It must agree with
// the compiler's quasiquote walk (compilation.quasiquoteNeedsRuntime /
// expandQuasi), because that walk is what decides which positions are actually
// evaluated. Where they disagree in the permissive direction (this says data, the
// compiler says live) a hidden set! goes unmarked and the inliner miscompiles
// silently — the exact failure this file exists to prevent.
//
// The rules the compiler and the shipped prior art
// (compilation.renamePair, which does this for macro-binder renaming) both hold to:
//
//   - quasiquote: arguments are one level deeper.
//   - unquote / unquote-splicing: one level shallower, but ONLY when already
//     inside a template. At depth 0 an unquote head is not an escape.
//   - quote: a barrier at depth 0 (a real quote form is pure datum). NOT special
//     inside a template — the compiler does not special-case it there, so its
//     nested unquotes stay live (R7RS §4.2.6).
//   - anything else: arguments stay at the current depth.
func quasiHeadDepth(key string, quasi int) (argDepth int, barrier bool) {
	switch key {
	case quasiquoteKey:
		return quasi + 1, false

	case unquoteKey, unquoteSplicingKey:
		// Only an escape from inside a template. At depth 0 an unquote head is an
		// ordinary form, and decrementing would run the depth negative.
		if quasi == quasiDepthCode {
			return quasi, false
		}
		return quasi - 1, false

	case quoteKey:
		// A real quote form is pure datum. Inside a template it is an ordinary
		// datum symbol whose nested unquotes stay live (R7RS §4.2.6), which is why
		// the barrier is depth-0-only and the compiler's own walk has no quote case.
		if quasi == quasiDepthCode {
			return quasi, true
		}
		return quasi, false
	}
	return quasi, false
}

// forEachRawSymbol calls fn for every SyntaxSymbol in a LIVE position of a raw
// syntax tree — a position the compiler would evaluate — and skips symbols that are
// data. quasi is the quasiquote nesting depth: quasiDepthCode (0) is an evaluated
// position, anything higher is inside a template.
//
// Data is skipped rather than visited because a symbol the compiler emits as a
// literal cannot be a set! target, and marking it withdraws the Stable stamp (and
// with it top-level immutability) from a name nothing can touch. See
// markOpaqueSubtree.
//
// Two traps, both live in the tests:
//
//   - Dotted unquote. `(a . ,x) parses as (a unquote x): a BARE unquote symbol in
//     the spine followed by exactly one element, not a sub-pair head (R7RS §4.2.8).
//     The compiler handles it in quasiquoteNeedsRuntimeList; a spine walk that only
//     inspects pair heads misses it and under-marks x.
//   - Nested depth. `(a `(b ,,(set! x 1))) brings the inner form back to depth 0
//     through two unquotes; one unquote leaves it at depth 1, still data.
//
// The spine is walked iteratively so that a long list costs no Go stack; only
// genuine nesting (car, vector element) recurses.
func forEachRawSymbol(v values.Value, quasi int, fn func(*syntax.SyntaxSymbol)) {
	switch e := v.(type) {
	case nil:
		return

	case *syntax.SyntaxSymbol:
		if quasi != quasiDepthCode {
			return
		}
		fn(e)

	case *syntax.SyntaxPair:
		forEachRawSymbolPair(e, quasi, fn)

	case *syntax.SyntaxVector:
		// A vector at depth 0 is a self-evaluating literal: its elements are data.
		// Inside a template they may carry unquotes, so they must be walked.
		if quasi == quasiDepthCode {
			return
		}
		for _, elem := range e.Values {
			forEachRawSymbol(elem, quasi, fn)
		}
	}
}

// forEachRawSymbolPair walks a pair, dispatching on a quasiquote keyword head via
// quasiHeadDepth before falling through to an ordinary car/cdr walk.
//
// Heads are matched by NAME. That is the same limitation the prior art accepts
// (compilation.renamePair): a program that lexically shadows quote/quasiquote/
// unquote would be misread. Detecting that needs binding information this walk does
// not carry.
func forEachRawSymbolPair(p *syntax.SyntaxPair, quasi int, fn func(*syntax.SyntaxSymbol)) {
	head, ok := p.SyntaxCar().(*syntax.SyntaxSymbol)
	if ok {
		argDepth, barrier := quasiHeadDepth(head.Key(), quasi)
		if barrier {
			return
		}
		if argDepth != quasi {
			forEachRawSymbol(p.SyntaxCdr(), argDepth, fn)
			return
		}
	}
	for cur := p; cur != nil; {
		tail, isDotted := dottedUnquoteTail(cur, quasi)
		if isDotted {
			// `(a . ,x): the tail is evaluated, and it is the end of the spine.
			argDepth, barrier := quasiHeadDepth(unquoteKey, quasi)
			if !barrier {
				forEachRawSymbol(tail, argDepth, fn)
			}
			return
		}
		forEachRawSymbol(cur.Car(), quasi, fn)
		next, ok := cur.Cdr().(*syntax.SyntaxPair)
		if !ok {
			forEachRawSymbol(cur.Cdr(), quasi, fn)
			return
		}
		cur = next
	}
}

// dottedUnquoteTail reports whether cur is the (unquote x) tail that `(a . ,x)
// parses into — a bare unquote symbol in the SPINE followed by exactly one element
// (R7RS §4.2.8) — and returns the element that unquote evaluates.
//
// It is a spine shape, not a head shape: reached as a cdr, it never passes through
// the keyword dispatch in forEachRawSymbolPair. The compiler detects it the same
// way and in the same place (compilation.quasiquoteNeedsRuntimeList). Only
// meaningful inside a template; at quasiDepthCode a bare unquote is an ordinary
// symbol and the ordinary spine walk already marks the tail.
func dottedUnquoteTail(cur *syntax.SyntaxPair, quasi int) (syntax.SyntaxValue, bool) {
	if quasi == quasiDepthCode {
		return nil, false
	}
	sym, ok := cur.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok || sym.Key() != unquoteKey {
		return nil, false
	}
	cdrPair, ok := cur.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || cdrPair.Length() != 1 {
		return nil, false
	}
	return cdrPair.SyntaxCar(), true
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
// The imprecision, not the direction, was the defect, and it is fixed:
// forEachRawSymbol now marks only EVALUATED positions, so a template mentioning a
// name no unquote reaches no longer costs that name its stamp. What remains below
// is an over-approximation within the live positions — every symbol there is a
// candidate, because what a macro expands to is not knowable here.
// markOpaqueCode marks an opaque subtree that is ordinary CODE — a passthrough
// body, an include, a cond-expand. Every symbol in it is live until a quote or
// quasiquote inside it says otherwise.
func markOpaqueCode(env *environment.EnvironmentFrame, raw values.Value, result *ValidationResult) {
	markOpaqueSubtree(env, raw, quasiDepthCode, result)
}

// markOpaqueTemplate marks a quasiquote TEMPLATE, which the caller has already
// stepped into: its symbols are data until an unquote makes them live again. The
// depth split is the whole precision win — a template mentioning a name no unquote
// reaches must not cost that name its Stable stamp.
func markOpaqueTemplate(env *environment.EnvironmentFrame, raw values.Value, result *ValidationResult) {
	markOpaqueSubtree(env, raw, quasiDepthTemplate, result)
}

func markOpaqueSubtree(env *environment.EnvironmentFrame, raw values.Value, quasi int, result *ValidationResult) {
	if result == nil || raw == nil {
		return
	}
	forEachRawSymbol(raw, quasi, func(sym *syntax.SyntaxSymbol) {
		if env != nil {
			ref := env.ResolveBindingRef(sym.Sym, sym.Scopes())
			if ref.IsLocal() {
				result.markMutated(ref)
			}
		}
		result.markMutated(environment.GlobalRef(sym.Key()))
	})
}
