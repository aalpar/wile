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
//     let-syntax, with-syntax, an include the expander left (malformed, or no
//     file resolver) and friends here; their
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
// compiler would evaluate a position is a fact this package can decide
// (forEachRawSymbol / quasiHeadDepth) — structure, plus one resolution per head to
// ask which marker it denotes. The compiler decides the same way EXCEPT for a head
// that resolves to a non-form binding: validateForm (headDenotesSpecialForm) still
// compiles the form for a Predeclared slot and for a top-level variable reached
// from a body (validate.go, "STILL DIVERGENT"), so forEachDemotedOpener reads such
// a head both ways. What an unevaluated macro would expand to is not decidable. Skipping a symbol because it is
// quasiquoted data is therefore safe in a way that skipping one because "it does not
// look like a set!" never is.
//
// Why macro TEMPLATES are not in the hazard set. Validation runs AFTER expansion
// (compilation.ExpandAndCompile: expand, then CompileExpression → ValidateExpression).
// A define-syntax template is therefore never runtime code in this scope — by the
// time we validate, any use of the macro has already been substituted, and the
// substituted set! is an ordinary validated form this unit sees. It is the forms
// that SURVIVE expansion — a quasiquote template, let-syntax, and an include the
// expander left unchanged (malformed, or no file resolver) — that are genuinely
// opaque here. cond-expand and a spliced include do not survive: the expander
// replaces them with their forms.

// opaqueRawSyntax reports whether an expression is an opaque subtree, and returns
// the raw syntax it conceals together with the environment that syntax's heads
// denote in. It is the single place that decides what "opaque" means; every
// consumer below and in capture_operator.go asks through it.
//
// The return values answer DIFFERENT questions, and conflating the first two is a
// fail-open bug. Opacity is a property of the NODE ("this analysis cannot see
// inside"); the raw syntax is merely the payload to scan, and it may be nil. A
// *ValidatedQuasiquote with a nil Template is still opaque — a nil payload is when
// we know least, not most, so reporting it as transparent would be exactly
// backwards. Callers that scan must nil-guard the payload; callers that ask
// "is this unsafe?" must use only the boolean.
//
// WHY THE NODE OWNS THE ENVIRONMENT rather than each consumer supplying one.
// Marker recognition is a RESOLUTION (markerName), so it has an answer only
// relative to a frame, and the frame that matters is the one the form was
// validated in — the same one the compiler will expand its template in. The
// consumers stand somewhere else: the boxing pass asks from the enclosing lambda
// or let (ref_index.go), the or-shaped lowering from wherever the let is being
// compiled or walked (or_lower.go, self_tail.go), and those are ancestors, or
// different frames for the same node between two passes. Letting each supply its
// own frame makes the answer depend on WHO ASKED, and two consumers that must
// agree — compileOrShapedLet's frame elision and tailExprHasSelfCall's pop count
// — would be asking from different ones. Carrying it on the node makes the
// answer a property of the subtree, which is what it is.
func opaqueRawSyntax(expr ValidatedExpr) (syntax.SyntaxValue, *environment.EnvironmentFrame, bool) {
	switch e := expr.(type) {
	case *ValidatedQuasiquote:
		return e.Template, e.Env, true

	case *ValidatedLiteral:
		// A *ValidatedLiteral is overloaded: genuine self-evaluating data
		// (numbers, strings, booleans, the empty list) AND passthrough forms.
		// Only the latter conceal code. A form is a non-empty syntax pair;
		// self-evaluating data never is.
		pair, ok := e.Value.(*syntax.SyntaxPair)
		if !ok || pair.IsEmptyList() {
			return nil, nil, false
		}
		return pair, e.Env, true
	}
	return nil, nil, false
}

// IsOpaqueSubtree reports whether an expression is an opaque subtree — the
// boolean half of opaqueRawSyntax, without the payload.
//
// It is exported because the classification is needed OUTSIDE this package and
// must not be re-derived there. pkg/machine/compilation's Pass 1 asks the same
// question for a different reason: an opaque subtree may hold an outer
// reference free-variable enumeration cannot name, so the closure keeps its
// static link. That is "not enumerable ⇒ retain"; this package's own consumer
// (bodyReferencesCaptureOperator) reads the same shapes as "not analysable ⇒
// may capture".
//
// Two justifications, one classification, and the frame-release gates depend on
// them agreeing: a closure whose template retains the lexical env pins the
// creating frame, and the only thing keeping the release gates off such a body
// is that the capture walk refuses it too. That agreement was previously an
// unenforced coincidence between this function and a hand-copy in compilation.
// Adding a third opaque shape to opaqueRawSyntax now moves both walks at once.
func IsOpaqueSubtree(expr ValidatedExpr) bool {
	_, _, ok := opaqueRawSyntax(expr)
	return ok
}

// ForEachOpaqueLiveSymbol calls fn for every symbol in a LIVE position of expr's
// concealed syntax, and reports whether expr was opaque at all. LIVE here is
// walkReads: the evaluated positions plus the data symbols of a quasisyntax
// template, which the runtime path reads as bindings. The boxing scan takes this
// mode defensively: over-reporting only over-boxes, and no test discriminates it
// from walkWrites. It does NOT
// descend into validated children: a caller that already walks the tree (every
// caller so far does) recurses itself and asks this at each node.
//
// It is exported so that pkg/machine/compilation's boxing pass can ask the
// question this package already answers, instead of concluding "nothing in
// there" from WalkSubExprs reporting an opaque node childless. That blindness is
// what left a `set!` inside a quasiquote or a passthrough form invisible to the ref
// index, so a captured local was never boxed and the closure kept a stale copy —
// (let ((n 0)) (let ((f (lambda () n))) `(,(set! n 99)) f)) returned 0.
//
// WHY NOT EXPORT forEachRawSymbol. Its parameters are the entry depth, the marker
// set and the environment the payload's heads denote in, and all three are decided
// by the NODE and the consumer, not the caller: the depth differs by SHAPE (a
// quasiquote Template has already been stepped into, a passthrough form has not),
// the environment is the one the form was validated in, not the one the caller
// happens to stand in, and the walkMode is the consumer's. Exporting the raw
// walker moves those choices to the call site, where getting any of them wrong
// under-marks silently — the exact failure mode this file exists to prevent. The
// classification stays here, with the switch that defines it.
//
// The boolean and fn answer DIFFERENT questions, per opaqueRawSyntax's contract:
// an opaque node with a nil payload calls fn zero times and still reports true,
// because a nil payload is when we know least.
func ForEachOpaqueLiveSymbol(expr ValidatedExpr, fn func(*syntax.SyntaxSymbol)) bool {
	if expr == nil {
		return false
	}
	raw, env, opaque := opaqueRawSyntax(expr)
	if !opaque {
		return false
	}
	if raw == nil {
		return true
	}
	forEachRawSymbol(env, raw, opaqueEntryDepth(expr), quasiquoteKW, walkReads, fn)
	return true
}

// opaqueEntryDepth reports the quasiquote depth an opaque node's payload is
// entered at. A *ValidatedQuasiquote's Template is the form's ARGUMENT, so the
// caller has already stepped one level in; every other opaque node conceals
// ordinary code, quasisyntax included (a passthrough form, entered at code depth,
// where quasiHeadDepth switches to its markers). This is the same split
// markOpaqueTemplate / markOpaqueCode make, in one place so a third opaque shape
// moves every walk at once. Callers pass quasiquoteKW as the marker set: it is
// read only at a template depth, and a *ValidatedQuasiquote is always that dialect.
func opaqueEntryDepth(expr ValidatedExpr) int {
	_, isQuasi := expr.(*ValidatedQuasiquote)
	if isQuasi {
		return quasiDepthTemplate
	}
	return quasiDepthCode
}

// quasiDepthCode is the entry depth for a subtree that is ordinary CODE: a
// passthrough body, a surviving include. Every symbol in it is a live
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

// quoteKey is the one marker that is not part of a quasi dialect: a real quote
// form is a barrier at code depth, and inside a template it is ordinary data.
const quoteKey = "quote"

// quasiMarkers is one dialect's three template markers, by the form each DENOTES.
// It mirrors the marker fields of compilation.quasiKeywords (unquote, splicing,
// nesting) and nothing else: quasiKeywords.quoting is the compiler's synthesis head
// and nestingAlwaysRuntime picks its expansion path. The two tables are written out
// twice, in the same order and under the same names (quasiquoteKW, quasisyntaxKW),
// because validate sits below compilation and the only shared source, the
// constants in pkg/parser, would add an import this package does not otherwise
// need. That leaves no compile-time check that they agree, and a drift test cannot
// reach the other table without an import cycle; the dialect-local rows in
// pkg/wile/quasisyntax_quote_depth_test.go are what would catch one.
type quasiMarkers struct {
	unquote  string
	splicing string
	nesting  string
}

var quasiquoteKW = quasiMarkers{
	unquote:  "unquote",
	splicing: "unquote-splicing",
	nesting:  "quasiquote",
}

var quasisyntaxKW = quasiMarkers{
	unquote:  "unsyntax",
	splicing: "unsyntax-splicing",
	nesting:  "quasisyntax",
}

// walkMode says which positions of an opaque subtree a walk reports. The two
// consumers want different sets because they guard against different failures.
type walkMode int

const (
	// walkWrites reports the positions the compiler EVALUATES, and nothing it
	// emits as a literal. Its consumer is markOpaqueSubtree: a name it reports
	// loses its Stable stamp, so reporting template data would withdraw top-level
	// immutability from a name nothing can set!.
	walkWrites walkMode = iota

	// walkReads additionally reports the symbols of a quasisyntax template at
	// template depth. On the runtime path expandQuasi emits (syntax sym) for them,
	// and (syntax sym) on a bound local yields that local's VALUE
	// (compile_syntax_form.go; (quasisyntax (a n)) alone yields #'n, while
	// (quasisyntax (a n #,1)) yields the value), so they are references the
	// or-shaped lowering (opaqueSubtreeMentions) must see even though nothing is
	// assigned. The boxing scan shares the mode defensively. Over-reporting here
	// costs a box or a kept frame, never a stamp.
	// Quasiquote data stays unreported in both modes: it compiles to (quote sym).
	walkReads
)

// markerName returns the marker a head identifier names: the form it DENOTES in
// env, and its spelling when it denotes none.
//
// This is compilation.headFormName, restated here rather than shared because the
// two packages sit on opposite sides of the validate → machine edge; the
// primitive both reduce to is environment.DenotedForm, and neither adds anything
// to it. Keeping them the same function in two places is the price of the
// layering, and quasiHeadDepth's doc is where the agreement is stated as a
// requirement.
//
// A nil env means spelling-only. That is what a node carrying no environment
// gets (opaqueRawSyntax), and it is also what the whole walk did before markers
// could be renamed.
//
// The resolution is TryGetBinding, not GetBinding, because this head is a
// DATUM: an incomparable scope-set tie here is not a reference the program got
// wrong, so it answers "denotes no form" and falls through to the spelling,
// exactly as an unbound head does. GetBinding would raise out of a walk whose
// whole job is to be conservative, turning a hygiene curiosity into a
// compile-time crash.
//
// The definition-site pin is read first, as headFormName reads it: a marker
// renamed where its template was written is unbound where the template is used.
func markerName(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol) string {
	denoted, _, _ := resolveHead(env, sym)
	if denoted != "" {
		return denoted
	}
	return sym.Key()
}

// resolveHead is the one resolution markerName and headKey share: the form sym
// DENOTES in env ("" when it denotes none), the binding it resolved to (nil when
// it resolved to none, and also on an ambiguous scope-set tie), and whether it
// tied. The definition-site pin is read first.
func resolveHead(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol) (string, *environment.Binding, bool) {
	if env == nil {
		return "", nil, false
	}
	denoted := environment.DenotedForm(environment.PinnedBinding(sym))
	if denoted != "" {
		return denoted, nil, false
	}
	bnd, ambiguous := env.TryGetBinding(sym.Sym, syntax.ScopesOf(sym.Scopes()))
	return environment.DenotedForm(bnd), bnd, ambiguous
}

// headKey is markerName for a head met during the walk, and reports whether the
// head was DEMOTED: at code depth it resolves to a binding that denotes no form.
// The compiler dispatches on what a head denotes, so a lambda-bound local named
// quasisyntax, quote or quasiquote is an ordinary call and its operands are
// evaluated; a demoted head yields key "" for the caller to walk as a call.
//
// The compiler is NARROWER than "resolves to a non-form binding", though.
// validateForm (headDenotesSpecialForm) still compiles the form when the head
// reaches a Predeclared slot, which a LATER (define quasisyntax ...) in the same
// body mints, and when it reaches a top-level variable from inside a let or
// lambda body (the divergence documented there as STILL DIVERGENT). So for a head
// spelled quasiquote or quasisyntax the caller must not trust the demotion alone;
// see forEachRawSymbolPair. A demoted quote is safe as an ordinary call, because
// the code-depth walk reports everything its barrier would have hidden.
//
// An ambiguous scope-set tie at code depth demotes too. Demotion only ever adds
// to what the walk reports (a demoted quote walks its operands, a demoted opener
// is read both ways), so it is the conservative answer for every consumer here.
// Reading the tie as the marker was safe only while every frame the walk resolved
// in was one the compiler also resolves in, where GetBinding raises on the same
// tie; the binder frames forEachRawBinderForm mints from raw syntax are not, so a
// tie among them proves nothing about what the compiler will see.
func headKey(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol, quasi int) (string, bool) {
	denoted, bnd, ambiguous := resolveHead(env, sym)
	if denoted != "" {
		return denoted, false
	}
	if quasi == quasiDepthCode && (bnd != nil || ambiguous) {
		return "", true
	}
	return sym.Key(), false
}

// quasiHeadDepth reports how to walk a form whose head DENOTES key, sitting at
// quasiquote depth quasi inside a template of dialect mk. argDepth is the depth
// for the form's ARGUMENTS (its cdr) and argMarkers the dialect they are read in;
// barrier reports that nothing inside the form is live, so the walk skips it whole.
//
// This predicate is the soundness boundary of the whole file. It must agree with
// the compiler's quasiquote walk (compilation.quasiNeedsRuntime /
// expandQuasi), because that walk is what decides which positions are actually
// evaluated. Where they disagree in the permissive direction (this says data, the
// compiler says live) a hidden set! goes unmarked and the inliner miscompiles
// silently — the exact failure this file exists to prevent.
//
// The agreement is on the KEY as well as the rules, which is why key is what a
// head denotes and not what it spells: compilation.quasiKeywords.headName
// resolves a head through its binding and falls back to the spelling, and
// markerName is that same function on this side. A spelling-only reading here
// left `(,(set! f …)) unmarked whenever unquote arrived renamed or prefixed, and
// the inliner then kept f's stale body — exactly the permissive disagreement the
// paragraph above forbids, measured in
// pkg/wile/renamed_inner_positions_test.go's TestRenamedQuasiquoteMarkerIsVisibleToTheOpaqueScan.
//
// The rules the compiler's own walk holds to, and that this predicate must match:
//
//   - the dialect's nesting head (quasiquote, quasisyntax): arguments are one
//     level deeper. At depth 0 EITHER dialect's nesting head opens a template and
//     selects its markers; inside a template the other dialect's head is data.
//   - the dialect's unquote / unquote-splicing (unquote, unsyntax): one level
//     shallower, but ONLY when already inside a template. At depth 0 such a head
//     is not an escape, and in a template of the other dialect it is data.
//   - quote: a barrier at depth 0 (a real quote form is pure datum). NOT special
//     inside a template — the compiler does not special-case it there, so its
//     nested unquotes stay live (R7RS §4.2.6). That is the quasisyntax case this
//     parameter exists for: `#,` inside a quoted subtree of a quasisyntax template
//     fires, and a walk that read quasisyntax as an ordinary head at depth 0 hit
//     the quote barrier and never saw it.
//   - anything else: arguments stay at the current depth.
//
// Not modeled: compilation.quasiKeywords.nestingAlwaysRuntime. It selects the
// EXPANSION path for a nested quasisyntax at depth 1, and the two paths differ in
// what they emit for a bound identifier ((syntax z) yields z's value). That is a
// difference in READS, not in what runs: an unsyntax at depth 2 stays data on both,
// so its set! does not execute, and every symbol in a quasisyntax template is a
// read on both, which is why walkReads reports template data and walkWrites does
// not. The depth-2 behaviour rows in pkg/wile/quasisyntax_quote_depth_test.go pin
// the VALUES the scan must agree with; they do not exercise the scan.
func quasiHeadDepth(key string, quasi int, mk quasiMarkers) (argDepth int, argMarkers quasiMarkers, barrier bool) {
	if quasi == quasiDepthCode {
		// Code. Either dialect's nesting head opens a template of its own, and a
		// real quote form is pure datum. Unquote and unquote-splicing heads are
		// ordinary forms here, and decrementing would run the depth negative.
		switch key {
		case quasiquoteKW.nesting:
			return quasiDepthTemplate, quasiquoteKW, false
		case quasisyntaxKW.nesting:
			return quasiDepthTemplate, quasisyntaxKW, false
		case quoteKey:
			return quasi, mk, true
		}
		return quasi, mk, false
	}

	// Inside a template only ITS dialect's markers count. The other dialect's
	// nesting head is data, and so is quote: the compiler's walk has no quote
	// case in a template, so its nested unquotes stay live (R7RS §4.2.6).
	switch key {
	case mk.nesting:
		return quasi + 1, mk, false
	case mk.unquote, mk.splicing:
		return quasi - 1, mk, false
	}
	return quasi, mk, false
}

// forEachRawSymbol calls fn for every SyntaxSymbol in a LIVE position of a raw
// syntax tree — a position the compiler would evaluate — and, in walkWrites mode,
// skips symbols that are data. quasi is the quasiquote nesting depth: quasiDepthCode (0) is an evaluated
// position, anything higher is inside a template. mk is the dialect of the
// template being walked (read only when quasi is above quasiDepthCode, and
// switched by quasiHeadDepth when a nesting head opens a template), and mode picks
// which template positions are reported (see walkMode).
//
// In walkWrites mode data is skipped rather than visited because a symbol the
// compiler emits as a literal cannot be a set! target, and marking it withdraws
// the Stable stamp (and with it top-level immutability) from a name nothing can
// touch. See markOpaqueSubtree. walkReads keeps quasisyntax template data, which
// the compiler reads rather than quotes.
//
// Two traps, both live in the tests:
//
//   - Dotted unquote. `(a . ,x) parses as (a unquote x): a BARE unquote symbol in
//     the spine followed by exactly one element, not a sub-pair head (R7RS §7.1.4).
//     The compiler handles it in dottedTailCell; a spine walk that only inspects
//     pair heads misses it and under-marks x. `(a . `x) is the same shape one
//     level deeper (dottedTailMarker).
//   - Nested depth. `(a `(b ,,(set! x 1))) brings the inner form back to depth 0
//     through two unquotes; one unquote leaves it at depth 1, still data.
//
// The spine is walked iteratively so that a long list costs no Go stack; only
// genuine nesting (car, vector element) recurses.
func forEachRawSymbol(env *environment.EnvironmentFrame, v values.Value, quasi int, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) {
	switch e := v.(type) {
	case nil:
		return

	case *syntax.SyntaxSymbol:
		if quasi == quasiDepthCode || (mode == walkReads && mk == quasisyntaxKW) {
			fn(e)
		}

	case *syntax.SyntaxPair:
		forEachRawSymbolPair(env, e, quasi, mk, mode, fn)

	case *syntax.SyntaxVector:
		// A vector at depth 0 is a self-evaluating literal: its elements are data.
		// Inside a template they may carry unquotes, so they must be walked.
		if quasi == quasiDepthCode {
			return
		}
		for _, elem := range e.Values {
			forEachRawSymbol(env, elem, quasi, mk, mode, fn)
		}
	}
}

// forEachRawSymbolPair walks a pair, dispatching on a quasiquote marker head via
// quasiHeadDepth before falling through to an ordinary car/cdr walk.
//
// Heads are matched by what they DENOTE in env, falling back to the spelling when
// they denote no form (headKey, and markerName for the dotted tail) — the rule compilation.quasiKeywords.headName
// uses, so a renamed or prefixed unquote is a marker on both sides, and a lexical
// shadow of a RENAMED marker name is a marker on neither (the shadow's spelling no
// longer matches the marker, so neither the denotation nor the spelling fallback
// recognizes it). That does NOT hold for the CANONICAL spelling: the fallback
// reads spelling whenever the head denotes no form, and a local binding of
// `unquote` still denotes no form, so it still reads as the marker by spelling —
// measured, `(let ((unquote (lambda (x) x))) `(1 ,2))` still answers `(1 2)`, not
// `(1 (unquote 2))`. With a nil env the walk is spelling-only and the renamed-shadow
// case is misread; opaqueRawSyntax says which nodes carry one.
func forEachRawSymbolPair(env *environment.EnvironmentFrame, p *syntax.SyntaxPair, quasi int, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) {
	head, ok := p.SyntaxCar().(*syntax.SyntaxSymbol)
	if ok {
		key, demoted := headKey(env, head, quasi)
		if demoted && (head.Key() == quasiquoteKW.nesting || head.Key() == quasisyntaxKW.nesting) {
			forEachDemotedOpener(env, p, head, quasi, mk, mode, fn)
			return
		}
		if quasi == quasiDepthCode && forEachRawBinderForm(env, p, head, key, mk, mode, fn) {
			return
		}
		argDepth, argMarkers, barrier := quasiHeadDepth(key, quasi, mk)
		if barrier {
			return
		}
		// The marker-set comparison cannot differ from the depth one today (every
		// dialect switch also changes the depth); it keeps a future same-depth
		// switch from being walked in the old dialect.
		if argDepth != quasi || argMarkers != mk {
			forEachRawSymbol(env, p.SyntaxCdr(), argDepth, argMarkers, mode, fn)
			return
		}
	}
	var end syntax.SpineEnd
	for cur, e := range syntax.Spine(p) {
		marker, tail, isDotted := dottedTailMarker(env, cur, quasi, mk)
		if isDotted {
			// `(a . ,x) or `(a . `x): the marker governs the tail's depth, and the
			// tail is the end of the spine.
			argDepth, argMarkers, barrier := quasiHeadDepth(marker, quasi, mk)
			if !barrier {
				forEachRawSymbol(env, tail, argDepth, argMarkers, mode, fn)
			}
			return
		}
		forEachRawSymbol(env, cur.Car(), quasi, mk, mode, fn)
		end = e
	}
	// The dotted-unquote arm above returns outright, so reaching here means the
	// walk ran to a terminator. Improper() screens out the proper-list case,
	// where the old walk handed SyntaxEmptyList to forEachRawSymbol as a no-op.
	if end.Improper() {
		forEachRawSymbol(env, end.Tail, quasi, mk, mode, fn)
	}
}

// forEachDemotedOpener walks a form whose head is spelled quasiquote or
// quasisyntax and resolved to a binding denoting no form, so the compiler may
// read it either way (see headKey): as a call, or as the opener it still is when
// the slot is Predeclared or reached from a body. It reports the head and the
// union of both readings of the operands, which over-approximates and so costs an
// optimization at worst. Reading it only as a call let a template opener hide a
// quote barrier, and the unquote beneath it went unreported.
func forEachDemotedOpener(env *environment.EnvironmentFrame, p *syntax.SyntaxPair, head *syntax.SyntaxSymbol, quasi int, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) {
	fn(head)
	argDepth, argMarkers, _ := quasiHeadDepth(head.Key(), quasi, mk)
	cdr, ok := p.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok {
		return
	}
	// Element by element: walking the cdr as a form would read its first operand
	// as a head and dispatch on it.
	var end syntax.SpineEnd
	for cur, e := range syntax.Spine(cdr) {
		forEachRawSymbol(env, cur.Car(), quasi, mk, mode, fn)
		forEachRawSymbol(env, cur.Car(), argDepth, argMarkers, mode, fn)
		end = e
	}
	if end.Improper() {
		forEachRawSymbol(env, end.Tail, quasi, mk, mode, fn)
		forEachRawSymbol(env, end.Tail, argDepth, argMarkers, mode, fn)
	}
}

// forEachRawBinderForm walks a core binding form met at code depth, resolving
// the heads in each part in the frame the compiler will resolve them in, and
// reports whether it took the form. False means key names no binding form, the
// node carries no environment, or the shape is not one it parses; the caller then
// walks the form the ordinary way, which is what it did before binders were read.
//
// The frames are why. A head resolves in the walk's env, and the node's env holds
// none of the binders the raw subtree itself introduces, so in
// ((lambda (quote) (quote (begin (set! f ...) 1))) list) the inner quote resolved
// to the special form and its barrier hid the set!. The compiler binds the
// parameter first and compiles that quote as a call. Each binder's frame is
// minted from the binder's own scope sets, as createLambdaValidationEnv and
// extendEnvWithSymbols mint them for validated code, so the inner quote reaches
// that binding by subset and headKey demotes it. A same-spelled identifier the
// binder's scopes do not cover still reaches the outer form.
//
// Each frame covers exactly the region the form's binders scope over: a lambda
// clause's parameters and a let's names cover the body only; let* adds one frame
// per binding, so each init sees the names before it; letrec and letrec* cover
// their inits as well; a named let's tag and names cover the body, its inits stay
// outside. A frame that leaked to a sibling or an outer init would demote a real
// quote and cost the names under it their Stable stamp.
//
// The head and every binder are still reported, as the ordinary walk reported
// them. Only the frame the parts are walked in changes, and fn's consumers resolve
// what they are handed in their own env, so a binder that shadows an outer name
// is over-reported against that name, never under-reported.
//
// A body's internal defines are binders too (walkRawBody). A derived binding form
// (do, let-values, a user macro) needs nothing here: the expander expands every
// operand a template fires before validation runs (compilation's
// expandQuasiOperands), so the walk meets the core forms it expands to. What the
// walk still cannot see into is a body that is not expanded before validation,
// such as a let-syntax body.
func forEachRawBinderForm(env *environment.EnvironmentFrame, p *syntax.SyntaxPair, head *syntax.SyntaxSymbol, key string, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) bool {
	if env == nil {
		return false
	}
	switch key {
	case "lambda", "case-lambda", "let", "let*", "letrec", "letrec*":
	default:
		return false
	}
	elems, improper := collectList(p)
	if improper || len(elems) < 2 {
		return false
	}
	switch key {
	case "lambda":
		if len(elems) < 3 {
			return false
		}
		params := validateParams(elems[1], &ValidationResult{})
		if params == nil {
			return false
		}
		fn(head)
		walkRawClause(createLambdaValidationEnv(env, params), params, elems[2:], mk, mode, fn)
		return true

	case "case-lambda":
		return forEachRawCaseLambda(env, head, elems[1:], mk, mode, fn)

	case "let", "let*", "letrec", "letrec*":
		return forEachRawLet(env, head, key, elems, mk, mode, fn)
	}
	return false
}

// forEachRawCaseLambda is forEachRawBinderForm's case-lambda arm. Every clause is
// parsed before anything is reported, so a malformed clause falls back to the
// ordinary walk without having reported half the form.
func forEachRawCaseLambda(env *environment.EnvironmentFrame, head *syntax.SyntaxSymbol, clauses []syntax.SyntaxValue, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) bool {
	type clause struct {
		params *ValidatedParams
		body   []syntax.SyntaxValue
	}
	parsed := make([]clause, 0, len(clauses))
	for _, c := range clauses {
		pair, ok := c.(*syntax.SyntaxPair)
		if !ok {
			return false
		}
		elems, improper := collectList(pair)
		if improper || len(elems) < 2 {
			return false
		}
		params := validateParams(elems[0], &ValidationResult{})
		if params == nil {
			return false
		}
		parsed = append(parsed, clause{params: params, body: elems[1:]})
	}
	fn(head)
	for _, c := range parsed {
		walkRawClause(createLambdaValidationEnv(env, c.params), c.params, c.body, mk, mode, fn)
	}
	return true
}

// walkRawClause reports a lambda clause's parameters and walks its body in
// bodyEnv, the frame that binds them.
func walkRawClause(bodyEnv *environment.EnvironmentFrame, params *ValidatedParams, body []syntax.SyntaxValue, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) {
	for _, sym := range params.Required {
		fn(sym)
	}
	if params.Rest != nil {
		fn(params.Rest)
	}
	walkRawBody(bodyEnv, body, mk, mode, fn)
}

// walkRawBody walks each form of a body at code depth, in env extended by the
// body's internal defines. Those scope over the whole body, their own values
// included (R7RS §5.3.2, letrec* semantics), so one frame covers every form.
func walkRawBody(env *environment.EnvironmentFrame, body []syntax.SyntaxValue, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) {
	bodyEnv := extendEnvWithSymbols(env, rawBodyDefines(env, body, nil))
	for _, form := range body {
		forEachRawSymbol(bodyEnv, form, quasiDepthCode, mk, mode, fn)
	}
}

// rawBodyDefines appends to q the names a body's internal defines bind: each
// (define name ...) or (define (name . formals) ...) at the body's top level or
// spliced through a begin, its head resolved in env. A shape it does not read
// binds nothing here, which leaves its heads resolving outside, as before.
func rawBodyDefines(env *environment.EnvironmentFrame, body []syntax.SyntaxValue, q []*syntax.SyntaxSymbol) []*syntax.SyntaxSymbol {
	for _, form := range body {
		pair, ok := form.(*syntax.SyntaxPair)
		if !ok {
			continue
		}
		head, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
		if !ok {
			continue
		}
		key, _ := headKey(env, head, quasiDepthCode)
		if key != "begin" && key != "define" {
			continue
		}
		elems, improper := collectList(pair)
		if improper || len(elems) < 2 {
			continue
		}
		switch key {
		case "begin":
			q = rawBodyDefines(env, elems[1:], q)
		case "define":
			target := elems[1]
			sig, isSig := target.(*syntax.SyntaxPair)
			if isSig {
				target = sig.SyntaxCar()
			}
			name, ok := target.(*syntax.SyntaxSymbol)
			if ok {
				q = append(q, name)
			}
		}
	}
	return q
}

// forEachRawLet is forEachRawBinderForm's let-family arm; elems is the whole
// form, head first. The bindings are parsed with the validator's own
// parseLetBindingPairs, so a shape the validator would refuse falls back too.
func forEachRawLet(env *environment.EnvironmentFrame, head *syntax.SyntaxSymbol, key string, elems []syntax.SyntaxValue, mk quasiMarkers, mode walkMode, fn func(*syntax.SyntaxSymbol)) bool {
	tag, named := elems[1].(*syntax.SyntaxSymbol)
	if named && key != "let" {
		return false
	}
	bindingsAt := 1
	if named {
		bindingsAt = 2
	}
	if len(elems) < bindingsAt+2 {
		return false
	}
	bindings, ok := parseRawLetBindings(elems[bindingsAt], key)
	if !ok {
		return false
	}
	body := elems[bindingsAt+1:]
	names := make([]*syntax.SyntaxSymbol, len(bindings))
	for i, b := range bindings {
		names[i] = b.name
	}

	fn(head)
	if named {
		fn(tag)
	}
	for _, name := range names {
		fn(name)
	}
	switch key {
	case "let*":
		cur := env
		for _, b := range bindings {
			forEachRawSymbol(cur, b.initStx, quasiDepthCode, mk, mode, fn)
			cur = extendEnvWithSymbols(cur, []*syntax.SyntaxSymbol{b.name})
		}
		walkRawBody(cur, body, mk, mode, fn)

	case "letrec", "letrec*":
		full := extendEnvWithSymbols(env, names)
		for _, b := range bindings {
			forEachRawSymbol(full, b.initStx, quasiDepthCode, mk, mode, fn)
		}
		walkRawBody(full, body, mk, mode, fn)

	default:
		for _, b := range bindings {
			forEachRawSymbol(env, b.initStx, quasiDepthCode, mk, mode, fn)
		}
		bodyEnv := env
		if named {
			// The validator's two frames (validateNamedLet): the tag, then the
			// names over it, so a name that repeats the tag shadows it.
			bodyEnv = extendEnvWithSymbols(bodyEnv, []*syntax.SyntaxSymbol{tag})
		}
		walkRawBody(extendEnvWithSymbols(bodyEnv, names), body, mk, mode, fn)
	}
	return true
}

// parseRawLetBindings parses a let-family bindings list, the empty list
// included, with the validator's parser. Its errors are discarded: a refusal here
// only sends the form back to the ordinary walk.
func parseRawLetBindings(stx syntax.SyntaxValue, key string) ([]rawLetBinding, bool) {
	if syntax.IsSyntaxEmptyList(stx) {
		return nil, true
	}
	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, false
	}
	list, improper := collectList(pair)
	if improper {
		return nil, false
	}
	return parseLetBindingPairs(list, key, &ValidationResult{})
}

// dottedTailMarker reports whether cur is a dotted-tail marker cell, and if so
// which marker and the element it governs. `(a . ,x) parses into (a unquote x)
// and `(a . `x) into (a quasiquote x): a bare marker symbol in the SPINE followed
// by exactly one element (R7RS §4.2.8). The unquote reading takes the tail one
// level shallower, the nesting reading one level deeper; quasiHeadDepth applies
// either once the caller hands it the marker.
//
// It is a spine shape, not a head shape: reached as a cdr, it never passes through
// the keyword dispatch in forEachRawSymbolPair. The compiler detects it the same
// way and in the same place (compilation.dottedTailCell, consulted from both
// quasiNeedsRuntimeList and expandQuasiList), and accepts the same two markers.
// Splicing is absent on both sides: a splice is an element, never a tail. Only
// meaningful inside a template; at quasiDepthCode a bare marker is an ordinary
// symbol and the ordinary spine walk already marks the tail.
//
// The nesting arm is not the conservative side to leave out. Read element-wise, a
// dotted `(a . `(b ,'(,x))) puts the unquote at depth 0 and its quote is a
// barrier; the compiler reads it one level deeper, where the quote is data and
// the inner unquote fires.
//
// The marker is identified the way every other marker head here is, by what it
// denotes with spelling as the fallback (markerName), because the compiler's
// dottedTailCell reads this same cell through kw.headName. A spelling-only test
// would miss `(a . ,x) written with a renamed unquote and under-mark x.
func dottedTailMarker(env *environment.EnvironmentFrame, cur *syntax.SyntaxPair, quasi int, mk quasiMarkers) (string, syntax.SyntaxValue, bool) {
	if quasi == quasiDepthCode {
		return "", nil, false
	}
	sym, ok := cur.SyntaxCar().(*syntax.SyntaxSymbol)
	if !ok {
		return "", nil, false
	}
	marker := markerName(env, sym)
	if marker != mk.unquote && marker != mk.nesting {
		return "", nil, false
	}
	cdrPair, ok := cur.SyntaxCdr().(*syntax.SyntaxPair)
	// Exactly one proper element: cdrPair is a pair whose cdr is the empty list.
	// IsSyntaxEmptyList never panics, where Length() panics on an improper list
	// like the tail of `(a unquote x . y) — a malformed template must reach a
	// clean validation path, not an internal-error crash.
	if !ok || !syntax.IsSyntaxEmptyList(cdrPair.SyntaxCdr()) {
		return "", nil, false
	}
	return marker, cdrPair.SyntaxCar(), true
}

// markOpaqueCode marks an opaque subtree that is ordinary CODE — a passthrough
// body, a surviving include. Every symbol in it is live until a quote or
// quasiquote inside it says otherwise.
func markOpaqueCode(env *environment.EnvironmentFrame, raw values.Value, result *ValidationResult) {
	markOpaqueSubtree(env, raw, quasiDepthCode, quasiquoteKW, result)
}

// markOpaqueTemplate marks a quasiquote TEMPLATE, which the caller has already
// stepped into: its symbols are data until an unquote makes them live again. The
// depth split is the whole precision win — a template mentioning a name no unquote
// reaches must not cost that name its Stable stamp.
func markOpaqueTemplate(env *environment.EnvironmentFrame, raw values.Value, result *ValidationResult) {
	markOpaqueSubtree(env, raw, quasiDepthTemplate, quasiquoteKW, result)
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
// (compilation.CompileValidatedSetBang, keyed on binding.IsStable()) and the
// redefine guard (compilation.declareDefineBinding, keyed on Meta().Stable) both
// read it. So over-marking a name silently turns top-level immutability OFF for
// it: given (begin (define x 1) `(,x)), a later unit's (set! x 2) COMPILES, where
// without the unquote it is rejected.
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
func markOpaqueSubtree(env *environment.EnvironmentFrame, raw values.Value, quasi int, mk quasiMarkers, result *ValidationResult) {
	if result == nil || raw == nil {
		return
	}
	forEachRawSymbol(env, raw, quasi, mk, walkWrites, func(sym *syntax.SyntaxSymbol) {
		if env != nil {
			// Every tied binder: validation cannot know which scopes expansion leaves
			// on this reference. If it stays tied the compiler refuses it, so the
			// extra marks cost nothing; if it settles, it settles on one of them.
			for _, ref := range env.ResolveBindingRefs(sym.Sym, syntax.ScopesOf(sym.Scopes())) {
				if ref.IsLocal() {
					result.markMutated(ref)
				}
			}
		}
		result.markMutated(environment.GlobalRef(sym.Key()))
	})
}
