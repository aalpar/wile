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
	"context"
	"slices"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// quasiKeywords holds the keyword names that distinguish quasiquote expansion
// from quasisyntax expansion. Both share the same structural logic; only the
// keyword strings (and whether dotted-pair unquote is supported) differ.
type quasiKeywords struct {
	unquote             string // "unquote" or "unsyntax"
	splicing            string // "unquote-splicing" or "unsyntax-splicing"
	nesting             string // "quasiquote" or "quasisyntax"
	quoting             string // "quote" or "syntax"
	handleDottedUnquote bool   // true for quasiquote (R7RS §4.2.8), false for quasisyntax

	// nestingAlwaysRuntime forces a nested form at depth 1 onto the runtime
	// path instead of asking whether anything inside it fires. True for
	// quasisyntax, and it is SEMANTICALLY LOAD-BEARING, not a conservative
	// approximation — the two paths disagree on a bound identifier:
	//
	//	(let ((z 5)) #`#`(a #,z))  =>  (quasisyntax (a (unsyntax 5)))
	//	(let ((z 5))  ``(a ,z))    =>  (quasiquote (a (unquote z)))
	//
	// The runtime expansion materializes (syntax z) for the inner argument,
	// and (syntax z) on a bound local yields that local's VALUE, where the
	// literal path would keep the symbol. Pinned by integration/
	// quasisyntax_test.go's nested rows and by pkg/wile/boxed_capture_test.go,
	// whose whole purpose is that the nested form takes the runtime path.
	// Do not "clean it up".
	nestingAlwaysRuntime bool
}

var quasiquoteKW = quasiKeywords{
	unquote:             "unquote",
	splicing:            "unquote-splicing",
	nesting:             "quasiquote",
	quoting:             "quote",
	handleDottedUnquote: true,
}

var quasisyntaxKW = quasiKeywords{
	unquote:              "unsyntax",
	splicing:             "unsyntax-splicing",
	nesting:              "quasisyntax",
	quoting:              "syntax",
	handleDottedUnquote:  false,
	nestingAlwaysRuntime: true,
}

// buildQuasiSyntaxList creates a proper list from syntax elements, stamping
// every pair with the SAME srcCtx. That uniformity is the difference from
// syntax.SyntaxList, which stamps each pair with its own car's context: these
// pairs are synthesized, so there is no per-element source to attribute them to.
//
// A package function, not a method, and that split is the rule across this
// file: a method reads compile-time state, a package function does not.
// quasiHead is the only thing in the quasi cluster that genuinely reads any —
// it consults the sealed startup set — so everything still carrying a receiver
// does so because it reaches quasiHead, directly or through quasiForm. The
// needs-runtime predicate reaches nothing, which is why it is a pair of package
// functions and can be exercised without a compiler at all.
func buildQuasiSyntaxList(srcCtx *syntax.SourceContext, elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var q syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := range slices.Backward(elems) {
		q = syntax.NewSyntaxCons(elems[i], q, srcCtx)
	}
	return q
}

// quasiHead builds one of the list-construction heads the quasiquote and
// quasisyntax expansions synthesize — list, cons, append, list->vector — pinned
// to the startup set's binding for that name.
//
// Without the pin these are ordinary free references that resolve at the USE
// site, so a top-level (define list …) captures every quasiquote in the
// program: `(1 ,x 3) returned whatever the user's list returned. The reference
// is the expander's own, not the user's — the user wrote no `list` — so it must
// carry the meaning it had where quasiquote was defined, which for a Go-side
// form is the sealed startup set.
//
// nil pin means "no sealed binding of that name here" and leaves the reference
// unpinned, which is the pre-existing behaviour: this can only remove a hijack,
// never introduce one. The pin is consulted by tryResolvedBinding, which runs
// AFTER local scope-set resolution, so it does not change the already-correct
// local-shadow answers (a let/lambda binder carries {lambdaScope} while this
// reference carries the quasiquote form's own scopes).
//
// The synthesized `quote` head is deliberately NOT routed through here: it is a
// special form, resolves as one, and is not hijackable — the negative control.
func (p *CompileTimeContinuation) quasiHead(name string, srcCtx *syntax.SourceContext) syntax.SyntaxValue {
	sym := syntax.NewSyntaxSymbol(name, srcCtx)
	if p.env == nil {
		return sym
	}
	ns := p.env.Namespace()
	if ns == nil {
		return sym
	}
	// The env's OWN phase, not PhaseRuntime: a quasiquote inside a transformer
	// body expands and runs at phase 1, and its list must be phase 1's.
	gi := ns.Store().SealedGlobalIndexAt(values.NewSymbol(name), values.EmptyScopes(), p.env.PhaseLevel())
	if gi == nil {
		return sym
	}
	return sym.WithResolvedBinding(gi)
}

// getSymbolName returns the symbol name if the value is a symbol.
//
// It compares SPELLINGS, and every caller here is right to: these walks run on
// an unexpanded template datum before any scope-set resolution, and R7RS §4.2.6
// defines quasiquote's recognition of unquote on the datum. This is not the
// binding-identity rule's territory; validate/opaque_subtree.go documents the
// same limitation for the same reason.
func getSymbolName(v syntax.SyntaxValue) (string, bool) {
	s, ok := v.(*syntax.SyntaxSymbol)
	if ok {
		return s.Key(), true
	}
	return "", false
}

// quasiQuoted renders v as the dialect's quoting form — (quote v) under
// quasiquote, (syntax v) under quasisyntax. It is the terminal case of the
// whole expansion: everything that is not evaluated is quoted.
//
// Deliberately NOT routed through quasiForm: quote and syntax are special
// forms, resolve as such, and are not hijackable. They are the negative control
// for the head pin quasiHead describes — and the reason this is a package
// function while quasiForm, three lines below it, is a method. The two differ in
// exactly one thing: whether the head they synthesize needs pinning.
func quasiQuoted(kw quasiKeywords, v syntax.SyntaxValue, srcCtx *syntax.SourceContext) syntax.SyntaxValue {
	return buildQuasiSyntaxList(srcCtx, syntax.NewSyntaxSymbol(kw.quoting, srcCtx), v)
}

// quasiForm builds one synthesized call — (list …), (cons …), (append …),
// (list->vector …) — with its operator pinned by quasiHead. Every synthesized
// call goes through here, which makes the pin a structural property of the
// expansion rather than a discipline remembered at each construction site.
func (p *CompileTimeContinuation) quasiForm(srcCtx *syntax.SourceContext, head string, args ...syntax.SyntaxValue) syntax.SyntaxValue {
	return buildQuasiSyntaxList(srcCtx, append([]syntax.SyntaxValue{p.quasiHead(head, srcCtx)}, args...)...)
}

// consChain folds elems onto tail right to left, yielding
// (cons e0 (cons e1 … tail)). No elements yields tail untouched, which is what
// makes the degenerate improper list `(. ,x) render as just x.
//
// tail is a parameter rather than something this derives, because its two
// callers mean different things by it: the dotted-unquote arm passes the RAW
// tail expression, evaluated at run time, while the improper-list arm passes an
// already-expanded datum.
func (p *CompileTimeContinuation) consChain(srcCtx *syntax.SourceContext, elems []syntax.SyntaxValue, tail syntax.SyntaxValue) syntax.SyntaxValue {
	q := tail
	for i := range slices.Backward(elems) {
		q = p.quasiForm(srcCtx, "cons", elems[i], q)
	}
	return q
}

// rewrapQuasiForm re-emits (keyword arg) as a form that RECONSTRUCTS itself at
// run time — (list '<keyword> <arg expanded at newDepth>) — which is what an
// unquote too deep to fire, or a nested quasiquote, has to become. A malformed
// form (anything but exactly one argument) is quoted verbatim instead; R7RS
// gives no error here, and `(quasiquote (unquote)) evaluates to (unquote).
//
// keyword is the name the caller's switch already matched, not a re-spelled
// kw field, so a keyword cannot be paired with another keyword's depth delta.
//
// It does not enter the depth guard: the recursive expandQuasi call does.
func (p *CompileTimeContinuation) rewrapQuasiForm(
	ctx context.Context, v *syntax.SyntaxPair, keyword string, newDepth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := v.SourceContext()
	if !hasSyntaxArity(v, 2) {
		return quasiQuoted(kw, v, srcCtx), nil
	}
	cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
	arg, err := p.expandQuasi(ctx, cdr.SyntaxCar(), newDepth, kw, g)
	if err != nil {
		return nil, err
	}
	return buildQuasiSyntaxList(srcCtx,
		p.quasiHead("list", srcCtx),
		quasiQuoted(kw, syntax.NewSyntaxSymbol(keyword, srcCtx), srcCtx),
		arg,
	), nil
}

// quasiNeedsRuntime reports whether a template contains anything that has to be
// built at run time rather than emitted as a compile-time literal — an unquote
// or unsyntax that reaches depth 1. It is the shadow of expandQuasi and must
// agree with it everywhere: a false answer does not merely forgo an
// optimization, it decides that the expander never runs, so an unquote the
// predicate cannot see simply stops happening and its keyword survives into the
// output datum.
//
//   - unquote / splicing at depth 1 -> yes
//   - unquote / splicing deeper     -> ask the argument at depth-1
//   - nesting                       -> ask the form at depth+1
//
// It shares its keyword table with the expander rather than hardcoding
// spellings, because the two spent a release out of step: the quasisyntax copy
// never received the improper-tail fix its quasiquote twin carries, and grew a
// vector blind spot of its own. Both are corrected in the shape below.
//
// Bound the same way the expander is bounded: when the input is too deep to
// walk safely, report "needs runtime" so the depth-guarded expander converts
// the over-depth into a catchable error rather than letting this overflow the
// Go stack first.
func quasiNeedsRuntime(stx syntax.SyntaxValue, depth int, kw quasiKeywords, g *expandDepthGuard) bool {
	if g.enter() {
		g.leave()
		return true
	}
	defer g.leave()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		// No empty-list guard: (*SyntaxPair).IsEmptyList is an unconditional
		// false, so this arm never holds one.
		carSymName, ok := getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case kw.unquote, kw.splicing:
				if depth == 1 {
					return true
				}
				// ,,x at depth 2: the inner unquote is at depth 1 and fires.
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					return quasiNeedsRuntime(cdr.SyntaxCar(), depth-1, kw, g)
				}
				return false
			case kw.nesting:
				if kw.nestingAlwaysRuntime && depth == 1 {
					return true
				}
				return quasiNeedsRuntimeList(v, depth+1, kw, g)
			}
		}
		return quasiNeedsRuntimeList(v, depth, kw, g)

	case *syntax.SyntaxVector:
		for _, elem := range v.Values {
			if quasiNeedsRuntime(elem, depth, kw, g) {
				return true
			}
		}
		return false

	default:
		return false
	}
}

// quasiNeedsRuntimeList walks a template list for quasiNeedsRuntime.
//
// syntax.Spine rather than SyntaxForEach, and the dotted-unquote test is why:
// `(a . ,x) parses as (a unquote x), so the shape to recognize is a bare
// `unquote` in the SPINE followed by exactly one element (R7RS §4.2.8) — a
// property of the CELL, decided by looking at its cdr while standing on the
// unquote. A car-yielding walk cannot reach that cdr, and restating the test
// over cars would replace a local pattern match with a stateful post-condition.
// validate's forEachRawSymbolPair recognizes the same shape for the same reason
// and takes the cell too (dottedUnquoteTail, opaque_subtree.go).
//
// The second reason is `return true`: this is a predicate that stops at its
// first hit, which a range-over-func does with an ordinary return and a ForEach
// consumer can only do by signalling through the error channel.
func quasiNeedsRuntimeList(pair *syntax.SyntaxPair, depth int, kw quasiKeywords, g *expandDepthGuard) bool {
	var end syntax.SpineEnd
	for cell, e := range syntax.Spine(pair) {
		end = e
		car := cell.SyntaxCar()

		// Gated on handleDottedUnquote, not on kw.unquote alone. quasisyntax
		// does not implement the dotted form, so answering "needs runtime"
		// here would move #`(1 . #,x) off the literal path and onto
		// (datum->syntax #f …) without changing its datum — a silent deopt no
		// value assertion could see.
		if kw.handleDottedUnquote {
			carSymName, ok := getSymbolName(car)
			if ok && carSymName == kw.unquote && depth == 1 {
				cdrPair, ok := cell.SyntaxCdr().(*syntax.SyntaxPair)
				if ok && hasSyntaxArity(cdrPair, 1) {
					return true
				}
			}
		}

		if quasiNeedsRuntime(car, depth, kw, g) {
			return true
		}
	}
	// An improper tail that is not a pair — a vector, most usefully. It is still
	// part of the template and can carry an unquote of its own, so ASK it rather
	// than assume it is inert. Treating it as a terminator (which is what both
	// hand-rolled walks did) reported "no runtime needed" for `(1 . #(,x)) and
	// emitted the whole form as a literal, so it printed (1 . #((unquote x))).
	// Improper() is false for a proper list and for a walk that never reached a
	// terminator, so no separate "did the loop finish?" guard is needed.
	if end.Improper() {
		return quasiNeedsRuntime(end.Tail, depth, kw, g)
	}
	return false
}

// quasiSpine says which container the elements a list expansion is walking came
// from. It is NOT a restatement of kw.handleDottedUnquote: the dotted test needs
// BOTH the dialect to implement R7RS §4.2.8 and the container to admit a dotted
// tail, and a vector admits none however it was written.
//
// It is a parameter rather than a cleared kw field because clearing the field on
// the by-value copy is measurably wrong, twice over. expandQuasiList forwards
// the same kw to expandQuasi for every SUBLIST, so the suppression leaks into
// every list nested under a vector — `#((a . ,x)) renders #((a unquote x))
// instead of #((a . 5)). And it is separately needed for the vector's OWN
// elements: with the test left on, `#(,y a unquote x) stops compiling, because
// the dotted arm fires on the synthesized spine and a raw `unquote` symbol
// reaches the compiler.
//
// Both were mutation-tested. Each fails exactly one row of
// TestQuasiExpandShape (nested-list-under-vector, vector-spine-bare-unquote)
// and leaves every other package green, which is why the two rows exist.
type quasiSpine bool

const (
	spineFromVector quasiSpine = false
	spineFromPair   quasiSpine = true
)

// expandQuasi transforms quasiquoted/quasisyntax syntax into equivalent Scheme code.
// The kw parameter selects which keywords to match (unquote vs unsyntax, etc.).
//
// At depth=1, unquotes are evaluated. At depth>1, they produce literal unquote forms.
func (p *CompileTimeContinuation) expandQuasi(
	ctx context.Context, stx syntax.SyntaxValue, depth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := stx.SourceContext()

	// Context cancellation takes precedence over the depth bound (mirrors
	// ExpandExpression). The guard then bounds Go-stack recursion: deeply nested
	// quasiquote from macro/datum->syntax output would otherwise overflow.
	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	default:
	}
	exceeded := g.enter()
	defer g.leave()
	if exceeded {
		return nil, wrapSourcedError(srcCtx, werr.WrapForeignErrorf(werr.ErrExpandDepthExceeded,
			"quasiquote: nesting depth exceeds maximum of %d", g.max))
	}

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		// No empty-list guard: (*SyntaxPair).IsEmptyList is an unconditional
		// false, so this arm never holds one. () reads as SyntaxEmptyList,
		// which is not a *SyntaxPair and lands in the default arm.
		carSymName, ok := getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case kw.unquote:
				// The escape. Depth 1 is where an unquote fires, and it yields
				// the RAW argument: `,x is x, not an expansion of x.
				if depth == 1 && hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					return cdr.SyntaxCar(), nil
				}
				return p.rewrapQuasiForm(ctx, v, carSymName, depth-1, kw, g)

			case kw.splicing:
				// A splice reached HERE is one no list walk claimed, so there
				// is nothing to splice into and depth 1 does not fire: a bare
				// `,@(list 1 2) evaluates to (unquote-splicing (list 1 2)).
				if depth <= 1 {
					return quasiQuoted(kw, v, srcCtx), nil
				}
				return p.rewrapQuasiForm(ctx, v, carSymName, depth-1, kw, g)

			case kw.nesting:
				return p.rewrapQuasiForm(ctx, v, carSymName, depth+1, kw, g)
			}
		}

		// Regular list - delegate to list expansion
		return p.expandQuasiList(ctx, v, depth, kw, spineFromPair, g)

	case *syntax.SyntaxSymbol:
		return quasiQuoted(kw, v, srcCtx), nil

	case *syntax.SyntaxVector:
		return p.expandQuasiVector(ctx, v, depth, kw, g)

	default:
		return quasiQuoted(kw, stx, srcCtx), nil
	}
}

// expandQuasiList expands a list template in ONE walk, which accumulates two
// things: a run of ordinary expanded elements, and — once a splice has been
// seen — the argument list of an (append …). The ending picks the shape from
// what the walk found:
//
//	no splice, proper spine    -> (list e …)
//	no splice, tail            -> (cons e … tail)
//	splice                     -> (append (list e …) spliced … tail?)
//
// One walk suffices even though a splice changes how the elements BEFORE it are
// rendered, because runs are only closed when a splice is met: with no splice
// anywhere, every element is still sitting in one run at the end. The only
// early exit is the dotted-unquote arm, and that arm requires the cell's cdr to
// be a one-element list, so exactly one element remains and no splice can hide
// behind it — except as that final tail expression itself, which reaches the
// compiler raw either way and errors identically ("unquote-splicing: not in
// quasiquote context", at the same token).
//
// Two facts about the ordering here are load-bearing and neither is local:
//
//   - sawSplice is set BEFORE the arity test, matching the prescan this
//     replaced, which tested the car symbol alone. Setting it after would make
//     "a splice was seen" and "a splice argument was recorded" the same
//     predicate, and `(a b (unquote-splicing) c) would render as
//     ((unquote-splicing) c) — a and b silently dropped, because flushRun has
//     already moved them into appendArgs while the ending reads only elems.
//   - the tail is expanded BEFORE the final flushRun, matching the order the
//     two-walk version expanded it in (at the bottom of its last iteration).
func (p *CompileTimeContinuation) expandQuasiList(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords, spine quasiSpine, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := pair.SourceContext()

	var elems []syntax.SyntaxValue      // the run in progress
	var appendArgs []syntax.SyntaxValue // read only when sawSplice
	var improperTail syntax.SyntaxValue
	sawSplice := false

	flushRun := func() {
		if len(elems) > 0 {
			appendArgs = append(appendArgs, p.quasiForm(srcCtx, "list", elems...))
			elems = nil
		}
	}

	var end syntax.SpineEnd
	for cell, e := range syntax.Spine(pair) {
		// Assigned at the top so the dotted-unquote break below cannot leave
		// end holding the PREVIOUS cell's terminator.
		end = e
		car := cell.SyntaxCar()

		// Dotted-pair unquote (quasiquote only). `(a . ,x) READS as the proper
		// list (a unquote x), so a dotted tail never arrives as a non-pair cdr —
		// it arrives as the bare symbol `unquote` sitting on the spine, followed
		// by exactly one element. The improper-tail branch below therefore never
		// fires for it, and without this check the walk took `unquote` and `x`
		// in as ordinary elements: `(,@x . ,y) rendered as the 4-element list
		// (1 2 unquote y). R7RS §4.2.8.
		//
		// This is a property of the CELL — it reads the cdr while standing on
		// the unquote — which is why the walk yields cells rather than cars.
		//
		// Two independent facts gate it: the container admits a dotted tail,
		// and the dialect implements the form.
		if spine == spineFromPair && kw.handleDottedUnquote {
			carSymName, ok := getSymbolName(car)
			if ok && carSymName == kw.unquote && depth == 1 {
				cdrPair, ok := cell.SyntaxCdr().(*syntax.SyntaxPair)
				if ok && hasSyntaxArity(cdrPair, 1) {
					// Raw, not expanded: the tail expression is evaluated at run
					// time, exactly as a splice's expression is.
					improperTail = cdrPair.SyntaxCar()
					break
				}
			}
		}

		carPair, ok := car.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := getSymbolName(carPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				sawSplice = true
				flushRun()
				if hasSyntaxArity(carPair, 2) {
					cdrPair := carPair.SyntaxCdr().(*syntax.SyntaxPair)
					appendArgs = append(appendArgs, cdrPair.SyntaxCar())
					continue
				}
				// Malformed: taken as an ordinary element, but the run before it
				// is already closed, so it starts a new one.
				expandedCar, err := p.expandQuasi(ctx, car, depth, kw, g)
				if err != nil {
					return nil, err
				}
				elems = append(elems, expandedCar)
				continue
			}
		}

		expandedCar, err := p.expandQuasi(ctx, car, depth, kw, g)
		if err != nil {
			return nil, err
		}
		elems = append(elems, expandedCar)
	}

	// An improper tail is part of the template and can carry an unquote of its
	// own, so it is expanded rather than assumed inert. Improper() is false both
	// for a proper spine and after the dotted-unquote break (whose cell's cdr is
	// a pair by construction), so improperTail's raw value survives that path.
	if end.Improper() {
		expandedTail, err := p.expandQuasi(ctx, end.Tail, depth, kw, g)
		if err != nil {
			return nil, err
		}
		improperTail = expandedTail
	}

	if sawSplice {
		flushRun()
		// R7RS append with a non-list final argument produces the improper list.
		if improperTail != nil {
			appendArgs = append(appendArgs, improperTail)
		}
		return p.quasiForm(srcCtx, "append", appendArgs...), nil
	}
	if improperTail != nil {
		return p.consChain(srcCtx, elems, improperTail), nil
	}
	return p.quasiForm(srcCtx, "list", elems...), nil
}

// expandQuasiVector expands a vector template. A vector IS list->vector over the
// list expansion of its elements — same runs, same splices, same (list …) /
// (append …) choice — so it re-uses expandQuasiList over a synthesized spine
// rather than restating the walk. What it does not share is the dotted-tail
// test, and quasiSpine is what withholds it.
//
// The elements are relinked into a proper list stamped with the vector's own
// source context, which is what expandQuasiList would have read off the
// original pair anyway.
func (p *CompileTimeContinuation) expandQuasiVector(
	ctx context.Context, v *syntax.SyntaxVector, depth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := v.SourceContext()

	// buildQuasiSyntaxList of no elements is SyntaxEmptyList, not a *SyntaxPair,
	// so an empty vector never enters the list expander.
	spine, ok := buildQuasiSyntaxList(srcCtx, v.Values...).(*syntax.SyntaxPair)
	if !ok {
		return p.quasiForm(srcCtx, "list->vector", p.quasiForm(srcCtx, "list")), nil
	}
	elems, err := p.expandQuasiList(ctx, spine, depth, kw, spineFromVector, g)
	if err != nil {
		return nil, err
	}
	return p.quasiForm(srcCtx, "list->vector", elems), nil
}
