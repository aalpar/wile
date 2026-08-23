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
	"errors"
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
}

var quasiquoteKW = quasiKeywords{
	unquote:             "unquote",
	splicing:            "unquote-splicing",
	nesting:             "quasiquote",
	quoting:             "quote",
	handleDottedUnquote: true,
}

var quasisyntaxKW = quasiKeywords{
	unquote:             "unsyntax",
	splicing:            "unsyntax-splicing",
	nesting:             "quasisyntax",
	quoting:             "syntax",
	handleDottedUnquote: false,
}

// buildQuasiSyntaxList creates a proper list from syntax elements.
func (p *CompileTimeContinuation) buildQuasiSyntaxList(srcCtx *syntax.SourceContext, elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var result syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := range slices.Backward(elems) {
		result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
	}
	return result
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

// getSymbolName returns the symbol name if the value is a symbol
func (p *CompileTimeContinuation) getSymbolName(v syntax.SyntaxValue) (string, bool) {
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
// for the head pin quasiHead describes.
func (p *CompileTimeContinuation) quasiQuoted(kw quasiKeywords, v syntax.SyntaxValue, srcCtx *syntax.SourceContext) syntax.SyntaxValue {
	return p.buildQuasiSyntaxList(srcCtx, syntax.NewSyntaxSymbol(kw.quoting, srcCtx), v)
}

// quasiForm builds one synthesized call — (list …), (cons …), (append …),
// (list->vector …) — with its operator pinned by quasiHead. Every synthesized
// call goes through here, which makes the pin a structural property of the
// expansion rather than a discipline remembered at each construction site.
func (p *CompileTimeContinuation) quasiForm(srcCtx *syntax.SourceContext, head string, args ...syntax.SyntaxValue) syntax.SyntaxValue {
	return p.buildQuasiSyntaxList(srcCtx, append([]syntax.SyntaxValue{p.quasiHead(head, srcCtx)}, args...)...)
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
		return p.quasiQuoted(kw, v, srcCtx), nil
	}
	cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
	arg, err := p.expandQuasi(ctx, cdr.SyntaxCar(), newDepth, kw, g)
	if err != nil {
		return nil, err
	}
	return p.buildQuasiSyntaxList(srcCtx,
		p.quasiHead("list", srcCtx),
		p.quasiQuoted(kw, syntax.NewSyntaxSymbol(keyword, srcCtx), srcCtx),
		arg,
	), nil
}

// expandQuasi transforms quasiquoted/quasisyntax syntax into equivalent Scheme code.
// The kw parameter selects which keywords to match (unquote vs unsyntax, etc.).
//
// At depth=1, unquotes are evaluated. At depth>1, they produce literal unquote forms.
// Vectors are handled here too, by delegating to expandQuasiquoteVector; the
// quasiquote entry point pre-dispatches them as well, so both paths agree.
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
		if syntax.IsSyntaxEmptyList(v) {
			return p.quasiQuoted(kw, v, srcCtx), nil
		}

		carSymName, ok := p.getSymbolName(v.SyntaxCar())
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
					return p.quasiQuoted(kw, v, srcCtx), nil
				}
				return p.rewrapQuasiForm(ctx, v, carSymName, depth-1, kw, g)

			case kw.nesting:
				return p.rewrapQuasiForm(ctx, v, carSymName, depth+1, kw, g)
			}
		}

		// Regular list - delegate to list expansion
		return p.expandQuasiList(ctx, v, depth, kw, g)

	case *syntax.SyntaxSymbol:
		return p.quasiQuoted(kw, v, srcCtx), nil

	case *syntax.SyntaxVector:
		return p.expandQuasiquoteVector(ctx, v, depth, kw, g)

	default:
		return p.quasiQuoted(kw, stx, srcCtx), nil
	}
}

// expandQuasiList handles list expansion for both quasiquote and quasisyntax.
// It detects splicing, dotted-pair unquote (quasiquote only), and improper lists.
func (p *CompileTimeContinuation) expandQuasiList(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := pair.SourceContext()

	// Scan for splicing at depth 1
	hasSplice := false
	_, err := pair.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, carSyntax syntax.SyntaxValue) error {
		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				hasSplice = true
				return werr.ErrStopIteration
			}
		}
		return nil
	})
	if err != nil && !errors.Is(err, werr.ErrStopIteration) {
		return nil, wrapSourcedError(srcCtx, werr.WrapForeignErrorf(err, "quasi: error scanning list"))
	}

	if hasSplice {
		return p.expandQuasiListWithSplice(ctx, pair, depth, kw, g)
	}

	// No splice path: build (list elem1 elem2 ...)
	var elems []syntax.SyntaxValue

	current := pair
	for !values.IsEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car

		// Detect dotted-pair unquote (quasiquote only): `(a . ,x)` parses as
		// `(a unquote x)`. When we see the bare symbol at depth 1 followed by
		// exactly one more element, treat the remaining (unquote expr) as the
		// tail expression per R7RS §4.2.8.
		if kw.handleDottedUnquote {
			carSymName, ok := p.getSymbolName(carSyntax)
			if ok && carSymName == kw.unquote && depth == 1 {
				cdr := current.SyntaxCdr()
				cdrPair, ok := cdr.(*syntax.SyntaxPair)
				if ok && hasSyntaxArity(cdrPair, 1) {
					// Raw, not expanded: the tail expression is evaluated at
					// run time.
					return p.consChain(srcCtx, elems, cdrPair.SyntaxCar()), nil
				}
			}
		}

		expandedCar, err := p.expandQuasi(ctx, carSyntax, depth, kw, g)
		if err != nil {
			return nil, err
		}
		elems = append(elems, expandedCar)
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
			continue
		}

		// Improper list: cons the already-expanded elements onto the expanded
		// tail. The tail is part of the template and can carry an unquote of its
		// own, so it is expanded rather than assumed inert.
		expandedTail, err := p.expandQuasi(ctx, cdr, depth, kw, g)
		if err != nil {
			return nil, err
		}
		return p.consChain(srcCtx, elems, expandedTail), nil
	}

	return p.quasiForm(srcCtx, "list", elems...), nil
}

// expandQuasiListWithSplice handles lists containing splicing (unquote-splicing
// or unsyntax-splicing). It accumulates the arguments of an (append …): each run
// of ordinary elements becomes one (list e …), each splice contributes its
// expression directly.
func (p *CompileTimeContinuation) expandQuasiListWithSplice(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := pair.SourceContext()

	var appendArgs []syntax.SyntaxValue
	var currentElems []syntax.SyntaxValue
	var improperTail syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			appendArgs = append(appendArgs, p.quasiForm(srcCtx, "list", currentElems...))
			currentElems = nil
		}
	}

	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car

		// Dotted-pair unquote, mirroring expandQuasiList's arm. `(a . ,x)` READS as
		// the proper list `(a unquote x)`, so a dotted tail never arrives as a
		// non-pair cdr — it arrives as the bare symbol `unquote` sitting on the
		// spine, followed by exactly one element. The improper-tail branch further
		// down therefore never fires for it, and without this check the splice path
		// walked `unquote` and `x` in as ordinary elements: `(,@x . ,y) rendered as
		// the 4-element list (1 2 unquote y). R7RS §4.2.8.
		if kw.handleDottedUnquote {
			carSymName, ok := p.getSymbolName(carSyntax)
			if ok && carSymName == kw.unquote && depth == 1 {
				cdr := current.SyntaxCdr()
				cdrPair, ok := cdr.(*syntax.SyntaxPair)
				if ok && hasSyntaxArity(cdrPair, 1) {
					flushNormal()
					// Raw, not expanded: the tail expression is evaluated at
					// runtime, exactly as a splice segment's expr is. append with
					// a non-list final argument yields the improper list.
					improperTail = cdrPair.SyntaxCar()
					break
				}
			}
		}

		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				flushNormal()
				if !hasSyntaxArity(carPair, 2) {
					// Malformed - treat as normal
					expandedCar, err := p.expandQuasi(ctx, carSyntax, depth, kw, g)
					if err != nil {
						return nil, err
					}
					currentElems = append(currentElems, expandedCar)
				} else {
					cdrPair := carPair.SyntaxCdr().(*syntax.SyntaxPair)
					appendArgs = append(appendArgs, cdrPair.SyntaxCar())
				}
				goto next
			}
		}

		// Block-scoped so the declarations are not in scope at `next:` (a goto
		// may not jump over an in-scope variable declaration).
		{
			expandedCar, err := p.expandQuasi(ctx, carSyntax, depth, kw, g)
			if err != nil {
				return nil, err
			}
			currentElems = append(currentElems, expandedCar)
		}

	next:
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			// Improper list: expand the dotted tail and preserve it
			// as the final append argument. R7RS append with a non-list
			// final argument produces an improper list.
			var err error
			improperTail, err = p.expandQuasi(ctx, cdr, depth, kw, g)
			if err != nil {
				return nil, err
			}
			break
		}
	}

	flushNormal()

	if improperTail != nil {
		appendArgs = append(appendArgs, improperTail)
	}

	return p.quasiForm(srcCtx, "append", appendArgs...), nil
}
