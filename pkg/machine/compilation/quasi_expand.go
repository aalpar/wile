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
			quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
			return p.buildQuasiSyntaxList(srcCtx, quoteSym, v), nil
		}

		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case kw.unquote:
				if depth == 1 {
					if hasSyntaxArity(v, 2) {
						cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
						return cdr.SyntaxCar(), nil
					}
				}
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg, err := p.expandQuasi(ctx, arg, depth-1, kw, g)
					if err != nil {
						return nil, err
					}
					return p.buildQuasiSyntaxList(srcCtx,
						p.quasiHead("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.unquote, srcCtx),
						),
						processedArg,
					), nil
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v), nil

			case kw.splicing:
				if depth > 1 && hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg, err := p.expandQuasi(ctx, arg, depth-1, kw, g)
					if err != nil {
						return nil, err
					}
					return p.buildQuasiSyntaxList(srcCtx,
						p.quasiHead("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.splicing, srcCtx),
						),
						processedArg,
					), nil
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v), nil

			case kw.nesting:
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					processedBody, err := p.expandQuasi(ctx, body, depth+1, kw, g)
					if err != nil {
						return nil, err
					}
					return p.buildQuasiSyntaxList(srcCtx,
						p.quasiHead("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.nesting, srcCtx),
						),
						processedBody,
					), nil
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v), nil
			}
		}

		// Regular list - delegate to list expansion
		return p.expandQuasiList(ctx, v, depth, kw, g)

	case *syntax.SyntaxSymbol:
		quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
		return p.buildQuasiSyntaxList(srcCtx, quoteSym, v), nil

	case *syntax.SyntaxVector:
		return p.expandQuasiquoteVector(ctx, v, depth, kw, g)

	default:
		quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
		return p.buildQuasiSyntaxList(srcCtx, quoteSym, stx), nil
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
	elems = append(elems, p.quasiHead("list", srcCtx))

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
					tailExpr := cdrPair.SyntaxCar()
					var result syntax.SyntaxValue
					result = tailExpr
					for i := len(elems) - 1; i >= 1; i-- {
						result = p.buildQuasiSyntaxList(srcCtx,
							p.quasiHead("cons", srcCtx),
							elems[i],
							result,
						)
					}
					return result, nil
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

		// Improper list: build nested cons from already-expanded elements + expanded tail.
		// elems[0] is the "list" symbol; elems[1:] are the already-expanded elements.
		expandedTail, err := p.expandQuasi(ctx, cdr, depth, kw, g)
		if err != nil {
			return nil, err
		}
		result := expandedTail
		for i := len(elems) - 1; i >= 1; i-- {
			result = p.buildQuasiSyntaxList(srcCtx,
				p.quasiHead("cons", srcCtx),
				elems[i],
				result,
			)
		}
		return result, nil
	}

	return p.buildQuasiSyntaxList(srcCtx, elems...), nil
}

// quasiSegmentKind distinguishes a run of literal elements from a single
// spliced expression when expanding a quasiquoted list or vector that contains
// unquote-splicing / unsyntax-splicing.
type quasiSegmentKind int

const (
	quasiSegNormal quasiSegmentKind = iota
	quasiSegSplice
)

// quasiSegment is one run of a spliced quasiquote form: either a sequence of
// literal elements (quasiSegNormal) or a single spliced expression
// (quasiSegSplice). The vector and list expanders accumulate these and render
// them via segmentsToAppendArgs.
type quasiSegment struct {
	kind  quasiSegmentKind
	elems []syntax.SyntaxValue // for quasiSegNormal
	expr  syntax.SyntaxValue   // for quasiSegSplice
}

// segmentsToAppendArgs renders accumulated splice segments into the argument
// list of an (append ...) form: each normal run becomes (list e ...), each
// splice contributes its expression directly. The returned slice begins with
// the `append` symbol. Shared by the list and vector quasiquote expanders.
func (p *CompileTimeContinuation) segmentsToAppendArgs(srcCtx *syntax.SourceContext, segments []quasiSegment) []syntax.SyntaxValue {
	appendArgs := []syntax.SyntaxValue{p.quasiHead("append", srcCtx)}
	for _, seg := range segments {
		switch seg.kind {
		case quasiSegNormal:
			listArgs := []syntax.SyntaxValue{p.quasiHead("list", srcCtx)}
			listArgs = append(listArgs, seg.elems...)
			appendArgs = append(appendArgs, p.buildQuasiSyntaxList(srcCtx, listArgs...))
		case quasiSegSplice:
			appendArgs = append(appendArgs, seg.expr)
		}
	}
	return appendArgs
}

// expandQuasiListWithSplice handles lists containing splicing (unquote-splicing
// or unsyntax-splicing). It segments the list into normal and splice segments,
// then builds (append seg1 seg2 ...).
func (p *CompileTimeContinuation) expandQuasiListWithSplice(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords, g *expandDepthGuard,
) (syntax.SyntaxValue, error) {
	srcCtx := pair.SourceContext()

	var segments []quasiSegment
	var currentElems []syntax.SyntaxValue
	var improperTail syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			segments = append(segments, quasiSegment{kind: quasiSegNormal, elems: currentElems})
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
					expr := cdrPair.SyntaxCar()
					segments = append(segments, quasiSegment{kind: quasiSegSplice, expr: expr})
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

	appendArgs := p.segmentsToAppendArgs(srcCtx, segments)
	if improperTail != nil {
		appendArgs = append(appendArgs, improperTail)
	}

	return p.buildQuasiSyntaxList(srcCtx, appendArgs...), nil
}
