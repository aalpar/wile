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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
// Vector handling is NOT done here — the quasiquote caller handles vectors separately.
func (p *CompileTimeContinuation) expandQuasi(
	ctx context.Context, stx syntax.SyntaxValue, depth int, kw quasiKeywords,
) syntax.SyntaxValue {
	srcCtx := stx.SourceContext()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
			return p.buildQuasiSyntaxList(srcCtx, quoteSym, v)
		}

		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case kw.unquote:
				if depth == 1 {
					if v.Length() == 2 {
						cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
						return cdr.SyntaxCar()
					}
				}
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasi(ctx, arg, depth-1, kw)
					return p.buildQuasiSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.unquote, srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v)

			case kw.splicing:
				if depth > 1 && v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasi(ctx, arg, depth-1, kw)
					return p.buildQuasiSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.splicing, srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v)

			case kw.nesting:
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					processedBody := p.expandQuasi(ctx, body, depth+1, kw)
					return p.buildQuasiSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol(kw.quoting, srcCtx),
							syntax.NewSyntaxSymbol(kw.nesting, srcCtx),
						),
						processedBody,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
				return p.buildQuasiSyntaxList(srcCtx, quoteSym, v)
			}
		}

		// Regular list - delegate to list expansion
		return p.expandQuasiList(ctx, v, depth, kw)

	case *syntax.SyntaxSymbol:
		quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
		return p.buildQuasiSyntaxList(srcCtx, quoteSym, v)

	case *syntax.SyntaxVector:
		return p.expandQuasiquoteVector(ctx, v, depth, kw)

	default:
		quoteSym := syntax.NewSyntaxSymbol(kw.quoting, srcCtx)
		return p.buildQuasiSyntaxList(srcCtx, quoteSym, stx)
	}
}

// expandQuasiList handles list expansion for both quasiquote and quasisyntax.
// It detects splicing, dotted-pair unquote (quasiquote only), and improper lists.
func (p *CompileTimeContinuation) expandQuasiList(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords,
) syntax.SyntaxValue {
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
		panic(werr.WrapForeignErrorf(err, "quasi: error scanning list at %s", srcCtx.SchemeString()))
	}

	if hasSplice {
		return p.expandQuasiListWithSplice(ctx, pair, depth, kw)
	}

	// No splice path: build (list elem1 elem2 ...)
	var elems []syntax.SyntaxValue
	elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))

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
				if ok && cdrPair.Length() == 1 {
					tailExpr := cdrPair.SyntaxCar()
					var result syntax.SyntaxValue
					result = tailExpr
					for i := len(elems) - 1; i >= 1; i-- {
						result = p.buildQuasiSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("cons", srcCtx),
							elems[i],
							result,
						)
					}
					return result
				}
			}
		}

		elems = append(elems, p.expandQuasi(ctx, carSyntax, depth, kw))
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
		expandedTail := p.expandQuasi(ctx, cdr, depth, kw)
		result := expandedTail
		for i := len(elems) - 1; i >= 1; i-- {
			result = p.buildQuasiSyntaxList(srcCtx,
				syntax.NewSyntaxSymbol("cons", srcCtx),
				elems[i],
				result,
			)
		}
		return result
	}

	return p.buildQuasiSyntaxList(srcCtx, elems...)
}

// expandQuasiListWithSplice handles lists containing splicing (unquote-splicing
// or unsyntax-splicing). It segments the list into normal and splice segments,
// then builds (append seg1 seg2 ...).
func (p *CompileTimeContinuation) expandQuasiListWithSplice(
	ctx context.Context, pair *syntax.SyntaxPair, depth int, kw quasiKeywords,
) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	type segmentType int
	const (
		segNormal segmentType = iota
		segSplice
	)

	type segment struct {
		typ   segmentType
		elems []syntax.SyntaxValue // for normal segments
		expr  syntax.SyntaxValue   // for splice segments
	}

	var segments []segment
	var currentElems []syntax.SyntaxValue
	var improperTail syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			segments = append(segments, segment{typ: segNormal, elems: currentElems})
			currentElems = nil
		}
	}

	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car
		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				flushNormal()
				if carPair.Length() != 2 {
					// Malformed - treat as normal
					currentElems = append(currentElems, p.expandQuasi(ctx, carSyntax, depth, kw))
				} else {
					cdrPair := carPair.SyntaxCdr().(*syntax.SyntaxPair)
					expr := cdrPair.SyntaxCar()
					segments = append(segments, segment{typ: segSplice, expr: expr})
				}
				goto next
			}
		}

		currentElems = append(currentElems, p.expandQuasi(ctx, carSyntax, depth, kw))

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
			improperTail = p.expandQuasi(ctx, cdr, depth, kw)
			break
		}
	}

	flushNormal()

	// Build (append seg1 seg2 ...)
	var appendArgs []syntax.SyntaxValue
	appendArgs = append(appendArgs, syntax.NewSyntaxSymbol("append", srcCtx))

	for _, seg := range segments {
		switch seg.typ {
		case segNormal:
			listArgs := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
			listArgs = append(listArgs, seg.elems...)
			appendArgs = append(appendArgs, p.buildQuasiSyntaxList(srcCtx, listArgs...))
		case segSplice:
			appendArgs = append(appendArgs, seg.expr)
		}
	}

	if improperTail != nil {
		appendArgs = append(appendArgs, improperTail)
	}

	return p.buildQuasiSyntaxList(srcCtx, appendArgs...)
}
