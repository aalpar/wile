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

package machine

import (
	"context"
	"errors"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// compileQuasiquoteDatum compiles a quasiquoted datum at the given nesting depth.
//
// depth=1 means we're inside one level of quasiquote (the common case).
// depth=2 means nested quasiquote `(a `(b ,x)), etc.
// depth=0 would mean we should evaluate (but we start at 1, so this is the trigger).
func (p *CompileTimeContinuation) compileQuasiquoteDatum(ctctx CompileTimeCallContext, datum syntax.SyntaxValue, depth int) error {
	// Optimization: if no runtime evaluation needed, emit as literal
	if !p.quasiquoteNeedsRuntime(datum, depth) {
		// Intern symbols to ensure eq? identity per R7RS 6.5
		val, err := p.internSymbolsInValue(datum.UnwrapAll())
		if err != nil {
			return err
		}
		li := p.template.MaybeAppendLiteral(val)
		p.AppendOperations(NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Transform to equivalent Scheme code and compile
	expanded := p.expandQuasiquote(ctctx.ctx, datum, depth)
	return p.CompileExpression(ctctx, expanded)
}

// buildQuasiquoteSyntaxList creates a proper list from syntax elements.
func (p *CompileTimeContinuation) buildQuasiquoteSyntaxList(srcCtx *syntax.SourceContext, elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var result syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := len(elems) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
	}
	return result
}

// expandQuasiquote transforms quasiquoted syntax into equivalent Scheme code.
// At depth=1, unquotes are evaluated. At depth>1, they produce literal unquote forms.
//
// Key behaviors:
//   - unquote at d=1: return the expression directly (evaluate it)
//   - unquote at d>1: process arg at d-1, wrap in (list 'unquote <result>)
//   - unquote-splicing at d=1: handled specially by expandQuasiquoteList
//   - unquote-splicing at d>1: process arg at d-1, wrap in (list 'unquote-splicing <result>)
//   - quasiquote: process body at d+1, wrap in (list 'quasiquote <result>)
//   - lists: generate (list ...) or (append ...) for runtime construction
//   - atoms: quote them
func (p *CompileTimeContinuation) expandQuasiquote(ctx context.Context, stx syntax.SyntaxValue, depth int) syntax.SyntaxValue {
	srcCtx := stx.SourceContext()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
			return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)
		}

		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case "unquote":
				if depth == 1 {
					if v.Length() == 2 {
						cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
						return cdr.SyntaxCar()
					}
				}
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasiquote(ctx, arg, depth-1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("unquote", srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

			case "unquote-splicing":
				if depth > 1 && v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					processedArg := p.expandQuasiquote(ctx, arg, depth-1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("unquote-splicing", srcCtx),
						),
						processedArg,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

			case "quasiquote":
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					processedBody := p.expandQuasiquote(ctx, body, depth+1)
					return p.buildQuasiquoteSyntaxList(srcCtx,
						syntax.NewSyntaxSymbol("list", srcCtx),
						p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("quote", srcCtx),
							syntax.NewSyntaxSymbol("quasiquote", srcCtx),
						),
						processedBody,
					)
				}
				quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
				return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)
			}
		}

		// Regular list - check for unquote-splicing at depth 1
		return p.expandQuasiquoteList(ctx, v, depth)

	case *syntax.SyntaxSymbol:
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, v)

	case *syntax.SyntaxVector:
		// Vectors: expand elements and wrap in (list->vector ...)
		// Check if any element is unquote-splicing at depth 1
		hasSplice := false
		for _, elem := range v.Values {
			elemPair, ok := elem.(*syntax.SyntaxPair)
			if ok {
				carSymName, ok := p.getSymbolName(elemPair.SyntaxCar())
				if ok {
					if carSymName == "unquote-splicing" && depth == 1 {
						hasSplice = true
						break
					}
				}
			}
		}

		if !hasSplice {
			// Simple case: (list->vector (list elem1 elem2 ...))
			var elems []syntax.SyntaxValue
			elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))
			for _, elem := range v.Values {
				elems = append(elems, p.expandQuasiquote(ctx, elem, depth))
			}
			listExpr := p.buildQuasiquoteSyntaxList(srcCtx, elems...)
			return p.buildQuasiquoteSyntaxList(srcCtx,
				syntax.NewSyntaxSymbol("list->vector", srcCtx),
				listExpr,
			)
		}

		// Has splicing: (list->vector (append seg1 seg2 ...))
		// Segment the elements similar to expandQuasiquoteListWithSplice
		type segmentType int
		const (
			segNormal segmentType = iota
			segSplice
		)
		type segment struct {
			typ   segmentType
			elems []syntax.SyntaxValue
			expr  syntax.SyntaxValue
		}

		var segments []segment
		var currentElems []syntax.SyntaxValue

		flushNormal := func() {
			if len(currentElems) > 0 {
				segments = append(segments, segment{typ: segNormal, elems: currentElems})
				currentElems = nil
			}
		}

		for _, elem := range v.Values {
			elemPair, ok := elem.(*syntax.SyntaxPair)
			if ok {
				carSymName, ok := p.getSymbolName(elemPair.SyntaxCar())
				if ok {
					if carSymName == "unquote-splicing" && depth == 1 {
						flushNormal()
						if elemPair.Length() == 2 {
							cdrPair := elemPair.SyntaxCdr().(*syntax.SyntaxPair)
							expr := cdrPair.SyntaxCar()
							segments = append(segments, segment{typ: segSplice, expr: expr})
						} else {
							// Malformed - treat as normal
							currentElems = append(currentElems, p.expandQuasiquote(ctx, elem, depth))
						}
						continue
					}
				}
			}
			currentElems = append(currentElems, p.expandQuasiquote(ctx, elem, depth))
		}
		flushNormal()

		// Build (append seg1 seg2 ...)
		var appendArgs []syntax.SyntaxValue
		appendArgs = append(appendArgs, syntax.NewSyntaxSymbol("append", srcCtx))

		for _, seg := range segments {
			switch seg.typ {
			case segNormal:
				// Wrap in (list ...)
				listArgs := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
				listArgs = append(listArgs, seg.elems...)
				appendArgs = append(appendArgs, p.buildQuasiquoteSyntaxList(srcCtx, listArgs...))
			case segSplice:
				appendArgs = append(appendArgs, seg.expr)
			}
		}

		appendExpr := p.buildQuasiquoteSyntaxList(srcCtx, appendArgs...)
		return p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("list->vector", srcCtx),
			appendExpr,
		)

	default:
		quoteSym := syntax.NewSyntaxSymbol("quote", srcCtx)
		return p.buildQuasiquoteSyntaxList(srcCtx, quoteSym, stx)
	}
}

// expandQuasiquoteList handles list expansion, detecting unquote-splicing.
func (p *CompileTimeContinuation) expandQuasiquoteList(ctx context.Context, pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Check if any element is ,@ at depth 1
	hasSplice := false
	current := pair
	// Scan for unquote-splicing at the current depth. For improper lists,
	// SyntaxForEach returns the dotted tail — that's fine, the expansion
	// logic below handles improper lists via expandQuasiquoteImproperList.
	_, err := current.SyntaxForEach(ctx, func(_ context.Context, _ int, _ bool, carSyntax syntax.SyntaxValue) error {
		carPair, ok := carSyntax.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(carPair.SyntaxCar())
			if ok {
				if carSymName == "unquote-splicing" && depth == 1 {
					hasSplice = true
					return werr.ErrStopIteration
				}
			}
		}
		return nil
	})
	if err != nil && !errors.Is(err, werr.ErrStopIteration) {
		panic(werr.WrapForeignErrorf(err, "quasiquote: error scanning list at %s", srcCtx.SchemeString()))
	}
	if !hasSplice {
		// Simple case: (list elem1 elem2 ...)
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))

		current := pair
		for !values.IsEmptyList(current) {
			car := current.SyntaxCar()
			carSyntax := car

			// Detect dotted-pair unquote: `(a . ,x)` parses as `(a unquote x)`.
			// When we see the symbol `unquote` as a bare element followed by
			// exactly one more element, treat the remaining `(unquote expr)` as
			// the tail expression per R7RS §4.2.8.
			carSymName, ok := p.getSymbolName(carSyntax)
			if ok && carSymName == "unquote" && depth == 1 {
				cdr := current.SyntaxCdr()
				cdrPair, ok := cdr.(*syntax.SyntaxPair)
				if ok && cdrPair.Length() == 1 {
					// This is `(... unquote expr)` — a dotted-pair unquote.
					// Build (cons prev-elems... expr) using the collected elements so far.
					tailExpr := cdrPair.SyntaxCar()
					var result syntax.SyntaxValue
					result = tailExpr
					for i := len(elems) - 1; i >= 1; i-- {
						result = p.buildQuasiquoteSyntaxList(srcCtx,
							syntax.NewSyntaxSymbol("cons", srcCtx),
							elems[i],
							result,
						)
					}
					return result
				}
			}

			elems = append(elems, p.expandQuasiquote(ctx, carSyntax, depth))
			cdr := current.SyntaxCdr()
			if syntax.IsSyntaxEmptyList(cdr) {
				break
			}
			nextPair, ok := cdr.(*syntax.SyntaxPair)
			if ok {
				current = nextPair
			} else {
				// Improper list - handle dotted tail
				// Generate (cons elem1 (cons elem2 ... tail))
				return p.expandQuasiquoteImproperList(ctx, pair, depth)
			}
		}
		return p.buildQuasiquoteSyntaxList(srcCtx, elems...)
	}

	// Has splicing: use (append seg1 seg2 ...)
	return p.expandQuasiquoteListWithSplice(ctx, pair, depth)
}

// expandQuasiquoteImproperList handles improper (dotted) lists.
func (p *CompileTimeContinuation) expandQuasiquoteImproperList(ctx context.Context, pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Collect all elements and the tail
	var elements []syntax.SyntaxValue
	var tail syntax.SyntaxValue

	current := pair
	for {
		car := current.SyntaxCar()
		carSyntax := car
		elements = append(elements, p.expandQuasiquote(ctx, carSyntax, depth))
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			tail = syntax.SyntaxEmptyList
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
			// Found the improper tail
			tailSyntax := cdr
			tail = tailSyntax
			break
		}
	}

	// Build nested cons: (cons elem1 (cons elem2 ... tail))
	var result syntax.SyntaxValue
	tailSyntax := tail
	result = p.expandQuasiquote(ctx, tailSyntax, depth)

	for i := len(elements) - 1; i >= 0; i-- {
		result = p.buildQuasiquoteSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("cons", srcCtx),
			elements[i],
			result,
		)
	}
	return result
}

// expandQuasiquoteListWithSplice handles lists containing unquote-splicing.
func (p *CompileTimeContinuation) expandQuasiquoteListWithSplice(ctx context.Context, pair *syntax.SyntaxPair, depth int) syntax.SyntaxValue {
	srcCtx := pair.SourceContext()

	// Segment types
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
			if ok {
				if carSymName == "unquote-splicing" && depth == 1 {
					flushNormal()
					if carPair.Length() != 2 {
						// Malformed - treat as normal
						currentElems = append(currentElems, p.expandQuasiquote(ctx, carSyntax, depth))
					} else {
						cdrPair := carPair.SyntaxCdr().(*syntax.SyntaxPair)
						expr := cdrPair.SyntaxCar()
						segments = append(segments, segment{typ: segSplice, expr: expr})
					}
					goto next
				}
			}
		}

		currentElems = append(currentElems, p.expandQuasiquote(ctx, carSyntax, depth))

	next:
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if ok {
			current = nextPair
		} else {
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
			// Wrap in (list ...)
			listArgs := []syntax.SyntaxValue{syntax.NewSyntaxSymbol("list", srcCtx)}
			listArgs = append(listArgs, seg.elems...)
			appendArgs = append(appendArgs, p.buildQuasiquoteSyntaxList(srcCtx, listArgs...))
		case segSplice:
			appendArgs = append(appendArgs, seg.expr)
		}
	}

	return p.buildQuasiquoteSyntaxList(srcCtx, appendArgs...)
}

// quasiquoteNeedsRuntime checks if a syntax value contains unquotes that would
// be evaluated at the given depth. This is used to determine whether we can
// emit the quasiquoted form as a compile-time literal or need runtime list construction.
//
// Returns true if the form contains any unquote/unquote-splicing that reaches depth 1.
// For nested forms, it recursively adjusts the depth:
//   - unquote/unquote-splicing at depth 1 → needs runtime (returns true)
//   - unquote/unquote-splicing at depth > 1 → check argument at depth-1
//   - quasiquote → check body at depth+1
func (p *CompileTimeContinuation) quasiquoteNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(v) {
			return false
		}
		// Check if this is (unquote ...) or (unquote-splicing ...) at depth 1
		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case "unquote", "unquote-splicing":
				if depth == 1 {
					return true
				}
				// Nested unquote at depth > 1 - check if the argument needs runtime
				// For ,,x at depth 2: the inner ,x is at depth 1 and needs eval
				if v.Length() == 2 {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					argSyntax := arg
					return p.quasiquoteNeedsRuntime(argSyntax, depth-1)
				}
				return false
			case "quasiquote":
				// Nested quasiquote increases depth
				return p.quasiquoteNeedsRuntimeList(v, depth+1)
			}
		}
		// Check elements
		return p.quasiquoteNeedsRuntimeList(v, depth)

	case *syntax.SyntaxVector:
		// Check vector elements
		for _, elem := range v.Values {
			if p.quasiquoteNeedsRuntime(elem, depth) {
				return true
			}
		}
		return false

	default:
		return false
	}
}

func (p *CompileTimeContinuation) quasiquoteNeedsRuntimeList(pair *syntax.SyntaxPair, depth int) bool {
	current := pair
	for !syntax.IsSyntaxEmptyList(current) {
		car := current.SyntaxCar()
		carSyntax := car

		// Detect dotted-pair unquote: `(a . ,x)` parses as `(a unquote x)`.
		// The bare symbol `unquote` followed by exactly one element signals
		// a runtime-evaluated tail per R7RS §4.2.8.
		carSymName, ok := p.getSymbolName(carSyntax)
		if ok && carSymName == "unquote" && depth == 1 {
			cdr := current.SyntaxCdr()
			cdrPair, ok := cdr.(*syntax.SyntaxPair)
			if ok && cdrPair.Length() == 1 {
				return true
			}
		}

		if p.quasiquoteNeedsRuntime(carSyntax, depth) {
			return true
		}
		cdr := current.SyntaxCdr()
		if syntax.IsSyntaxEmptyList(cdr) {
			break
		}
		nextPair, ok := cdr.(*syntax.SyntaxPair)
		if !ok {
			break
		}
		current = nextPair
	}
	return false
}

// getSymbolName returns the symbol name if the value is a symbol
func (p *CompileTimeContinuation) getSymbolName(v syntax.SyntaxValue) (string, bool) {
	s, ok := v.(*syntax.SyntaxSymbol)
	if ok {
		return s.Sym.Key, true
	}
	return "", false
}

// CompileUnquote errors - unquote outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquote(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote: not in quasiquote context")
}

// CompileUnquoteSplicing errors - unquote-splicing outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquoteSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote-splicing: not in quasiquote context")
}
