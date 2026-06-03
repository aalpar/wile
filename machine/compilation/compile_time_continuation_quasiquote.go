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

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/internal/syntax"
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
		// Validate quoted literal for circular datum labels
		val, err := p.validateQuotedLiteral(datum.UnwrapAll())
		if err != nil {
			return err
		}
		li := p.template.MaybeAppendLiteral(val)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Transform to equivalent Scheme code and compile
	expanded := p.expandQuasiquote(ctctx.ctx, datum, depth)
	return p.CompileExpression(ctctx, expanded)
}

// expandQuasiquote transforms quasiquoted syntax into equivalent Scheme code.
// It handles vectors directly and delegates all other cases to expandQuasi.
func (p *CompileTimeContinuation) expandQuasiquote(ctx context.Context, stx syntax.SyntaxValue, depth int) syntax.SyntaxValue {
	v, ok := stx.(*syntax.SyntaxVector)
	if ok {
		return p.expandQuasiquoteVector(ctx, v, depth, quasiquoteKW)
	}
	return p.expandQuasi(ctx, stx, depth, quasiquoteKW)
}

// expandQuasiquoteVector handles vector quasiquote expansion.
// Vectors expand to (list->vector (list ...)) or (list->vector (append ...))
// depending on whether unquote-splicing is present.
func (p *CompileTimeContinuation) expandQuasiquoteVector(ctx context.Context, v *syntax.SyntaxVector, depth int, kw quasiKeywords) syntax.SyntaxValue {
	srcCtx := v.SourceContext()

	// Check if any element is unquote-splicing/unsyntax-splicing at depth 1
	hasSplice := false
	for _, elem := range v.Values {
		elemPair, ok := elem.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(elemPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				hasSplice = true
				break
			}
		}
	}

	if !hasSplice {
		// Simple case: (list->vector (list elem1 elem2 ...))
		var elems []syntax.SyntaxValue
		elems = append(elems, syntax.NewSyntaxSymbol("list", srcCtx))
		for _, elem := range v.Values {
			elems = append(elems, p.expandQuasi(ctx, elem, depth, kw))
		}
		listExpr := p.buildQuasiSyntaxList(srcCtx, elems...)
		return p.buildQuasiSyntaxList(srcCtx,
			syntax.NewSyntaxSymbol("list->vector", srcCtx),
			listExpr,
		)
	}

	// Has splicing: (list->vector (append seg1 seg2 ...))
	var segments []quasiSegment
	var currentElems []syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			segments = append(segments, quasiSegment{kind: quasiSegNormal, elems: currentElems})
			currentElems = nil
		}
	}

	for _, elem := range v.Values {
		elemPair, ok := elem.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(elemPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				flushNormal()
				if elemPair.Length() == 2 {
					cdrPair := elemPair.SyntaxCdr().(*syntax.SyntaxPair)
					expr := cdrPair.SyntaxCar()
					segments = append(segments, quasiSegment{kind: quasiSegSplice, expr: expr})
				} else {
					// Malformed - treat as normal
					currentElems = append(currentElems, p.expandQuasi(ctx, elem, depth, kw))
				}
				continue
			}
		}
		currentElems = append(currentElems, p.expandQuasi(ctx, elem, depth, kw))
	}
	flushNormal()

	appendExpr := p.buildQuasiSyntaxList(srcCtx, p.segmentsToAppendArgs(srcCtx, segments)...)
	return p.buildQuasiSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("list->vector", srcCtx),
		appendExpr,
	)
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

// CompileUnquote errors - unquote outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquote(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote: not in quasiquote context"))
}

// CompileUnquoteSplicing errors - unquote-splicing outside of quasiquote
func (p *CompileTimeContinuation) CompileUnquoteSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unquote-splicing: not in quasiquote context"))
}
