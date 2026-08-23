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

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// hasSyntaxArity reports whether p is a proper list of exactly n elements. It
// never panics, where (*syntax.SyntaxPair).Length panics on an improper list. The
// quasiquote and quasisyntax walks meet malformed templates like `(a unquote x . y);
// a malformed template must reach a clean compile path, not an internal-error crash.
// After hasSyntaxArity(p, k) is true, the first k SyntaxCdr().(*SyntaxPair) hops on
// p are guaranteed safe.
func hasSyntaxArity(p *syntax.SyntaxPair, n int) bool {
	if n <= 0 {
		return false
	}
	var cur syntax.SyntaxValue = p
	for range n {
		pair, ok := cur.(*syntax.SyntaxPair)
		if !ok {
			return false
		}
		cur = pair.SyntaxCdr()
	}
	return syntax.IsSyntaxEmptyList(cur)
}

// compileQuasiquoteDatum compiles a quasiquoted datum at the given nesting depth.
//
// depth=1 means we're inside one level of quasiquote (the common case).
// depth=2 means nested quasiquote `(a `(b ,x)), etc.
// depth=0 would mean we should evaluate (but we start at 1, so this is the trigger).
func (p *CompileTimeContinuation) compileQuasiquoteDatum(ctctx CompileTimeCallContext, datum syntax.SyntaxValue, depth int) error {
	// A single guard bounds both the needs-runtime analysis and the expansion;
	// each is reset (enter/leave is symmetric) before the next phase runs.
	g := p.newQuasiDepthGuard()

	// Optimization: if no runtime evaluation needed, emit as literal
	if !p.quasiquoteNeedsRuntime(datum, depth, g) {
		// Validate quoted literal for circular datum labels
		val, err := p.validateQuotedLiteral(datum.UnwrapAll())
		if err != nil {
			return err
		}
		// No unquote anywhere: the quasiquote is a constant and gets the same
		// R7RS §4.1.2 treatment (and the same pre-append mark) as a quote.
		li := p.appendConstantLiteral(val)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(li))
		return nil
	}

	// Transform to equivalent Scheme code and compile
	expanded, err := p.expandQuasiquote(ctctx.ctx, datum, depth, g)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	return p.CompileExpression(ctctx, expanded)
}

// expandQuasiquote transforms quasiquoted syntax into equivalent Scheme code.
// It handles vectors directly and delegates all other cases to expandQuasi.
func (p *CompileTimeContinuation) expandQuasiquote(ctx context.Context, stx syntax.SyntaxValue, depth int, g *expandDepthGuard) (syntax.SyntaxValue, error) {
	v, ok := stx.(*syntax.SyntaxVector)
	if ok {
		return p.expandQuasiquoteVector(ctx, v, depth, quasiquoteKW, g)
	}
	return p.expandQuasi(ctx, stx, depth, quasiquoteKW, g)
}

// expandQuasiquoteVector handles vector quasiquote expansion.
// Vectors expand to (list->vector (list ...)) or (list->vector (append ...))
// depending on whether unquote-splicing is present.
func (p *CompileTimeContinuation) expandQuasiquoteVector(ctx context.Context, v *syntax.SyntaxVector, depth int, kw quasiKeywords, g *expandDepthGuard) (syntax.SyntaxValue, error) {
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
		for _, elem := range v.Values {
			expandedElem, err := p.expandQuasi(ctx, elem, depth, kw, g)
			if err != nil {
				return nil, err
			}
			elems = append(elems, expandedElem)
		}
		return p.quasiForm(srcCtx, "list->vector", p.quasiForm(srcCtx, "list", elems...)), nil
	}

	// Has splicing: (list->vector (append seg1 seg2 ...))
	var appendArgs []syntax.SyntaxValue
	var currentElems []syntax.SyntaxValue

	flushNormal := func() {
		if len(currentElems) > 0 {
			appendArgs = append(appendArgs, p.quasiForm(srcCtx, "list", currentElems...))
			currentElems = nil
		}
	}

	for _, elem := range v.Values {
		elemPair, ok := elem.(*syntax.SyntaxPair)
		if ok {
			carSymName, ok := p.getSymbolName(elemPair.SyntaxCar())
			if ok && carSymName == kw.splicing && depth == 1 {
				flushNormal()
				if hasSyntaxArity(elemPair, 2) {
					cdrPair := elemPair.SyntaxCdr().(*syntax.SyntaxPair)
					appendArgs = append(appendArgs, cdrPair.SyntaxCar())
				} else {
					// Malformed - treat as normal
					expandedElem, err := p.expandQuasi(ctx, elem, depth, kw, g)
					if err != nil {
						return nil, err
					}
					currentElems = append(currentElems, expandedElem)
				}
				continue
			}
		}
		expandedElem, err := p.expandQuasi(ctx, elem, depth, kw, g)
		if err != nil {
			return nil, err
		}
		currentElems = append(currentElems, expandedElem)
	}
	flushNormal()

	return p.quasiForm(srcCtx, "list->vector", p.quasiForm(srcCtx, "append", appendArgs...)), nil
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
func (p *CompileTimeContinuation) quasiquoteNeedsRuntime(stx syntax.SyntaxValue, depth int, g *expandDepthGuard) bool {
	// Bound this analysis the same way the expander is bounded. When the input
	// is too deep to walk safely, report "needs runtime" so the depth-guarded
	// expander runs and converts the over-depth into a catchable error rather
	// than letting this predicate overflow the Go stack first.
	if g.enter() {
		g.leave()
		return true
	}
	defer g.leave()

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		// No empty-list guard: (*SyntaxPair).IsEmptyList is an unconditional
		// false, so this arm never holds one.
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
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					return p.quasiquoteNeedsRuntime(arg, depth-1, g)
				}
				return false
			case "quasiquote":
				// Nested quasiquote increases depth
				return p.quasiquoteNeedsRuntimeList(v, depth+1, g)
			}
		}
		// Check elements
		return p.quasiquoteNeedsRuntimeList(v, depth, g)

	case *syntax.SyntaxVector:
		// Check vector elements
		for _, elem := range v.Values {
			if p.quasiquoteNeedsRuntime(elem, depth, g) {
				return true
			}
		}
		return false

	default:
		return false
	}
}

// quasiquoteNeedsRuntimeList walks a template list, reporting whether anything
// in it has to be built at run time rather than emitted as a literal.
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
func (p *CompileTimeContinuation) quasiquoteNeedsRuntimeList(pair *syntax.SyntaxPair, depth int, g *expandDepthGuard) bool {
	var end syntax.SpineEnd
	for cell, e := range syntax.Spine(pair) {
		end = e
		car := cell.SyntaxCar()

		// Detect dotted-pair unquote: `(a . ,x)` parses as `(a unquote x)`.
		// The bare symbol `unquote` followed by exactly one element signals
		// a runtime-evaluated tail per R7RS §4.2.8.
		carSymName, ok := p.getSymbolName(car)
		if ok && carSymName == "unquote" && depth == 1 {
			cdrPair, ok := cell.SyntaxCdr().(*syntax.SyntaxPair)
			if ok && hasSyntaxArity(cdrPair, 1) {
				return true
			}
		}

		if p.quasiquoteNeedsRuntime(car, depth, g) {
			return true
		}
	}
	// Improper tail that is not a pair — a vector, most usefully. It is still
	// part of the template and can carry an unquote of its own, so ASK it rather
	// than assume it is inert. Treating it as a terminator (which is what the
	// hand-rolled walk did) reported "no runtime needed" for `(1 . #(,x)) and
	// emitted the whole form as a literal, so it printed (1 . #((unquote x))).
	// Improper() is false for a proper list and for a walk that never reached a
	// terminator, so no separate "did the loop finish?" guard is needed.
	if end.Improper() {
		return p.quasiquoteNeedsRuntime(end.Tail, depth, g)
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
