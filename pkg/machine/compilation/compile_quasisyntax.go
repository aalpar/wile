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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileQuasisyntax compiles the (quasisyntax template) form.
//
// quasisyntax is like quasiquote but for syntax objects. It supports:
//   - (unsyntax expr) - evaluate expr and insert the result at depth 1
//   - (unsyntax-splicing expr) - evaluate and splice list at depth 1
//   - nested quasisyntax increases depth
//
// Like quasiquote, unsyntax only evaluates when depth reaches 0.
// The result is a syntax object, not a raw datum.
func (p *CompileTimeContinuation) CompileQuasisyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	// expr is the CDR of the form (keyword stripped by syntaxCompiler in
	// register.go). So expr = (template) — exactly one element.
	template, err := formSingleArg(expr, "quasisyntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Compile the quasisyntax template at depth 1
	return p.compileQuasisyntaxTemplate(ctctx, template, 1)
}

// compileQuasisyntaxTemplate compiles a quasisyntax template at the given depth.
// At depth 1, unsyntax expressions are evaluated. At depth > 1, they become literals.
func (p *CompileTimeContinuation) compileQuasisyntaxTemplate(ctctx CompileTimeCallContext, stx syntax.SyntaxValue, depth int) error {
	srcCtx := stx.SourceContext()

	// Check if template needs runtime evaluation
	if !p.quasisyntaxNeedsRuntime(stx, depth) {
		// No unsyntax at current depth - just load as literal syntax
		litIdx := p.template.MaybeAppendLiteral(stx)
		p.AppendOperations(machine.NewOperationLoadLiteralByLiteralIndexImmediate(litIdx))
		return nil
	}

	// Transform to equivalent code and compile
	// The expansion produces regular Scheme values (list, etc), so we wrap with datum->syntax
	// to convert the result back to syntax objects.
	expanded, err := p.expandQuasisyntax(ctctx.ctx, stx, depth)
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Wrap: (datum->syntax #f expanded)
	wrapped := p.buildQuasiSyntaxList(srcCtx,
		syntax.NewSyntaxSymbol("datum->syntax", srcCtx),
		syntax.NewSyntaxObject(values.FalseValue, srcCtx),
		expanded,
	)
	return p.CompileExpression(ctctx, wrapped)
}

// quasisyntaxNeedsRuntime checks if a quasisyntax template contains unsyntax at
// the given depth. It creates a fresh recursion guard; the actual walk lives in
// quasisyntaxNeedsRuntimeGuarded.
func (p *CompileTimeContinuation) quasisyntaxNeedsRuntime(stx syntax.SyntaxValue, depth int) bool {
	return p.quasisyntaxNeedsRuntimeGuarded(stx, depth, p.newQuasiDepthGuard())
}

func (p *CompileTimeContinuation) quasisyntaxNeedsRuntimeGuarded(stx syntax.SyntaxValue, depth int, g *expandDepthGuard) bool {
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

		// Check for unsyntax/unsyntax-splicing/quasisyntax keywords
		carSymName, ok := p.getSymbolName(v.SyntaxCar())
		if ok {
			switch carSymName {
			case "unsyntax", "unsyntax-splicing":
				if depth == 1 {
					return true
				}
				// At depth > 1, check the argument at depth-1
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					arg := cdr.SyntaxCar()
					return p.quasisyntaxNeedsRuntimeGuarded(arg, depth-1, g)
				}
				return false
			case "quasisyntax":
				// Nested quasisyntax at depth 1 always needs runtime construction
				if depth == 1 {
					return true
				}
				// At depth > 1, check body at depth+1
				if hasSyntaxArity(v, 2) {
					cdr := v.SyntaxCdr().(*syntax.SyntaxPair)
					body := cdr.SyntaxCar()
					return p.quasisyntaxNeedsRuntimeGuarded(body, depth+1, g)
				}
				return false
			}
		}

		// Check list elements
		var end syntax.SpineEnd
		for cell, e := range syntax.Spine(v) {
			end = e
			if p.quasisyntaxNeedsRuntimeGuarded(cell.SyntaxCar(), depth, g) {
				return true
			}
		}
		// An improper tail that is not a pair — a vector, most usefully. It is
		// still part of the template and can carry an unsyntax of its own, so
		// ASK it rather than assume it is inert. Treating it as a terminator
		// (which is what the hand-rolled walk did) reported "no runtime needed"
		// for #`(1 . #(#,x)) and emitted the whole form as a literal, so it
		// printed (1 . #((unsyntax x))). Improper() is false for a proper list
		// and for a walk that never reached a terminator, so no separate "did
		// the loop finish?" guard is needed.
		if end.Improper() {
			return p.quasisyntaxNeedsRuntimeGuarded(end.Tail, depth, g)
		}
		return false

	case *syntax.SyntaxVector:
		// Without this arm a vector fell to `default: return false` and
		// #`#(1 #,x) was emitted whole as a literal, printing #(1 (unsyntax x)).
		// expandQuasi has always dispatched vectors correctly; only the
		// predicate deciding whether to CALL it was blind to them.
		for _, elem := range v.Values {
			if p.quasisyntaxNeedsRuntimeGuarded(elem, depth, g) {
				return true
			}
		}
		return false

	default:
		return false
	}
}

// expandQuasisyntax transforms quasisyntax template into equivalent Scheme code.
// Delegates to the unified expandQuasi with quasisyntax keywords.
func (p *CompileTimeContinuation) expandQuasisyntax(ctx context.Context, stx syntax.SyntaxValue, depth int) (syntax.SyntaxValue, error) {
	return p.expandQuasi(ctx, stx, depth, quasisyntaxKW, p.newQuasiDepthGuard())
}

// CompileUnsyntax errors - unsyntax outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntax(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unsyntax: not in quasisyntax context"))
}

// CompileUnsyntaxSplicing errors - unsyntax-splicing outside of quasisyntax
func (p *CompileTimeContinuation) CompileUnsyntaxSplicing(_ CompileTimeCallContext, _ syntax.SyntaxValue) error {
	return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "unsyntax-splicing: not in quasisyntax context"))
}
